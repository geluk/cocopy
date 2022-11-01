use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt::{self, Debug, Display, Formatter},
    slice::{Iter, IterMut},
};

use itertools::Itertools;

use crate::listing::Position;

#[derive(Debug)]
/// A combination of a name and one or more locations to which the name is
/// allocated.
pub struct NameAllocation<R> {
    lifetime: Lifetime,
    reg_slices: Vec<RegAllocation<R>>,
    stack_slices: Vec<StackAllocation>,
}
impl<R: Copy + Eq + Debug + Display> NameAllocation<R> {
    pub fn from_reg(reg: RegAllocation<R>) -> NameAllocation<R> {
        Self {
            lifetime: reg.lifetime,
            reg_slices: vec![reg],
            stack_slices: vec![],
        }
    }

    pub fn from_stack(stack: StackAllocation) -> NameAllocation<R> {
        Self {
            lifetime: stack.lifetime,
            reg_slices: vec![],
            stack_slices: vec![stack],
        }
    }

    pub fn lifetime(&self) -> Lifetime {
        self.lifetime
    }

    /// Returns `true` if this allocation has one or more slices allocated on the stack.
    pub fn has_stack_allocation(&self) -> bool {
        !self.stack_slices.is_empty()
    }

    /// Returns the slice at the given position. Could be either a stack slice or a register slice.
    /// Panics if the position is not contained within the lifetime of the current allocation.
    pub fn slice_at(&self, position: Position) -> Destination<R> {
        let reg = self
            .reg_slices
            .iter()
            .find(|l| l.lifetime.contains(position))
            .map(Destination::Reg);

        let stack = self
            .stack_slices
            .iter()
            .find(|l| l.lifetime.contains(position))
            .copied()
            .map(Destination::Stack);

        reg.or(stack).unwrap_or_else(|| {
            panic!(
                "Attempted to look up invalid position {} in allocation:\n{:#?}",
                position, self,
            )
        })
    }

    // TODO: remove and update caller to use `slice_at`.
    pub fn reg_slice_at(&self, position: Position) -> Option<&RegAllocation<R>> {
        self.reg_slices
            .iter()
            .find(|s| s.lifetime.contains(position))
    }

    /// Returns a mutable reference to the register slice at the given position. Panics if no such
    /// slice exists.
    pub fn reg_slice_at_mut(&mut self, position: Position) -> &mut RegAllocation<R> {
        self.reg_slices
            .iter_mut()
            .find(|s| s.lifetime.contains(position))
            .expect("Attempted to look up invalid position")
    }

    /// Takes the register slice at the given position out of the list of slices, so it can be
    /// easily mutated. Care must be taken to ensure the current allocation is left in a valid
    /// state when finished, which is why this method is not public.
    fn take_reg_slice_at(&mut self, position: Position) -> RegAllocation<R> {
        let index = self
            .reg_slices
            .iter()
            .position(|s| s.lifetime.contains(position))
            .expect("Attempted to look up invalid position");

        self.reg_slices.remove(index)
    }

    /// Iterate over all register slices.
    pub fn iter_reg_slices(&self) -> Iter<RegAllocation<R>> {
        self.reg_slices.iter()
    }

    /// Iterate over all stack slices.
    pub fn iter_stack_slices(&self) -> Iter<StackAllocation> {
        self.stack_slices.iter()
    }

    /// Mutable iteration over all stack slices.
    pub fn iter_stack_slices_mut(&mut self) -> IterMut<StackAllocation> {
        self.stack_slices.iter_mut()
    }

    pub fn reg_slices_within(&self, lifetime: Lifetime) -> Vec<&RegAllocation<R>> {
        self.reg_slices
            .iter()
            .filter(|s| s.lifetime.overlaps(lifetime))
            .collect()
    }

    /// Returns `true` if the current allocation has one or more slices assigned to the target register
    /// within the given lifetime.
    pub fn conflicts_on_lifetime(&self, lifetime: Lifetime, target_reg: R) -> bool {
        self.reg_slices
            .iter()
            .any(|s| s.register == target_reg && s.lifetime.overlaps(lifetime))
    }

    /// Returns `true` if the current allocation has a slice assigned to the target register at the
    /// given position.
    pub(crate) fn conflicts_on_position(&self, position: Position, target_reg: R) -> bool {
        self.reg_slices
            .iter()
            .any(|s| s.register == target_reg && s.lifetime.contains(position))
    }

    /// Tries to kick the current allocation out of the given register within the given lifetime,
    /// by moving all overlapping slices out of the register. Any locked slices encountered will
    /// be left in place. Returns `true` only if all overlapping slices were moved successfully.
    pub fn try_kick_from(&mut self, target_reg: R, lifetime: Lifetime, free_register: R) -> bool {
        let overlapping_slices: Vec<_> = self
            .reg_slices
            .iter_mut()
            .filter(|s| s.register == target_reg && s.lifetime.overlaps(lifetime))
            .collect();

        let mut any_locked = false;
        for slice in overlapping_slices {
            if slice.has_lock() {
                any_locked = true;
            } else {
                slice.move_to(free_register);
            }
        }

        any_locked
    }

    /// Lock the allocation to the given register at the given position. If the slice enveloping
    /// that position is not locked to its current register, it is moved directly. Otherwise, it
    /// will be broken up into two or three slices, depending on how many locks it has.
    pub fn add_lock(&mut self, target_reg: R, lock_point: Position, avoid_points: Vec<Position>) {
        let subject = self.reg_slice_at_mut(lock_point);

        // The slice is already in the correct register! Easy.
        if subject.register() == target_reg {
            for point in avoid_points {
                assert!(
                    !subject.lifetime().contains(point),
                    "Slice was already incorrectly allocated to a point that should be avoided."
                );
            }
            subject.add_lock(lock_point);
            return;
        }

        let (mut locked_before, mut locked_at, mut locked_after) = subject.locks_around(lock_point);

        locked_before = locked_before || avoid_points.iter().any(|&p| p < lock_point);
        locked_at = locked_at || avoid_points.iter().any(|&p| p == lock_point);
        locked_after = locked_after || avoid_points.iter().any(|&p| p > lock_point);

        assert!(
            !locked_at,
            "Slice {subject} is already locked to {} at the same position ({lock_point})",
            subject.register()
        );

        let mut subject = self.take_reg_slice_at(lock_point);
        match (locked_before, locked_after) {
            (false, false) => {
                // The subject slice is not locked in any way, so we can just move it.
                subject.move_and_lock_to(target_reg, lock_point);
                self.reg_slices.push(subject);
            }
            (false, true) => {
                // The subject slice has a lock after the lock we're introducing, so we split it
                // at lock_point + 1 and move the first slice.
                let (mut at, after) = subject.split_at(lock_point + 1);
                at.move_and_lock_to(target_reg, lock_point);

                self.reg_slices.push(at);
                self.reg_slices.push(after);
            }
            (true, false) => {
                // The subject slice has a lock before the lock we're introducing, so we split it
                // at lock_point and move the second slice.
                let (before, mut at) = subject.split_at(lock_point);
                at.move_and_lock_to(target_reg, lock_point);

                self.reg_slices.push(before);
                self.reg_slices.push(at);
            }
            (true, true) => {
                // The subject has locks before and after the lock we're introducing, so we chop it
                // up as closely around the lock point as we can.
                let (before, rest) = subject.split_at(lock_point);
                let (mut middle, after) = rest.split_at(lock_point + 1);

                middle.move_and_lock_to(target_reg, lock_point);

                self.reg_slices.push(before);
                self.reg_slices.push(middle);
                self.reg_slices.push(after);
            }
        }
    }

    pub fn get_moves(&self) -> Vec<Move<R>> {
        assert!(
            self.stack_slices.is_empty(),
            "TODO: Implement moves to stack"
        );
        let mut slices: Vec<_> = self.reg_slices.iter().collect();
        slices.sort_by_key(|a| a.lifetime());

        let mut moves = vec![];
        for (from, to) in slices.into_iter().tuple_windows() {
            moves.push(Move {
                from: from.register(),
                to: to.register(),
                position: to.lifetime().start(),
            })
        }
        moves
    }

    /// Find the slice that envelops the given position, and cut it up in such a way that as much of
    /// it is moved out of its current position as possible while honouring existing locks.
    /// The freed slice will be moved to the given register.
    pub fn cut_and_move_out(&mut self, position: Position, free_register: R) -> Lifetime {
        let slice = self.take_reg_slice_at(position);

        let (before, mut middle, after) = slice.cut_around(position);

        middle.move_to(free_register);

        let freed_lifetime = middle.lifetime();

        self.reg_slices.extend(before);
        self.reg_slices.push(middle);
        self.reg_slices.extend(after);

        freed_lifetime
    }

    /// Cuts a 1-sized slice out of the slice that envelops the given position, and locks that new
    /// slice to the given register.
    pub fn cut_and_lock_to(&mut self, position: Position, target_reg: R) {
        let slice = self.take_reg_slice_at(position);

        let (before, middle) = slice.split_at(position);
        let (mut middle, after) = middle.split_at(position + 1);

        middle.move_and_lock_to(target_reg, position);

        self.reg_slices.push(before);
        self.reg_slices.push(middle);
        self.reg_slices.push(after);
    }
}

/// A destination in which the value of a variable will be held.
/// Values can be stored in a register or on the stack.
#[derive(Debug)]
pub enum Destination<'a, R: Copy + Eq> {
    Reg(&'a RegAllocation<R>),
    // Stack allocations are `Copy`, so there is no need to pass references.
    Stack(StackAllocation),
}

/// An allocation assigning a variable to a register during a certain lifetime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegAllocation<R> {
    register: R,
    lifetime: Lifetime,
    locks: HashSet<Position>,
}
impl<R: Copy + Eq> RegAllocation<R> {
    pub fn new(register: R, lifetime: Lifetime) -> Self {
        Self {
            register,
            locks: HashSet::new(),
            lifetime,
        }
    }

    pub fn register(&self) -> R {
        self.register
    }

    pub fn lifetime(&self) -> Lifetime {
        self.lifetime
    }

    pub fn has_lock(&self) -> bool {
        !self.locks.is_empty()
    }

    pub fn locks_around(&self, position: Position) -> (bool, bool, bool) {
        let mut locked_before = false;
        let mut locked_at = false;
        let mut locked_after = false;
        for &existing_lock in &self.locks {
            match existing_lock.cmp(&position) {
                Ordering::Less => {
                    locked_before = true;
                }
                Ordering::Equal => {
                    locked_at = true;
                }
                Ordering::Greater => {
                    locked_after = true;
                }
            }
        }
        (locked_before, locked_at, locked_after)
    }

    pub fn add_lock(&mut self, lock_point: Position) {
        self.locks.insert(lock_point);
    }

    pub fn move_to(&mut self, target_reg: R) {
        if !self.locks.is_empty() {
            panic!("Can't move this slice! It is already locked to a different register")
        }
        self.register = target_reg;
    }

    pub fn move_and_lock_to(&mut self, target_reg: R, lock_point: Position) {
        self.move_to(target_reg);
        self.add_lock(lock_point);
    }

    /// Consumes this allocation an splits it into two separate allocations.
    /// The first allocation will end at `position`, the second will start there.
    fn split_at(self, position: Position) -> (RegAllocation<R>, RegAllocation<R>) {
        let (first, second) = self.lifetime.split_at(position);
        let mut locks_before = HashSet::new();
        let mut locks_after = HashSet::new();
        for lock in self.locks {
            if lock >= position {
                locks_after.insert(lock);
            } else {
                locks_before.insert(lock);
            }
        }
        (
            Self {
                register: self.register,
                lifetime: first,
                locks: locks_before,
            },
            Self {
                register: self.register,
                lifetime: second,
                locks: locks_after,
            },
        )
    }

    fn cut_around(self, position: Position) -> (Option<Self>, Self, Option<Self>) {
        let head_lock = self.locks.iter().copied().filter(|&p| p < position).max();
        let tail_lock = self.locks.iter().copied().filter(|&p| p > position).min();

        let mut head = None;
        let mut middle = self;
        let mut tail = None;

        if let Some(head_lock) = head_lock {
            let (h, m) = middle.split_at(head_lock + 1);
            (head, middle) = (Some(h), m)
        }
        if let Some(tail_lock) = tail_lock {
            let (m, t) = middle.split_at(tail_lock);
            (middle, tail) = (m, Some(t));
        }

        (head, middle, tail)
    }
}
impl<R: Display + Copy + Eq> Display for RegAllocation<R> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.lifetime(), self.register())
    }
}

/// An allocation assigning a variable to a point on the stack during a certain
/// lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackAllocation {
    offset: BaseOffset,
    lifetime: Lifetime,
}
impl StackAllocation {
    pub fn new(lifetime: Lifetime) -> Self {
        Self {
            offset: Default::default(),
            lifetime,
        }
    }

    pub fn offset(&self) -> BaseOffset {
        self.offset
    }

    pub fn lifetime(&self) -> Lifetime {
        self.lifetime
    }

    pub fn set_offset(&mut self, offset: BaseOffset) {
        self.offset = offset;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move<R: Copy + Eq> {
    position: Position,
    from: R,
    to: R,
}
impl<R: Copy + Eq> Move<R> {
    pub fn position(&self) -> Position {
        self.position
    }
    pub fn from(&self) -> R {
        self.from
    }
    pub fn to(&self) -> R {
        self.to
    }
}

/// A lifetime of an allocation, indicated by a start position (inclusive) and
/// an end position (exclusive) in the source code.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Lifetime {
    start: Position,
    end: Position,
}
impl Lifetime {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    /// Construct a new lifetime from a single position. The lifetime will be
    /// valid at the given position only.
    pub fn from_position(position: Position) -> Self {
        Self::new(position, position + 1)
    }

    pub fn start(&self) -> Position {
        self.start
    }

    /// Returns `true` if the supplied lifetime (partially) overlaps the current
    /// lifetime.
    pub fn overlaps(&self, other: Lifetime) -> bool {
        self.end > other.start && self.start < other.end
    }

    /// Returns `true` if the supplied position is contained within the current
    /// lifetime.
    pub fn contains(&self, position: Position) -> bool {
        position >= self.start && position < self.end
    }

    /// Expand the lifetime to produce a new lifetime enveloping the given position.
    pub fn expand(&self, position: Position) -> Lifetime {
        Self {
            start: self.start.min(position),
            end: self.end.max(Position(position.0 + 1)),
        }
    }

    /// Split the lifetime into two separate lifetimes at the given position.
    fn split_at(&self, position: Position) -> (Lifetime, Lifetime) {
        assert!(self.contains(position));
        (
            Self::new(self.start, position),
            Self::new(position, self.end),
        )
    }
}
impl Ord for Lifetime {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start.cmp(&other.start)
    }
}
impl PartialOrd for Lifetime {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.start.cmp(&other.start))
    }
}
impl Display for Lifetime {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[{}:{})", self.start.0, self.end.0)
    }
}
impl Debug for Lifetime {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("Lifetime({}:{})", self.start, self.end))
    }
}

/// A point on the stack, offset from the base of the current stack frame
/// by the given number of bytes.
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BaseOffset(usize);
impl BaseOffset {
    /// Construct a new [`BaseOffset`], offset from the base of the stack frame
    /// by a given amount of words. A word is considered to be 8 bytes wide
    /// in the current implementation.
    pub fn words(words: usize) -> Self {
        Self(words * 8)
    }
}
impl Debug for BaseOffset {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} bytes", self.0)
    }
}
impl Display for BaseOffset {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} bytes", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! lifetime {
        ($start:expr, $end:expr) => {
            Lifetime::new(Position($start), Position($end))
        };
    }

    macro_rules! alloc {
        ($reg:expr, $start:expr, $end:expr) => {
            RegAllocation::new($reg, lifetime!($start, $end))
        };
    }

    #[test]
    fn cut_around_without_locks_returns_equivalent_slice() {
        let original = alloc!('a', 0, 10);
        let (start, middle, end) = original.clone().cut_around(Position(4));

        assert!(start.is_none());
        assert_eq!(middle, original);
        assert!(end.is_none());
    }

    #[test]
    fn cut_around_when_locked_before_splits_after_lock() {
        let mut slice = alloc!('a', 0, 10);
        slice.add_lock(Position(2));

        let (start, middle, end) = slice.cut_around(Position(4));

        let mut expected_start = alloc!('a', 0, 3);
        expected_start.add_lock(Position(2));

        assert_eq!(start.unwrap(), expected_start);
        assert_eq!(middle, alloc!('a', 3, 10));
        assert!(end.is_none());
    }

    #[test]
    fn cut_around_when_locked_after_splits_before_lock() {
        let mut slice = alloc!('a', 0, 10);
        slice.add_lock(Position(5));

        let (start, middle, end) = slice.cut_around(Position(4));

        let mut expected_end = alloc!('a', 5, 10);
        expected_end.add_lock(Position(5));

        assert!(start.is_none());
        assert_eq!(middle, alloc!('a', 0, 5));
        assert_eq!(end.unwrap(), expected_end);
    }

    #[test]
    fn cut_around_when_both_locked_splits_both() {
        let mut slice = alloc!('a', 0, 10);
        slice.add_lock(Position(0));
        slice.add_lock(Position(8));

        let (start, middle, end) = slice.cut_around(Position(4));

        let mut expected_start = alloc!('a', 0, 1);
        expected_start.add_lock(Position(0));
        let mut expected_end = alloc!('a', 8, 10);
        expected_end.add_lock(Position(8));

        assert_eq!(start.unwrap(), expected_start);
        assert_eq!(middle, alloc!('a', 1, 8));
        assert_eq!(end.unwrap(), expected_end);
    }

    #[test]
    fn cut_around_with_many_locks_cuts_between_closest() {
        let mut slice = alloc!('a', 0, 10);
        slice.add_lock(Position(0));
        slice.add_lock(Position(1));
        slice.add_lock(Position(6));
        slice.add_lock(Position(8));

        let (start, middle, end) = slice.cut_around(Position(4));

        let mut expected_start = alloc!('a', 0, 2);
        expected_start.add_lock(Position(0));
        expected_start.add_lock(Position(1));
        let mut expected_end = alloc!('a', 6, 10);
        expected_end.add_lock(Position(6));
        expected_end.add_lock(Position(8));

        assert_eq!(start.unwrap(), expected_start);
        assert_eq!(middle, alloc!('a', 2, 6));
        assert_eq!(end.unwrap(), expected_end);
    }

    #[test]
    pub fn lifetime_contained_in_other() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(2), Position(3));

        assert!(one.overlaps(two));
        assert!(two.overlaps(one));
    }

    #[test]
    pub fn lifetime_ends_overlap() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(9), Position(15));

        assert!(one.overlaps(two));
        assert!(two.overlaps(one));
    }

    #[test]
    pub fn lifetime_no_overlap() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(11), Position(15));

        assert!(!one.overlaps(two));
        assert!(!two.overlaps(one));
    }

    #[test]
    pub fn lifetime_touches() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(10), Position(15));

        assert!(!one.overlaps(two));
        assert!(!two.overlaps(one));
    }

    #[test]
    pub fn lifetime_equal() {
        let one = Lifetime::new(Position(1), Position(10));
        let two = Lifetime::new(Position(1), Position(10));

        assert!(one.overlaps(two));
        assert!(two.overlaps(one));
    }
}

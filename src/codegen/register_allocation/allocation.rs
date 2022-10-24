use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt::{self, Debug, Display, Formatter},
    slice::{Iter, IterMut},
};

use crate::listing::Position;

#[derive(Debug)]
/// A combination of a name and one or more locations to which the name is
/// allocated.
pub struct NameAllocation<R: Copy + Eq> {
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

    /// Tries to kick the current allocation out of the given register within the given lifetime.
    /// If none of its overlapping slices are locked to the register, they are moved to the
    /// free register, and `true` is returned. Otherwise, returns `false` without taking any action.
    pub fn try_kick_from(&mut self, target_reg: R, lifetime: Lifetime, free_register: R) -> bool {
        let overlapping_slices: Vec<_> = self
            .reg_slices
            .iter_mut()
            .filter(|s| s.register == target_reg && s.lifetime.overlaps(lifetime))
            .collect();

        // At least one overlapping slice is locked to that register, so we refuse to move.
        // Does it make sense to still move the non-locked slices out?
        if overlapping_slices.iter().any(|s| s.has_lock()) {
            return false;
        }

        for slice in overlapping_slices {
            slice.move_to(free_register);
        }
        true
    }

    /// Lock the allocation to the given register at the given position. If the slice enveloping
    /// that position is not locked to its current register, it is moved directly. Otherwise, it
    /// will be broken up into two or three slices, depending on how many locks it has.
    pub fn add_lock(&mut self, target_reg: R, lock_point: Position) {
        let subject = self.reg_slice_at_mut(lock_point);

        // The slice is already in the correct register! Easy.
        if subject.register() == target_reg {
            subject.add_lock(lock_point);
            return;
        }

        let (locked_before, locked_at, locked_after) = subject.locks_around(lock_point);
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
pub struct RegAllocation<R: Copy + Eq> {
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

    #[cfg(test)]
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

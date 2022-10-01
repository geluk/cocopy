use std::{
    fmt::{self, Debug, Display, Formatter},
    slice::IterMut,
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
impl<R: Copy + Eq + Debug> NameAllocation<R> {
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

    pub fn has_stack_allocation(&self) -> bool {
        !self.stack_slices.is_empty()
    }

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
                "Attempted to look up invalid position {} from slice {:?}",
                position, self,
            )
        })
    }

    pub fn reg_slice_at(&self, position: Position) -> Option<&RegAllocation<R>> {
        self.reg_slices
            .iter()
            .find(|s| s.lifetime.contains(position))
    }

    pub fn reg_slice_at_mut(&mut self, position: Position) -> &mut RegAllocation<R> {
        self.reg_slices
            .iter_mut()
            .find(|s| s.lifetime.contains(position))
            .expect("Attempted to look up invalid position")
    }

    pub fn iter_stack_slices_mut(&mut self) -> IterMut<StackAllocation> {
        self.stack_slices.iter_mut()
    }

    pub fn reg_slices_within(&self, lifetime: Lifetime) -> Vec<&RegAllocation<R>> {
        self.reg_slices
            .iter()
            .filter(|s| s.lifetime.overlaps(lifetime))
            .collect()
    }

    pub fn conflicts_on_point(&self, lock_point: Position, target_reg: R) -> bool {
        self.reg_slices
            .iter()
            .any(|s| s.register == target_reg && s.lifetime.contains(lock_point))
    }

    pub fn conflicts_on_lifetime(&self, lifetime: Lifetime, target_reg: R) -> bool {
        self.reg_slices
            .iter()
            .any(|s| s.register == target_reg && s.lifetime.overlaps(lifetime))
    }

    pub fn try_kick_from(&mut self, target_reg: R, lifetime: Lifetime) -> bool {
        // TODO: Does it make sense to keep stack slices and register slices
        // in the same collection? They're never ever going to conflict after all.
        let overlapping_slices: Vec<_> = self
            .reg_slices
            .iter_mut()
            .filter(|s| s.register == target_reg && s.lifetime.overlaps(lifetime))
            .collect();

        if overlapping_slices.iter().any(|s| s.has_lock()) {
            return false;
        }

        for _ in overlapping_slices {
            todo!("Move slice to a new register.")
        }
        true
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
impl<'a, R: Copy + Eq + Display> Destination<'a, R> {
    pub fn describe(&self) -> String {
        match self {
            Destination::Reg(reg) => format!("{}", reg.register()),
            Destination::Stack(stack) => format!("stack offset {:?}", stack.offset()),
        }
    }
}
impl<'a, R: Copy + Eq> Destination<'a, R> {
    pub fn as_stack_mut(&mut self) -> Option<&mut StackAllocation> {
        match self {
            Destination::Reg(_) => None,
            Destination::Stack(stack) => Some(stack),
        }
    }
    pub fn as_stack(&self) -> Option<&StackAllocation> {
        match self {
            Destination::Reg(_) => None,
            Destination::Stack(stack) => Some(stack),
        }
    }
    pub fn as_reg(&self) -> Option<&RegAllocation<R>> {
        match self {
            Destination::Reg(reg) => Some(reg),
            Destination::Stack(_) => None,
        }
    }
    pub fn lifetime(&self) -> Lifetime {
        match self {
            Destination::Reg(reg) => reg.lifetime,
            Destination::Stack(stack) => stack.lifetime,
        }
    }
}

/// An allocation assigning a variable to a register during a certain lifetime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegAllocation<R: Copy + Eq> {
    register: R,
    lifetime: Lifetime,
    locks: Vec<Position>,
}
impl<R: Copy + Eq> RegAllocation<R> {
    pub fn new(register: R, lifetime: Lifetime, locks: Vec<Position>) -> Self {
        Self {
            register,
            locks,
            lifetime,
        }
    }

    pub fn register(&self) -> R {
        self.register
    }

    pub fn lifetime(&self) -> Lifetime {
        self.lifetime
    }

    pub fn locks(&self) -> &Vec<Position> {
        &self.locks
    }

    pub fn move_and_lock_to(&mut self, target_reg: R, lock_point: Position) {
        if !self.locks.is_empty() {
            panic!("Can't move this slice! It is already locked to a different register")
        }

        self.register = target_reg;
        self.locks = vec![lock_point];
    }

    fn has_lock(&self) -> bool {
        !self.locks.is_empty()
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

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
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
}
impl Display for Lifetime {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.start.0, self.end.0)
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

use std::fmt::{self, Debug, Display, Formatter};

use crate::listing::Position;

/// An allocation assigning a variable to a register during a certain lifetime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegAllocation<N: Eq, R: Copy + Eq> {
    name: N,
    register: R,
    lifetime: Lifetime,
    locks: Vec<Position>,
}
impl<N: Eq, R: Copy + Eq> RegAllocation<N, R> {
    pub fn new(name: N, register: R, lifetime: Lifetime, locks: Vec<Position>) -> Self {
        Self {
            name,
            register,
            lifetime,
            locks,
        }
    }
    pub fn register(&self) -> R {
        self.register
    }
    pub fn locks(&self) -> &Vec<Position> {
        &self.locks
    }
}

/// An allocation assigning a variable to a point on the stack during a certain
/// lifetime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackAllocation<N> {
    name: N,
    lifetime: Lifetime,
    offset: BaseOffset,
}
impl<N> StackAllocation<N> {
    pub fn new(name: N, lifetime: Lifetime) -> Self {
        Self {
            name,
            lifetime,
            offset: Default::default(),
        }
    }
    pub fn offset(&self) -> BaseOffset {
        self.offset
    }
    pub fn set_offset(&mut self, offset: BaseOffset) {
        self.offset = offset;
    }
}

/// Functionality common to both register and stack allocations.
pub trait Allocation<N: Eq> {
    fn name(&self) -> &N;
    fn lifetime(&self) -> Lifetime;

    /// Returns `true` if the given name and lifetime exactly match the current
    /// allocation.
    fn matches(&self, name: &N, lifetime: Lifetime) -> bool {
        self.name() == name && self.lifetime() == lifetime
    }

    /// Returns `true` if the given name matches the current allocation and
    /// their lifetimes overlap.
    fn conflicts_with(&self, name: &N, lifetime: Lifetime) -> bool {
        self.name() == name && self.lifetime().overlaps(lifetime)
    }

    /// Returns `true` if the given name matches the current allocation and
    /// the current allocation's lifetime contains the given position.
    fn contains(&self, name: &N, location: Position) -> bool {
        self.name() == name && self.lifetime().contains(location)
    }
}
impl<N: Eq, R: Copy + Eq> Allocation<N> for RegAllocation<N, R> {
    fn name(&self) -> &N {
        &self.name
    }

    fn lifetime(&self) -> Lifetime {
        self.lifetime
    }
}
impl<N: Eq> Allocation<N> for StackAllocation<N> {
    fn name(&self) -> &N {
        &self.name
    }

    fn lifetime(&self) -> Lifetime {
        self.lifetime
    }
}

/// A lifetime of an allocation, indicated by a start position (inclusive) and
/// an end position (exclusive) in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

use crate::il::Name;

/// An allocation assigning a variable to a register during a certain lifetime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegAllocation<R: Copy + Eq> {
    name: Name,
    register: R,
    lifetime: Lifetime,
    locks: Vec<Position>,
}
impl<R: Copy + Eq> RegAllocation<R> {
    pub fn new(name: Name, register: R, lifetime: Lifetime, locks: Vec<Position>) -> Self {
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
pub struct StackAllocation {
    name: Name,
    lifetime: Lifetime,
    offset: BaseOffset,
}
impl StackAllocation {
    pub fn new(name: Name, lifetime: Lifetime) -> Self {
        Self {
            name,
            lifetime,
            offset: Default::default(),
        }
    }
    pub fn set_offset(&mut self, offset: BaseOffset) {
        self.offset = offset;
    }
}

/// Functionality common to both register and stack allocations.
pub trait Allocation {
    fn name(&self) -> &Name;
    fn lifetime(&self) -> Lifetime;

    fn conflicts_with(&self, name: &Name, lifetime: Lifetime) -> bool {
        self.name() == name && self.lifetime().overlaps(lifetime)
    }

    fn contains(&self, name: &Name, location: Position) -> bool {
        self.name() == name && self.lifetime().contains(location)
    }
}
impl<R: Copy + Eq> Allocation for RegAllocation<R> {
    fn name(&self) -> &Name {
        &self.name
    }

    fn lifetime(&self) -> Lifetime {
        self.lifetime
    }
}
impl Allocation for StackAllocation {
    fn name(&self) -> &Name {
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
    first_use: Position,
    last_use: Position,
}
impl Lifetime {
    pub fn new(first_use: Position, last_use: Position) -> Self {
        Self {
            first_use,
            last_use,
        }
    }

    /// Returns `true` if the supplied lifetime (partially) overlaps the current
    /// lifetime.
    pub fn overlaps(&self, other: Lifetime) -> bool {
        self.last_use > other.first_use && self.first_use < other.last_use
    }

    /// Returns `true` if the supplied position is contained within the current
    /// lifetime.
    pub fn contains(&self, position: Position) -> bool {
        position >= self.first_use && position < self.last_use
    }
}

/// A position in a TAC function, indicated by a line number.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position(pub usize);

/// A point on the stack, offset from the base of the current stack frame
/// by the given number of bytes.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BaseOffset(usize);
impl BaseOffset {
    /// Construct a new [`BaseOffset`], offset from the base of the stack frame
    /// by a given amount of words. A word is considered to be 8 bytes wide
    /// in the current implementation.
    pub fn words(words: usize) -> Self {
        Self(words * 8)
    }
}

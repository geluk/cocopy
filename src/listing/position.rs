use std::{
    fmt::{Display, Formatter},
    ops::{Add, Sub},
};

/// A position in a listing, indicated by a line number.
/// Unlike line numbers in most source code editors, positions are zero-indexed,
/// so the first line in a listing has line number 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position(pub usize);

impl Add<usize> for Position {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}
impl Sub<usize> for Position {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        Self(self.0 - rhs)
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

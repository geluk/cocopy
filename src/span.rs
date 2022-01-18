//! Contains the [`Bytes`] and [`Span`] types, which describe source code positions.
use std::{
    fmt::{self, Debug, Display},
    ops::{Add, AddAssign, Sub},
};

// TODO: Once we've fully migrated to `Bytes`, we should remove as many ops as we can.

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Bytes(usize);
impl Bytes {
    pub fn new(pos: usize) -> Self {
        Self(pos)
    }
}
impl Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl From<Bytes> for usize {
    fn from(bytes: Bytes) -> Self {
        bytes.0
    }
}
impl AddAssign<usize> for Bytes {
    fn add_assign(&mut self, rhs: usize) {
        *self = Self(self.0 + rhs)
    }
}
impl AddAssign<Bytes> for Bytes {
    fn add_assign(&mut self, rhs: Bytes) {
        *self = Self(self.0 + rhs.0)
    }
}
impl Add<usize> for Bytes {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}
impl Add<isize> for Bytes {
    type Output = Self;

    fn add(self, rhs: isize) -> Self::Output {
        Self((self.0 as isize + rhs) as usize)
    }
}
impl Add<Bytes> for Bytes {
    type Output = Self;

    fn add(self, rhs: Bytes) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl Sub<Bytes> for Bytes {
    type Output = Self;

    fn sub(self, rhs: Bytes) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Chars(usize);
impl Chars {
    pub fn new(pos: usize) -> Self {
        Self(pos)
    }
}
impl Display for Chars {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl AddAssign<usize> for Chars {
    fn add_assign(&mut self, rhs: usize) {
        *self = Self(self.0 + rhs)
    }
}
impl Add<usize> for Chars {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}
impl Add<isize> for Chars {
    type Output = Self;

    fn add(self, rhs: isize) -> Self::Output {
        Self(self.0 + rhs as usize)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: Bytes,
    end: Bytes,
}
impl Span {
    pub fn new(start: Bytes, end: Bytes) -> Self {
        Self { start, end }
    }

    pub fn zero() -> Self {
        Self::new(Bytes::new(0), Bytes::new(0))
    }

    pub fn length(&self) -> Bytes {
        self.end - self.start
    }

    pub fn start(&self) -> Bytes {
        self.start
    }

    pub fn end(&self) -> Bytes {
        self.end
    }

    pub fn lookup<'t>(&self, target: &'t str) -> &'t str {
        &target[self.start.into()..self.end.into()]
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

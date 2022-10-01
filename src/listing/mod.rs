//! Generic logic for code listings(TAC, assembly, etc).

use std::{
    fmt::{Display, Formatter},
    ops::{Add, Sub},
    slice::Iter,
    vec::IntoIter,
};

pub struct Listing<T> {
    lines: Vec<T>,
}

impl<T> Listing<T> {
    pub fn empty() -> Self {
        Self { lines: vec![] }
    }
    pub fn push(&mut self, line: T) {
        self.lines.push(line);
    }

    pub fn iter_lines(&self) -> LinesIter<T> {
        LinesIter {
            inner: self.lines.iter(),
            position: Position(0),
        }
    }

    pub fn len(&self) -> usize {
        self.lines.len()
    }
}

pub struct LinesIter<'item, T> {
    inner: Iter<'item, T>,
    position: Position,
}

impl<'item, T> Iterator for LinesIter<'item, T> {
    type Item = (Position, &'item T);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|v| {
            let current = self.position;
            self.position = current + 1;
            (current, v)
        })
    }
}

pub struct IntoLines<T> {
    inner: IntoIter<T>,
    position: Position,
}

impl<T> Iterator for IntoLines<T> {
    type Item = (Position, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|v| {
            let current = self.position;
            self.position = current + 1;
            (current, v)
        })
    }
}

impl<T> IntoIterator for Listing<T> {
    type Item = (Position, T);
    type IntoIter = IntoLines<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoLines {
            inner: self.lines.into_iter(),
            position: Position(0),
        }
    }
}

/// A position in a listing, indicated by a line number.
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

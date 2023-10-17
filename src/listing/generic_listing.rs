use std::{
    fmt::{self, Display, Formatter},
    slice::{Iter, IterMut},
    vec::IntoIter,
};

use super::position::*;

#[derive(Debug)]
pub struct Listing<T> {
    lines: Vec<T>,
}

impl<T> Listing<T> {
    pub fn new() -> Self {
        Self { lines: vec![] }
    }

    pub fn push(&mut self, line: T) {
        self.lines.push(line);
    }

    pub fn remove(&mut self, position: Position) {
        self.lines.remove(position.0);
    }

    pub fn iter_lines(&self) -> LinesIter<T> {
        LinesIter {
            inner: self.lines.iter(),
            position: Position(0),
        }
    }

    pub fn iter_lines_mut(&mut self) -> LinesIterMut<T> {
        LinesIterMut {
            inner: self.lines.iter_mut(),
            position: Position(0),
        }
    }

    pub fn iter_instructions(&self) -> Iter<T> {
        self.lines.iter()
    }

    pub fn iter_instructions_mut(&mut self) -> IterMut<T> {
        self.lines.iter_mut()
    }

    pub fn into_lines(self) -> IntoLines<T> {
        IntoLines {
            inner: self.lines.into_iter(),
            position: Position(0),
        }
    }

    pub fn len(&self) -> Position {
        Position(self.lines.len())
    }
}
impl<T: Display> Listing<T> {
    pub fn display_line_nos(&self) -> DisplayLineNos<T> {
        DisplayLineNos { listing: self }
    }
}
impl<T: Display> Display for Listing<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for line in &self.lines {
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

pub struct DisplayLineNos<'a, T> {
    listing: &'a Listing<T>,
}
impl<'a, T: Display> Display for DisplayLineNos<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // If it's stupid but it works...
        let max_width = self.listing.lines.len().to_string().len();
        for (line_no, line) in self.listing.iter_lines() {
            writeln!(f, "{:0width$}| {}", line_no, line, width = max_width)?;
        }

        Ok(())
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

pub struct LinesIterMut<'item, T> {
    inner: IterMut<'item, T>,
    position: Position,
}

impl<'item, T> Iterator for LinesIterMut<'item, T> {
    type Item = (Position, &'item mut T);

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

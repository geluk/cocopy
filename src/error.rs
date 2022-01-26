//! Traits for converting subsystem errors into generic compilation errors, and displaying them.

use std::slice::Iter;

use crate::span::*;

/// A generic compile error, containing a message and position information.
pub struct CompileError {
    description: String,
    range: Span,
}
impl CompileError {
    pub fn new(description: String, range: Span) -> Self {
        Self { description, range }
    }

    pub fn range(&self) -> Span {
        self.range
    }
    pub fn describe(&self) -> String {
        self.description.clone()
    }
    pub fn length(&self) -> Bytes {
        self.range().length()
    }
}

pub struct CompileErrors {
    errors: Vec<CompileError>,
}
impl CompileErrors {
    pub fn iter(&self) -> Iter<CompileError> {
        self.errors.iter()
    }
}
impl<T> From<Vec<T>> for CompileErrors
where
    T: Into<CompileError>,
{
    fn from(errors: Vec<T>) -> Self {
        Self {
            errors: errors.into_iter().map(Into::into).collect(),
        }
    }
}
impl<T> From<T> for CompileErrors
where
    T: Into<CompileError>,
{
    fn from(error: T) -> Self {
        Self {
            errors: vec![error.into()],
        }
    }
}

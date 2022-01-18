//! Traits for converting subsystem errors into generic compilation errors, and displaying them.
use std::slice::Iter;

use crate::span::*;

pub trait PositionalError {
    fn range(&self) -> Span;
    fn describe(&self) -> String;

    fn length(&self) -> Bytes {
        self.range().length()
    }
}

pub struct CompileError {
    range: Span,
    description: String,
}
impl CompileError {
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

impl<T> From<T> for CompileError
where
    T: PositionalError,
{
    fn from(error: T) -> Self {
        Self {
            description: error.describe(),
            range: error.range(),
        }
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
    T: PositionalError,
{
    fn from(errors: Vec<T>) -> Self {
        Self {
            errors: errors.into_iter().map(Into::into).collect(),
        }
    }
}

impl<T> From<T> for CompileErrors
where
    T: PositionalError,
{
    fn from(error: T) -> Self {
        Self {
            errors: vec![error.into()],
        }
    }
}

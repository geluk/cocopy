use thiserror::Error;

use crate::{
    error::PositionalError,
    parser::syntax_tree::{BinOp, TerOp, TypeSpec, UnOp},
    span::Span,
};

pub fn error<S>(kind: TypeErrorKind, span: Span) -> Result<S, TypeError> {
    Err(TypeError::new(kind, span))
}
pub fn singleton_error<S>(kind: TypeErrorKind, span: Span) -> Result<S, Vec<TypeError>> {
    Err(vec![TypeError::new(kind, span)])
}
#[derive(Debug)]
pub struct TypeError {
    kind: TypeErrorKind,
    span: Span,
}
impl TypeError {
    pub fn new(kind: TypeErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
    pub fn kind(&self) -> &TypeErrorKind {
        &self.kind
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeErrorKind {
    #[error("unknown identifier: '{0}'")]
    UnknownIdentifier(String),
    #[error("cannot apply expression of type '{1}' to operand '{0}'")]
    UnOperandType(UnOp, TypeSpec),
    #[error("cannot apply expression of type '{1}' to operand '{0}'")]
    BinOperandType(BinOp, TypeSpec),
    #[error("cannot apply expression of type '{1}' to operand '{0}'")]
    TerOperandType(TerOp, TypeSpec),
    #[error("cannot assign expression of type 'None' to a primitive variable of type '{0}'")]
    AssignNoneToPrimitive(TypeSpec),
    #[error("cannot assign expression of type '{1}' to a variable of type '{0}'")]
    Assign(TypeSpec, TypeSpec),
}

pub trait AddSpan {
    type Annotated;

    fn add_span(self, span: Span) -> Self::Annotated;
}

impl<O> AddSpan for Result<O, TypeErrorKind> {
    type Annotated = Result<O, TypeError>;

    fn add_span(self, span: Span) -> Self::Annotated {
        self.map_err(|kind| TypeError::new(kind, span))
    }
}

impl PositionalError for TypeError {
    fn range(&self) -> Span {
        self.span
    }

    fn describe(&self) -> String {
        format!("{}", self.kind)
    }
}

impl From<TypeError> for Vec<TypeError> {
    fn from(type_error: TypeError) -> Self {
        vec![type_error]
    }
}

/// Marks a type from which multiple errors can be collected.
pub trait CollectErrors<E> {
    /// Collects all erros from `Self` and transforms them into a [`Vec<E>`].
    fn collect_errors(self) -> Vec<E>;
}
impl<R, E> CollectErrors<E> for Result<R, Vec<E>> {
    fn collect_errors(self) -> Vec<E> {
        self.err().into_iter().flatten().collect()
    }
}

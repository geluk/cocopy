use thiserror::Error;

use crate::{
    ast::{
        untyped::{BinOp, UnOp},
        TypeSpec,
    },
    error::CompileError,
    span::Span,
};

/// Construct a new error result.
pub fn error<S>(kind: TypeErrorKind, span: Span) -> Result<S, TypeError> {
    Err(TypeError::new(kind, span))
}

/// An error as produced by the type checker.
#[derive(Debug, Error)]
#[error("{kind}")]
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
impl From<TypeError> for CompileError {
    fn from(type_error: TypeError) -> Self {
        Self::new(type_error.to_string(), type_error.span)
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeErrorKind {
    #[error("unknown identifier: '{0}'")]
    UnknownIdentifier(String),
    #[error("cannot apply expression of type '{1}' to operator '{0}'")]
    UnOperandType(UnOp, TypeSpec),
    #[error("cannot apply expression of type '{1}' to operator '{0}'")]
    BinOperandType(BinOp, TypeSpec),
    #[error("cannot unify true side '{0}' with false side '{1}'")]
    TernaryIfBranchMismatch(TypeSpec, TypeSpec),
    #[error("cannot use expression of type '{0}' as the condition for an if-expression")]
    TernaryIfCondition(TypeSpec),
    #[error("cannot assign expression of type 'None' to a primitive variable of type '{0}'")]
    AssignNoneToPrimitive(TypeSpec),
    #[error("cannot assign expression of type '{1}' to a variable of type '{0}'")]
    Assign(TypeSpec, TypeSpec),
    #[error("type '{0}' is not a function")]
    NotCallable(TypeSpec),
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

impl From<TypeError> for Vec<TypeError> {
    fn from(type_error: TypeError) -> Self {
        vec![type_error]
    }
}

/// Marks a type from which multiple errors can be collected.
pub trait CollectErrors<E> {
    /// Collects all erros from `Self` into a [`Vec<E>`].
    fn collect_errors(self) -> Vec<E>;
}
impl<R, E> CollectErrors<E> for Result<R, Vec<E>> {
    fn collect_errors(self) -> Vec<E> {
        self.err().into_iter().flatten().collect()
    }
}

/// Poor man's `Validation` type.
pub trait ConcatResult<O1, O2, E> {
    /// Concatenate two results. Returns [`Ok`] if both results have succeeded.
    /// Otherwise, combines their errors.
    fn concat_result(self, other: Result<O2, E>) -> Result<(O1, O2), E>;
}

impl<O1, O2, E> ConcatResult<O1, O2, Vec<E>> for Result<O1, Vec<E>> {
    fn concat_result(self, other: Result<O2, Vec<E>>) -> Result<(O1, O2), Vec<E>> {
        match (self, other) {
            (Ok(a), Ok(b)) => Ok((a, b)),
            (Err(mut a), Err(mut b)) => {
                a.append(&mut b);
                Err(a)
            }
            (_, Err(b)) => Err(b),
            (Err(a), _) => Err(a),
        }
    }
}

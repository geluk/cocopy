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

/// Construct a new singleton error result.
pub fn singleton_error<S>(kind: TypeErrorKind, span: Span) -> Result<S, Vec<TypeError>> {
    Err(vec![TypeError::new(kind, span)])
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
    #[error("duplicate declaration of identifier: '{0}'")]
    DuplicateIdentifier(String),
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
    AssignType(TypeSpec, TypeSpec),
    #[error("cannot assign to {0}")]
    AssignExpr(&'static str),
    #[error("type '{0}' is not a function")]
    NotCallable(TypeSpec),
    #[error("function '{0}' accepts {1} parameter(s), but received {2}")]
    ParamCountMismatch(String, usize, usize),
    #[error("cannot apply expression of type '{1}' to a parameter of type '{0}'")]
    ParamTypeMismatch(TypeSpec, TypeSpec),
    #[error("cannot use expression of type '{0}' as the condition for a control statement")]
    ControlCondition(TypeSpec),
    #[error("can only return inside a function")]
    UnexpectedReturn,
    #[error("cannot return a value of type '{0}' from a function without return type")]
    UnexpectedReturnType(TypeSpec),
    #[error("return statement should return a value")]
    ShouldReturnAValue,
    #[error("cannot return a value of type '{1}' in a function that returns type '{0}'")]
    ReturnTypeMismatch(TypeSpec, TypeSpec),
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

/// Allows combining multiple ranges of errors.
pub trait ConcatResult<O1, O2, E> {
    /// Concatenate two results. Returns [`Ok`] if both results have succeeded.
    /// Otherwise, combines their errors.
    fn concat_result(self, other: Result<O2, Vec<E>>) -> Result<(O1, O2), Vec<E>>;

    /// Concatenate two results when the other result type only contains
    /// a single error.
    fn combine_with(self, other: Result<O2, E>) -> Result<(O1, O2), Vec<E>>
    where
        Self: Sized,
    {
        self.concat_result(other.map_err(|e| vec![e]))
    }
}
impl<O1, O2, E> ConcatResult<O1, O2, E> for Result<O1, Vec<E>> {
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
impl<O1, O2, E> ConcatResult<O1, O2, E> for Result<O1, E> {
    fn concat_result(self, other: Result<O2, Vec<E>>) -> Result<(O1, O2), Vec<E>> {
        self.map_err(|e| vec![e]).concat_result(other)
    }
}

pub trait CollectAll<S, E> {
    fn collect_all(self) -> Result<Vec<S>, Vec<E>>;
}
impl<I, S, E> CollectAll<S, E> for I
where
    I: Iterator<Item = Result<S, E>>,
{
    fn collect_all(self) -> Result<Vec<S>, Vec<E>> {
        let (lower_bound, _) = self.size_hint();
        let mut successes = Vec::with_capacity(lower_bound);
        let mut errors = vec![];

        for item in self {
            match item {
                Ok(s) => successes.push(s),
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(successes)
        } else {
            Err(errors)
        }
    }
}

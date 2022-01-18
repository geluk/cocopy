//! Error handling for parsing.
use thiserror::Error;

use crate::{error::CompileError, lexer::tokens::Token, span::Span};

/// Construct a parse error.
pub fn failure<R>(stage: Stage, reason: Reason) -> Result<R, ParseError> {
    Err(ParseError::new(stage, reason))
}

/// Indicates both the parsing stage in which the error was encoutered
/// and the cause for the error.
#[derive(Debug, Error)]
#[error("{reason} when parsing {stage}")]
pub struct ParseError {
    stage: Stage,
    reason: Reason,
}
impl ParseError {
    pub fn new(stage: Stage, reason: Reason) -> Self {
        Self { stage, reason }
    }

    pub fn stage(&self) -> Stage {
        self.stage
    }
    pub fn reason(&self) -> &Reason {
        &self.reason
    }
}
impl From<ParseError> for CompileError {
    fn from(parse_error: ParseError) -> Self {
        Self::new(
            parse_error.to_string(),
            match parse_error.reason {
                Reason::UnexpectedToken(tok) => tok.source,
                // TODO: Add position information to reason so we don't have to repeat ourselves here.
                Reason::UnexpectedEndOfInput => Span::zero(),
                Reason::UnknownType(_) => Span::zero(),
            },
        )
    }
}

/// Indicates why parsing failed.
#[derive(Debug, Error)]
pub enum Reason {
    #[error("unexpected {0}")]
    UnexpectedToken(Token),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("unknown type: {0}")]
    UnknownType(String),
}

/// Indicates where parsing failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum Stage {
    #[error("the end of a statement")]
    StatementEnd,
    #[error("an expression")]
    Expr,
    #[error("the end of a parenthesised expression")]
    ParenExprEnd,
    #[error("the end of an index expression")]
    IndexEnd,
    #[error("a ternary if-expression")]
    TernaryElse,
    #[error("a parameter list")]
    ParameterList,
    #[error("a variable definition")]
    VarDef,
    #[error("a type annotation")]
    TypeSpec,
    #[error("a statement")]
    Statement,
    #[error("an assignment target")]
    AssignTarget,
}

/// Allows adding a parsing stage to an error.
pub trait AddStage {
    type Annotated;
    /// Enrich the error by specifying a parsing stage.
    fn add_stage(self, stage: Stage) -> Self::Annotated;
}
impl<O> AddStage for Result<O, Reason> {
    type Annotated = Result<O, ParseError>;

    fn add_stage(self, stage: Stage) -> Self::Annotated {
        self.map_err(|reason| ParseError { stage, reason })
    }
}

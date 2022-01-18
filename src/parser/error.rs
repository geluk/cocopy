//! Error handling for parsing.
use std::fmt;

use thiserror::Error;

use crate::{
    error::PositionalError,
    lexer::tokens::Token,
    span::{Bytes, Span},
};

/// A parsing error, indicating both the parsing stage in which the error was encoutered
/// and the cause for the error.
#[derive(Debug)]
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

impl PositionalError for ParseError {
    fn range(&self) -> Span {
        match &self.reason {
            Reason::UnexpectedToken(tok) => tok.source,
            // TODO: Add position information to reason so we don't have to repeat ourselves here.
            Reason::UnexpectedEndOfInput => Span::new(Bytes::new(0), Bytes::new(0)),
            Reason::UnknownType(_) => Span::new(Bytes::new(0), Bytes::new(0)),
        }
    }

    fn describe(&self) -> String {
        format!(
            "{} when parsing {} ({:?})",
            self.reason(),
            self.stage(),
            self.stage()
        )
    }
}

#[derive(Debug, Error)]
pub enum Reason {
    #[error("unexpected {0}")]
    UnexpectedToken(Token),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("unknown type: {0}")]
    UnknownType(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    /// The end of a complete program.
    StatementEnd,
    /// The start of an expression.
    Expr,
    /// The end of a parenthesised expression.
    ParenExprEnd,
    /// The end of an index expression.
    IndexEnd,
    /// The 'else' keyword of a ternary expression
    TernaryElse,
    /// A a parameter list
    ParameterList,
    /// A variable definition
    VarDef,
    /// A type specification
    TypeSpec,
    /// A statement
    Statement,
    /// An assignment target
    AssignTarget,
}
impl fmt::Display for Stage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Stage::Expr => "an expression",
            Stage::ParenExprEnd => "the end of a parenthesised expression",
            Stage::IndexEnd => "the end of an index expression",
            Stage::StatementEnd => "the end of a statement",
            Stage::TernaryElse => "a ternary if-expression",
            Stage::ParameterList => "a parameter list",
            Stage::VarDef => "a variable definition",
            Stage::TypeSpec => "a type annotation",
            Stage::Statement => "a statement",
            Stage::AssignTarget => "an assignment target",
        })
    }
}

pub trait AddStage {
    type Annotated;

    fn add_stage(self, stage: Stage) -> Self::Annotated;
}

impl<O> AddStage for Result<O, Reason> {
    type Annotated = Result<O, ParseError>;

    fn add_stage(self, stage: Stage) -> Self::Annotated {
        self.map_err(|reason| ParseError { stage, reason })
    }
}

pub fn failure<R>(stage: Stage, reason: Reason) -> Result<R, ParseError> {
    Err(ParseError::new(stage, reason))
}

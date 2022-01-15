use std::{fmt, ops::Range};

use thiserror::Error;

use crate::{error::PositionalError, lexer::tokens::Token};

/// A parsing error, indicating both the parsing stage in which the error was encoutered
/// and the cause for the error.
#[derive(Debug)]
pub struct ParseError {
    stage: Stage,
    reason: Reason,
}

impl ParseError {
    pub fn stage(&self) -> Stage {
        self.stage
    }
    pub fn reason(&self) -> &Reason {
        &self.reason
    }
}

impl PositionalError for ParseError {
    fn range(&self) -> Range<usize> {
        match &self.reason {
            Reason::UnexpectedToken(tok) => tok.source.clone(),
            Reason::UnexpectedEndOfInput => 0..0,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    /// The end of a complete program.
    ProgramEnd,
    /// The start of an expression.
    ExprStart,
    /// The end of a binary expression. May be followed by another binary operator in the case of
    /// compound expressions (a + b + c).
    BinExprEnd,
    /// The end of a parenthesised expression.
    ParenExprEnd,
    /// The end of an index expression.
    IndexEnd,
    /// The 'else' keyword of a ternary expression
    TernaryElse,
    /// The 'else' keyword of a ternary expression
    CallEnd,
}
impl fmt::Display for Stage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Stage::ExprStart => "the beginning of an expression",
            Stage::BinExprEnd => "an expression",
            Stage::ParenExprEnd => "the end of a parenthesised expression",
            Stage::IndexEnd => "the end of an index expression",
            Stage::ProgramEnd => "the end of the program",
            Stage::TernaryElse => "a ternary if-expression",
            Stage::CallEnd => "a parameter list",
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
    Err(ParseError { stage, reason })
}

use std::{
    fmt::{self, Display},
    ops::Range,
};

use thiserror::Error;

use crate::{error::PositionalError, lexer::tokens::Token};

#[derive(Clone, Copy)]
pub struct ParserState<'a> {
    tokens: &'a [Token],
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    pub fn next(self) -> ParseOption<'a, Token> {
        if self.tokens.is_empty() {
            None
        } else {
            Some((
                // TODO: Avoid this clone
                self.tokens[0].clone(),
                ParserState {
                    tokens: &self.tokens[1..],
                },
            ))
        }
    }

    pub fn advance(self) -> Self {
        let (_, state) = self.next().unwrap();
        state
    }
}

/// A parse operation that may fail, specifying both the reason for the failure, and the
/// parsing stage during which the failure occurred.
pub type ParseResult<'a, O> = Result<(O, ParserState<'a>), ParseError>;

/// A parse operation that may fail with a given reason, but without specifying the parsing stage
/// in which the failure occurred.
pub type Fallible<'a, O> = Result<(O, ParserState<'a>), Reason>;

/// A parse operation that may fail, without specifying a reason or parsing stage.
pub type ParseOption<'a, O> = Option<(O, ParserState<'a>)>;

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
}
impl Display for Stage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Stage::ExprStart => "the beginning of an expression",
            Stage::BinExprEnd => "an expression",
            Stage::ParenExprEnd => "the end of a parenthesised expression",
            Stage::IndexEnd => "the end of an index expression",
            Stage::ProgramEnd => "the end of the program",
            Stage::TernaryElse => "a ternary if-expression",
        })
    }
}

pub trait AnnotateStage {
    type Annotated;

    fn add_stage(self, stage: Stage) -> Self::Annotated;
}

impl<'a, O> AnnotateStage for Fallible<'a, O> {
    type Annotated = ParseResult<'a, O>;

    fn add_stage(self, stage: Stage) -> Self::Annotated {
        self.map_err(|reason| ParseError { stage, reason })
    }
}

pub fn success<O, E>(value: O, state: ParserState) -> Result<(O, ParserState), E> {
    Ok((value, state))
}

pub fn failure<R>(stage: Stage, reason: Reason) -> Result<R, ParseError> {
    Err(ParseError { stage, reason })
}

pub fn expect_token(state: ParserState) -> Fallible<Token> {
    match state.next() {
        Some((token, state)) => success(token, state),
        None => Err(Reason::UnexpectedEndOfInput),
    }
}

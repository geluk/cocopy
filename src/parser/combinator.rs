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

#[derive(Debug, Error, Clone)]
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

pub type ParseRes<'a, O, E> = Result<(O, ParserState<'a>), E>;

pub fn annotate<P, O>(stage: Stage, p: P) -> impl Fn(ParserState) -> ParseResult<O>
where
    P: Fn(ParserState) -> Fallible<O>,
{
    move |state| p(state).map_err(|reason| ParseError { stage, reason })
}

pub fn add_error<P, O, FE, E>(p: P, fe: FE) -> impl Fn(ParserState) -> ParseRes<O, E>
where
    FE: Fn() -> E,
    P: Fn(ParserState) -> ParseOption<O>,
{
    move |state| p(state).ok_or_else(|| fe())
}

pub fn parser<O, E, F>(f: F) -> impl Fn(ParserState) -> ParseRes<O, E>
where
    F: Fn(ParserState) -> ParseRes<O, E>,
{
    f
}

pub fn success_p<O, E>(result: O) -> impl Fn(ParserState) -> ParseRes<O, E>
where
{
    move |state| Ok((result, state))
}

pub fn token() -> impl Fn(ParserState) -> ParseRes<Token, Reason> {
    |state: ParserState| state.next().ok_or(Reason::UnexpectedEndOfInput)
}

pub fn expect<F>(filter: F) -> impl Fn(ParserState) -> ParseRes<Token, Reason>
where
    F: Fn(&Token) -> bool,
{
    filter_or(token(), move |t| filter(t), |t| Reason::UnexpectedToken(t))
}

pub fn filter_or<P, F, O, E, FE>(
    parser: P,
    filter: F,
    error_func: FE,
) -> impl Fn(ParserState) -> ParseRes<O, E>
where
    P: Fn(ParserState) -> ParseRes<O, E>,
    F: Fn(&O) -> bool,
    FE: Fn(O) -> E,
{
    move |state| {
        let (ra, state) = parser(state)?;

        if filter(&ra) {
            Ok((ra, state))
        } else {
            Err(error_func(ra))
        }
    }
}

pub fn filter_map<P, F, O1, O2, E, FE>(
    parser: P,
    filter: F,
    error_func: FE,
) -> impl Fn(ParserState) -> ParseRes<O2, E>
where
    P: Fn(ParserState) -> ParseRes<O1, E>,
    F: Fn(&O1) -> Option<O2>,
    FE: Fn(O1) -> E,
{
    move |state| {
        let (o1, state) = parser(state)?;
        let o2 = filter(&o1).ok_or_else(|| error_func(o1))?;
        Ok((o2, state))
    }
}

pub fn map<P, F, O1, O2, E>(p: P, f: F) -> impl Fn(ParserState) -> ParseRes<O2, E>
where
    P: Fn(ParserState) -> ParseRes<O1, E>,
    F: Fn(O1) -> O2,
{
    move |state| {
        let (ra, state) = p(state)?;
        Ok((f(ra), state))
    }
}

pub fn delimited<PA, PB, O1, O2, E>(pa: PA, pb: PB) -> impl Fn(ParserState) -> ParseRes<O1, E>
where
    PA: Fn(ParserState) -> ParseRes<O1, E>,
    PB: Fn(ParserState) -> ParseRes<O2, E>,
{
    move |state| {
        let (ra, state) = pa(state)?;
        let (rb, state) = pb(state)?;
        Ok((ra, state))
    }
}

pub fn sequence<PA, PB, O1, O2, E>(pa: PA, pb: PB) -> impl Fn(ParserState) -> ParseRes<(O1, O2), E>
where
    PA: Fn(ParserState) -> ParseRes<O1, E>,
    PB: Fn(ParserState) -> ParseRes<O2, E>,
{
    move |state| {
        let (ra, state) = pa(state)?;
        let (rb, state) = pb(state)?;
        Ok(((ra, rb), state))
    }
}

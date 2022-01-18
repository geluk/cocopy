//! Lexing functions for constructing a token stream.
use std::fmt::{Display, Formatter};

use thiserror::Error;

use crate::{error::PositionalError, span::*};

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("Indentation error")]
    IndentationError,
    #[error("Invalid integer literal: {0}")]
    IntegerLiteral(String),
    #[error("Unterminated string literal")]
    UnterminatedStringLiteral,
    #[error("Invalid string literal: {0}")]
    StringLiteral(String),
    #[error("Integer literals for positive numbers may not start with a zero")]
    IntegerLiteralZeroes,
    #[error("Invalid symbol")]
    UnknownToken,
}

#[derive(Error, Debug)]
pub struct LexError {
    pub range: Span,
    pub error_type: ErrorType,
}

impl LexError {
    pub fn length(&self) -> Bytes {
        self.range.length()
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.error_type))
    }
}

impl PositionalError for LexError {
    fn range(&self) -> Span {
        self.range
    }

    fn describe(&self) -> String {
        self.to_string()
    }
}

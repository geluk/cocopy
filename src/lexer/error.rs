//! Representation of lexer errors.
use thiserror::Error;

use crate::{error::CompileError, span::*};

/// A lexer error, consisting of an error type with position information.
#[derive(Debug, Error)]
#[error("{error_type}")]
pub struct LexError {
    pub span: Span,
    pub error_type: ErrorType,
}
impl LexError {
    pub fn length(&self) -> Bytes {
        self.span.length()
    }
}

/// The type of error that was encountered.
#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("indentation error")]
    IndentationError,
    #[error("mismatch between spaces and tabs")]
    IndentationMismatch,
    #[error("invalid integer literal: {0}")]
    IntegerLiteral(String),
    #[error("unterminated string literal")]
    UnterminatedStringLiteral,
    #[error("invalid string literal: {0}")]
    StringLiteral(String),
    #[error("integer literals for positive numbers may not start with a zero")]
    IntegerLiteralZeroes,
    #[error("invalid symbol")]
    UnknownToken,
}
impl From<LexError> for CompileError {
    fn from(error: LexError) -> Self {
        Self::new(error.to_string(), error.span)
    }
}

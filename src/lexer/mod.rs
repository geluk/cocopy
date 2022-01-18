//! Functionality for converting a source code string into a stream of tokens.
mod char_ext;
mod char_lexer;
mod error;
mod lexer;
pub mod tokens;

pub use lexer::*;

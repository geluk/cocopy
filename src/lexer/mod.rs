//! Functionality for converting a source code string into a [`Token`] stream.
mod char_ext;
mod char_lexer;
mod error;
#[allow(clippy::module_inception)]
mod lexer;

pub mod tokens;

pub use lexer::lex;

#[allow(unused_imports, reason = "Docstring uses this")]
use tokens::Token;

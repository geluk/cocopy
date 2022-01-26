//! Converts a token stream into an abstract syntax tree.
mod delimiter;
mod error;
mod fixity;
#[allow(clippy::module_inception)]
mod parser;
mod parser_base;

pub use parser::parse;

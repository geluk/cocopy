//! Converts a token stream into an abstract syntax tree.
mod delimiter;
mod error;
mod fixity;
mod parser;
mod parser_base;

pub mod syntax_tree;

pub use parser::parse;

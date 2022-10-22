//! Tokens, as produced by the lexer.
use std::fmt::{self, Display};

use crate::span::{Bytes, Span};

#[derive(Clone, Debug)]
pub struct Token {
    pub source: Span,
    pub kind: TokenKind,
}
impl Token {
    pub fn length(&self) -> Bytes {
        self.source.length()
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Structure(Structure),
    Keyword(Keyword),
    Identifier(String),
    Symbol(Symbol),
    Literal(Literal),
}

/// A structural token, representing a line break, indent, or dedent.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Structure {
    Newline,
    Indent,
    Dedent,
}

/// A reserved keyword. Not all Python keywords are used in ChocoPy, but they
/// are still treated as reserved, in order to maintain forwards compatibility
/// with Python.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    // Values
    False,
    None,
    True,
    // Used keywords
    And,
    Class,
    Def,
    Elif,
    Else,
    For,
    Global,
    If,
    Import,
    Nonlocal,
    Not,
    Or,
    Pass,
    Return,
    While,
    // Unused keywords
    As,
    Assert,
    Async,
    Await,
    Break,
    Continue,
    Del,
    Except,
    Finally,
    From,
    In,
    Is,
    Lambda,
    Raise,
    Try,
    With,
    Yield,
}

/// A symbol.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Symbol {
    Plus,
    Minus,
    Asterisk,
    DoubleSlash,
    Percent,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
    Assign,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Comma,
    Colon,
    Period,
    Arrow,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i32),
    String(String),
}

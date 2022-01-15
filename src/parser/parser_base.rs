use std::{iter::Peekable, slice::Iter};

use super::error::*;
use crate::lexer::tokens::*;

#[derive(Clone)]
pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn next(&mut self) -> Result<&Token, Reason> {
        self.tokens.next().ok_or(Reason::UnexpectedEndOfInput)
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek().copied()
    }

    /// Tries to read a token of the given kind. If the token does not match, the parser is not
    /// advanced, and an error is returned instead.
    pub fn expect(&mut self, kind: TokenKind) -> Result<TokenKind, Reason> {
        let next = self.peek().ok_or(Reason::UnexpectedEndOfInput)?;

        if next.kind == kind {
            self.next().unwrap();
            Ok(kind)
        } else {
            Err(Reason::UnexpectedToken(next.clone()))
        }
    }

    pub fn expect_symbol(&mut self, symbol: Symbol) -> Result<Symbol, Reason> {
        let next = self.next()?;

        if next.kind == TokenKind::Symbol(symbol) {
            Ok(symbol)
        } else {
            Err(Reason::UnexpectedToken(next.clone()))
        }
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> Result<Keyword, Reason> {
        let next = self.next()?;

        if next.kind == TokenKind::Keyword(keyword) {
            Ok(keyword)
        } else {
            Err(Reason::UnexpectedToken(next.clone()))
        }
    }

    pub fn expect_structure(&mut self, structure: Structure) -> Result<Structure, Reason> {
        let next = self.next()?;

        if next.kind == TokenKind::Structure(structure) {
            Ok(structure)
        } else {
            Err(Reason::UnexpectedToken(next.clone()))
        }
    }
}

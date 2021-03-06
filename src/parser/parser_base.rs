//! Basic token parsing functions and combinators.
use std::{iter::Peekable, slice::Iter};

use crate::{
    lexer::tokens::*,
    span::{Bytes, Span},
};

use super::error::*;

#[derive(Clone)]
pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    token_position: usize,
    end_position: Bytes,
}
impl<'a> Parser<'a> {
    /// Construct a new parser from the given token stream.
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
            token_position: 0,
            end_position: Bytes::new(0),
        }
    }

    /// Read the next token, if it is available.
    pub fn next(&mut self) -> Result<&Token, Reason> {
        match self.tokens.next() {
            Some(token) => {
                self.token_position += 1;
                self.end_position = token.source.end();
                Ok(token)
            }
            None => Err(Reason::UnexpectedEndOfInput),
        }
    }

    /// Check if there is another token. Returns [`None`] if the end was reached.
    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek().copied()
    }

    /// Check if there is another token, returning its [`TokenKind`] wrapped
    /// in an [`Option`].
    pub fn peek_kind(&mut self) -> Option<&TokenKind> {
        self.peek().map(|t| &t.kind)
    }

    /// Returns `true` if another token is available.
    pub fn has_next(&mut self) -> bool {
        self.peek().is_some()
    }

    /// Retrieves the position of the parser.
    /// The position is aligned on the start position of the next token if
    /// another token was available. Otherwise, the end position of the last
    /// encountered token is used instead.
    pub fn position(&mut self) -> Bytes {
        match self.tokens.peek() {
            Some(t) => t.source.start(),
            _ => self.end_position,
        }
    }

    /// Construct a span from the given start point to the current position
    /// of the parser.
    pub fn span_from(&self, start: Bytes) -> Span {
        Span::new(start, self.end_position)
    }

    /// Tries to read a token of the given kind. If the token does not match,
    /// the parser is not advanced, and an error is returned instead.
    pub fn recognise(&mut self, kind: TokenKind) -> Result<TokenKind, Reason> {
        let next = self.peek().ok_or(Reason::UnexpectedEndOfInput)?;

        if next.kind == kind {
            self.next().unwrap();
            Ok(kind)
        } else {
            Err(Reason::UnexpectedToken(next.clone()))
        }
    }

    /// Tries to read the next token as the given symbol.
    /// If the token does not match, the parser is not advanced,
    /// and an error is returned instead.
    pub fn recognise_symbol(&mut self, symbol: Symbol) -> Result<Symbol, Reason> {
        self.recognise(TokenKind::Symbol(symbol))?;
        Ok(symbol)
    }

    /// Tries to read the next token as the given keyword.
    /// If the token does not match, the parser is not advanced,
    /// and an error is returned instead.
    pub fn recognise_keyword(&mut self, keyword: Keyword) -> Result<Keyword, Reason> {
        self.recognise(TokenKind::Keyword(keyword))?;
        Ok(keyword)
    }

    /// Tries to read the next token as the given structural token.
    /// If the token does not match, the parser is not advanced,
    /// and an error is returned instead.
    pub fn recognise_structure(&mut self, structure: Structure) -> Result<Structure, Reason> {
        self.recognise(TokenKind::Structure(structure))?;
        Ok(structure)
    }

    /// Tries to read the next token as an identifier.
    /// If the token does not match, the parser is not advanced,
    /// and an error is returned instead.
    pub fn recognise_identifier(&mut self) -> Result<(String, Token), Reason> {
        let next = self.peek().ok_or(Reason::UnexpectedEndOfInput)?;

        match &next.kind {
            TokenKind::Identifier(id) => {
                let (id, next) = (id.clone(), next.clone());
                self.next().unwrap();
                Ok((id, next))
            }
            _ => Err(Reason::UnexpectedToken(next.clone())),
        }
    }

    /// Transforms a parsing function to only mutate the parsing state if it succeeds.
    /// If it fails, returns [`Err`] without modifying the parser state.
    pub fn recognise_parser<P, R, E>(&mut self, parse_func: P) -> Result<R, E>
    where
        P: FnOnce(&mut Self) -> Result<R, E>,
    {
        let mut sub_parser = self.clone();
        let res = parse_func(&mut sub_parser);
        if res.is_ok() {
            *self = sub_parser;
        }
        res
    }

    /// Tries two parsing functions, returning the result of the first if it succeeds.
    /// If the first fails, tries the second parser, returning its result if it succeeds.
    /// If both parsers fail, returns the error of the parser that advanced the farthest.
    pub fn alt<P1, P2, R, E>(&mut self, p1: P1, p2: P2) -> Result<R, E>
    where
        P1: FnOnce(&mut Self) -> Result<R, E>,
        P2: FnOnce(&mut Self) -> Result<R, E>,
    {
        let mut sub_parser_1 = self.clone();
        let result_1 = p1(&mut sub_parser_1);
        if result_1.is_ok() {
            *self = sub_parser_1;
            return result_1;
        }

        let mut sub_parser_2 = self.clone();
        let result_2 = p2(&mut sub_parser_2);
        if result_2.is_ok() {
            *self = sub_parser_2;
            return result_2;
        }

        if sub_parser_1.token_position >= sub_parser_2.token_position {
            *self = sub_parser_1;
            result_1
        } else {
            *self = sub_parser_2;
            result_2
        }
    }
}

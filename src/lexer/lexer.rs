use std::{
    fmt::{Display, Formatter},
    num::ParseIntError,
    ops::Range,
};

use thiserror::Error;

use crate::error::PositionalError;

use super::{char_lexer::CharLexer, tokens::*};

type LexResult<T> = Option<Result<T, LexError>>;

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("Indentation error")]
    IndentationError,
    #[error("Invalid integer literal: {0}")]
    IntegerLiteral(String),
    #[error("Integer literals for positive numbers may not start with a zero")]
    IntegerLiteralZeroes,
    #[error("Invalid symbol")]
    UnknownToken,
}

#[derive(Error, Debug)]
pub struct LexError {
    pub range: Range<usize>,
    pub error_type: ErrorType,
}

impl LexError {
    pub fn length(&self) -> usize {
        self.range.end - self.range.start
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Lexer error ({:?}) at {} - {}",
            self.error_type, self.range.start, self.range.end
        ))
    }
}

impl PositionalError for LexError {
    fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    fn describe(&self) -> String {
        self.to_string()
    }
}

struct Lexer<'s> {
    lexer: CharLexer<'s>,
    indent_level: usize,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
}

pub fn lex(source: &str) -> Result<Vec<Token>, Vec<LexError>> {
    Lexer::new(source).run()
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            lexer: CharLexer::new(source),
            indent_level: 0,
            tokens: vec![],
            errors: vec![],
        }
    }

    /// Finishes the lexer and consumes it, producing a [`Vec<Token>`]
    /// containing the tokens it read.
    fn run(mut self) -> Result<Vec<Token>, Vec<LexError>> {
        while self.line() {}

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(self.tokens)
        }
    }

    /// Attempts to read a single line. Returns `true` if another line was
    /// available, or `false` if the end of the file has been reached.
    fn line(&mut self) -> bool {
        // If this line is empty, skip past it.
        if self.try_consume_empty_line() {
            println!("Consuming empty line");
            return true;
        }

        // Consume all indentation up to the first token.
        // If this line contains an indentation error, skip it.
        if let Err(error) = self.consume_indentation() {
            self.lexer.consume(error.length());
            self.errors.push(error);
            self.advance_to_next_line();
            return !self.lexer.is_finished();
        }

        // Consume as many valid non-structural tokens as possible.
        let mut had_tokens = false;
        while self.try_consume_token() {
            had_tokens = true;
        }

        // If we can consume a newline now, we successfully parsed the line.
        if self.try_consume_newline() {
            println!("Consuming newline");
            return true;
        }

        // If we can't consume a newline anymore, we'd expect to be at the
        // end of the file now.
        if !self.lexer.is_finished() {
            // Based on the assumption that we've handled every possible
            // type of token in `try_consume_token()`.
            unreachable!("Failed to consume token");
        }

        // Emit 'virtual' newline and dedents here
        // to ensure our token stream is well-formed.
        if had_tokens {
            println!("Emitting final newline");
            self.insert_unsized_newline();
        }
        if self.indent_level > 0 {
            println!("Emitting remaining dedents");
            self.emit_dedents_to(0);
        }

        false
    }

    /// Attempts to consume a newline.
    fn try_consume_newline(&mut self) -> bool {
        if self.lexer.recognise('\n') || self.lexer.recognise('\r') {
            let newline = self.make_offset_token(-1, 1, TokenKind::Structure(Structure::Newline));
            self.tokens.push(newline);
            true
        } else {
            false
        }
    }

    fn insert_unsized_newline(&mut self) {
        let newline = self.make_sized_token(0, TokenKind::Structure(Structure::Newline));
        self.tokens.push(newline);
    }

    /// Tries to consume a non-structural token (no newlines, indents, or
    /// dedents). If a valid token is found, the lexer is advanced past the
    /// token and any non-newline whitespace following it.
    /// Returns `true` if a token was found, `false` otherwise.
    fn try_consume_token(&mut self) -> bool {
        let next_token = self
            .keyword()
            .or_else(|| self.identifier())
            .or_else(|| self.symbol())
            .map(Ok)
            .or_else(|| self.integer_literal())
            .or_else(|| self.string_literal())
            .or_else(|| self.unknown_token().map(Err));

        match next_token {
            None => false,
            Some(result) => {
                match result {
                    Ok(token) => {
                        self.lexer.consume(token.length());
                        self.tokens.push(token);
                    }
                    Err(error) => {
                        self.lexer.consume(error.length());
                        self.errors.push(error);
                    }
                }
                self.lexer.consume_while(CharExt::is_non_newline_whitespace);
                true
            }
        }
    }

    /// Advances the lexer to the next line, returning a [`String`] containing
    /// the characters that were read.
    fn advance_to_next_line(&mut self) -> String {
        let read = self.lexer.consume_while(CharExt::is_not_linebreak);
        // try_next because we may be at the end of input
        self.lexer.try_next();
        read
    }

    /// Looks ahead to determine whether the current line is an empty line
    /// (optional whitespace followed by a line delimiter)
    /// If it is empty, consumes it and returns `true`.
    /// Otherwise, returns `false`.
    /// Note that this returns `false` on the last line in the file, because
    /// even if it is empty, it isn't followed by a line delimiter.
    fn try_consume_empty_line(&mut self) -> bool {
        let mut lexer = self.lexer.clone();
        lexer.consume_while(CharExt::is_non_newline_whitespace);

        match lexer.try_next() {
            Some(ch) if ch == '\n' || ch == '\r' => {
                self.lexer = lexer;
                true
            }
            Some(_) => false,
            None => {
                self.lexer.finish();
                false
            }
        }
    }

    /// Emits as many dedents as required in order to bring the lexer back to the
    /// specified indentation level.
    fn emit_dedents_to(&mut self, new_indent_level: usize) {
        for _ in new_indent_level..self.indent_level {
            self.tokens
                .push(self.make_sized_token(0, TokenKind::Structure(Structure::Dedent)));
        }
        self.indent_level = new_indent_level;
    }

    /// Consumes the indentation at the start of a line, optionally generating
    /// one or more indents or dedents in the process.
    fn consume_indentation(&mut self) -> Result<(), LexError> {
        for level in 0..self.indent_level {
            match self.lexer.peek() {
                Some('\t') => {
                    self.lexer.next();
                }
                Some(' ') => return Err(self.make_error(1, ErrorType::IndentationError)),
                _ => {
                    self.emit_dedents_to(level);
                    return Ok(());
                }
            }
        }

        while self.lexer.recognise('\t') {
            self.tokens.push(self.make_offset_token(
                -1,
                1,
                TokenKind::Structure(Structure::Indent),
            ));
            self.indent_level += 1;
        }
        Ok(())
    }

    /// Checks the next token without consuming it. If it's a keyword,
    /// returns the token, wrapped in an [`Option`]. If it's something else,
    /// returns [`None`].
    fn keyword(&self) -> Option<Token> {
        let text: String = self
            .lexer
            .iter()
            .take_while(char::is_ascii_alphabetic)
            .collect();

        let keyword = match text.as_str() {
            // Values
            "False" => Keyword::False,
            "None" => Keyword::None,
            "True" => Keyword::True,
            // Used keywords
            "and" => Keyword::And,
            "class" => Keyword::Class,
            "def" => Keyword::Def,
            "elif" => Keyword::Elif,
            "else" => Keyword::Else,
            "for" => Keyword::For,
            "global" => Keyword::Global,
            "if" => Keyword::If,
            "import" => Keyword::Import,
            "nonlocal" => Keyword::Nonlocal,
            "not" => Keyword::Not,
            "or" => Keyword::Or,
            "pass" => Keyword::Pass,
            "return" => Keyword::Return,
            "while" => Keyword::While,
            // Unused keywords
            "as" => Keyword::As,
            "assert" => Keyword::Assert,
            "async" => Keyword::Async,
            "await" => Keyword::Await,
            "break" => Keyword::Break,
            "continue" => Keyword::Continue,
            "del" => Keyword::Del,
            "except" => Keyword::Except,
            "finally" => Keyword::Finally,
            "from" => Keyword::From,
            "in" => Keyword::In,
            "is" => Keyword::Is,
            "lambda" => Keyword::Lambda,
            "raise" => Keyword::Raise,
            "try" => Keyword::Try,
            "with" => Keyword::With,
            "yield" => Keyword::Yield,
            _ => return None,
        };

        Some(self.make_sized_token(text.len(), TokenKind::Keyword(keyword)))
    }

    /// Checks the next token without consuming it. If it's an identifier,
    /// returns the token, wrapped in an [`Option`]. If it's something else,
    /// returns [`None`].
    fn identifier(&self) -> Option<Token> {
        let mut iter = self.lexer.iter();

        // The first character of an identifier may not be a digit.
        iter.peek()
            .filter(|&&c| c.is_ascii_alphabetic() || c == '_')?;

        let id: String = iter
            .take_while(|&c| c.is_ascii_alphanumeric() || c == '_')
            .collect();

        Some(self.make_sized_token(id.len(), TokenKind::Identifier(id)))
    }

    /// Checks the next token without consuming it. If it's a symbol,
    /// returns the token, wrapped in an [`Option`]. If it's something else,
    /// returns [`None`].
    fn symbol(&self) -> Option<Token> {
        let mut iter = self.lexer.clone();

        let first = iter.try_next()?;
        let second = iter.try_next();

        // Try matching two-char symbols first
        match (first, second) {
            ('/', Some('/')) => Some(Symbol::DoubleSlash),
            ('<', Some('=')) => Some(Symbol::Lte),
            ('>', Some('=')) => Some(Symbol::Gte),
            ('=', Some('=')) => Some(Symbol::Eq),
            ('!', Some('=')) => Some(Symbol::Neq),
            ('-', Some('>')) => Some(Symbol::Arrow),
            _ => None,
        }
        .map(|s| self.make_sized_token(2, TokenKind::Symbol(s)))
        .or_else(|| {
            // Now try matching one-char symbols
            let two_char = match first {
                '+' => Symbol::Plus,
                '-' => Symbol::Minus,
                '*' => Symbol::Asterisk,
                '%' => Symbol::Percent,
                '<' => Symbol::Lt,
                '>' => Symbol::Gt,
                '=' => Symbol::Assign,
                '(' => Symbol::OpenParen,
                ')' => Symbol::CloseParen,
                '[' => Symbol::OpenBracket,
                ']' => Symbol::CloseBracket,
                ',' => Symbol::Comma,
                ':' => Symbol::Colon,
                '.' => Symbol::Period,
                _ => return None,
            };
            Some(self.make_sized_token(1, TokenKind::Symbol(two_char)))
        })
    }

    /// Checks the next token without consuming it.
    /// If it is not an integer literal, returns [`Ok(None)`],
    /// If it is an invalid literal, returns an error.
    /// If it is a valid literal, returns
    fn integer_literal(&self) -> LexResult<Token> {
        let mut lexer = self.lexer.clone();

        let first_digit = lexer.peek().filter(char::is_ascii_digit)?;

        let number = lexer.consume_while(char::is_ascii_digit);

        let parse_result = match (first_digit, number.len()) {
            ('0', l) if l > 1 => Err(self.make_error(l, ErrorType::IntegerLiteralZeroes)),
            (_, l) => number.parse().map_err(|err: ParseIntError| {
                self.make_error(l, ErrorType::IntegerLiteral(err.to_string()))
            }),
        }
        .map(|num| self.make_sized_token(number.len(), TokenKind::Literal(Literal::Integer(num))));

        Some(parse_result)
    }

    fn string_literal(&self) -> LexResult<Token> {
        None // TODO
    }

    fn unknown_token(&self) -> Option<LexError> {
        let mut lexer = self.lexer.clone();

        let unknown_token = lexer.consume_while(|ch| !ch.is_whitespace());

        if !unknown_token.is_empty() {
            Some(self.make_error(1, ErrorType::UnknownToken))
        } else {
            None
        }
    }

    fn make_error(&self, length: usize, error_type: ErrorType) -> LexError {
        self.make_offset_error(0, length, error_type)
    }

    fn make_offset_error(&self, offset: isize, length: usize, error_type: ErrorType) -> LexError {
        let position: usize = (self.lexer.position() as isize + offset) as usize;

        LexError {
            range: position..position + length,
            error_type,
        }
    }

    /// Constructs a token of size `length` and [`TokenKind`] `kind`,
    /// starting at the lexer's current position.
    fn make_sized_token(&self, length: usize, kind: TokenKind) -> Token {
        self.make_offset_token(0, length, kind)
    }

    /// Constructs a token of size `length` and [`TokenKind`] `kind`,
    /// Offset by `offset` characters from the lexer's current position.
    fn make_offset_token(&self, offset: isize, length: usize, kind: TokenKind) -> Token {
        let position: usize = (self.lexer.position() as isize + offset) as usize;

        Token {
            source: position..position + length,
            kind,
        }
    }
}

trait CharExt {
    fn is_non_newline_whitespace(&self) -> bool;

    fn is_not_linebreak(&self) -> bool;
}

impl CharExt for char {
    fn is_non_newline_whitespace(&self) -> bool {
        self.is_whitespace() && *self != '\n' && *self != '\r'
    }

    fn is_not_linebreak(&self) -> bool {
        *self != '\n' && *self != '\r'
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        describe_error,
        lexer::tokens::{Token, TokenKind},
    };

    use super::*;

    fn expect_kinds(expected: Vec<TokenKind>, actual: Vec<Token>) {
        let actual_kinds: Vec<&TokenKind> = actual.iter().map(|t| &t.kind).collect();

        for (idx, (kind, token)) in expected.iter().zip(actual.iter()).enumerate() {
            assert_eq!(
                kind, &token.kind,
                "\n\nWhen comparing:\n  (expected) {:?}\n  (actual)   {:?}\n\nExpected token #{} to have kind '{:?}', but was '{:?}'",
                expected, actual_kinds, idx, kind, &token.kind
            );
        }
        assert_eq!(
            expected.len(),
            actual.len(),
            "\n\nWhen comparing:\n  (expected) {:?}\n  (actual)   {:?}\n\nExpected to find {} tokens, but found {}",
            expected,
            actual_kinds,
            expected.len(),
            actual.len()
        );
    }

    fn assert_lexes(source: &str, token_kinds: Vec<TokenKind>) {
        let tokens = lex(source).expect("Unexpected lexer error");

        expect_kinds(token_kinds, tokens);
    }

    fn assert_token_lexes(source: &str, token_kind: TokenKind) {
        let token_kinds = vec![token_kind, TokenKind::Structure(Structure::Newline)];

        assert_lexes(source, token_kinds);
    }

    fn assert_lex_fails(source: &str) {
        match lex(source) {
            Ok(tokens) => panic!("Expected lexer to fail, but it produced: {:#?}", tokens),
            Err(errors) => {
                for error in errors {
                    println!("{}", error);
                    describe_error(&error, &source);
                    println!();
                }
            }
        }
    }

    #[test]
    fn lex_false() {
        assert_token_lexes("False", TokenKind::Keyword(Keyword::False))
    }

    #[test]
    fn lex_true() {
        assert_token_lexes("True", TokenKind::Keyword(Keyword::True))
    }

    #[test]
    fn integer_literal_number() {
        assert_token_lexes("123", TokenKind::Literal(Literal::Integer(123)))
    }

    #[test]
    fn integer_literal_zero() {
        assert_token_lexes("0", TokenKind::Literal(Literal::Integer(0)))
    }

    #[test]
    fn integer_literal_i32_max() {
        assert_token_lexes(
            "2147483647",
            TokenKind::Literal(Literal::Integer(2147483647)),
        )
    }

    #[test]
    fn integer_literal_above_i32_max_fails() {
        assert_lex_fails("2147483648")
    }

    #[test]
    fn integer_literal_nonzero_starts_with_zero_fails() {
        assert_lex_fails("01")
    }

    /// ChocoPy Language Reference: 3.1.1
    #[test]
    fn newline_style_lf() {
        assert_lexes(
            "one\ntwo",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Identifier("two".to_string()),
                TokenKind::Structure(Structure::Newline),
            ],
        );
    }

    /// ChocoPy Language Reference: 3.1.1
    #[test]
    fn newline_style_cr() {
        assert_lexes(
            "one\rtwo",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Identifier("two".to_string()),
                TokenKind::Structure(Structure::Newline),
            ],
        );
    }

    /// ChocoPy Language Reference: 3.1.1
    #[test]
    fn newline_style_crlf() {
        assert_lexes(
            "one\r\ntwo",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Identifier("two".to_string()),
                TokenKind::Structure(Structure::Newline),
            ],
        );
    }

    // TODO: Implement 3.1.3

    /// ChocoPy Language Reference: 3.1.4
    #[test]
    fn blank_line_is_ignored() {
        assert_lexes(
            "one\n   \t  \ntwo",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Identifier("two".to_string()),
                TokenKind::Structure(Structure::Newline),
            ],
        );
    }

    /// ChocoPy Language Reference: 3.1.5
    /// NOTE - Does not comply with the spec. Differences:
    ///  - Tabs are the only allowed form of indentation
    #[test]
    fn tabs_generate_indents() {
        assert_lexes(
            "one\n\ttwo\n",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Indent),
                TokenKind::Identifier("two".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Dedent),
            ],
        );
    }

    /// ChocoPy Language Reference: 3.1.5
    /// NOTE - Does not comply with the spec. Differences:
    ///  - Tabs are the only allowed form of indentation
    #[test]
    fn multiple_tabs_generate_multiple_indents() {
        assert_lexes(
            "one\n\t\ttwo\n",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Indent),
                TokenKind::Structure(Structure::Indent),
                TokenKind::Identifier("two".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Dedent),
                TokenKind::Structure(Structure::Dedent),
            ],
        );
    }

    /// ChocoPy Language Reference: 3.1.6
    #[test]
    fn whitespace_separates_tokens() {
        assert_lexes(
            "one\t\t  two",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Identifier("two".to_string()),
                TokenKind::Structure(Structure::Newline),
            ],
        );
    }

    /// ChocoPy Language Reference: 3.1.6
    #[test]
    fn whitespace_not_required_if_unambiguous() {
        assert_lexes(
            "one+1",
            vec![
                TokenKind::Identifier("one".to_string()),
                TokenKind::Symbol(Symbol::Plus),
                TokenKind::Literal(Literal::Integer(1)),
                TokenKind::Structure(Structure::Newline),
            ],
        );
    }

    /// ChocoPy Language Reference: 3.2
    #[test]
    fn identifier_consists_of_alphabetic_characters() {
        assert_token_lexes("hello", TokenKind::Identifier("hello".to_string()))
    }

    /// ChocoPy Language Reference: 3.2
    #[test]
    fn identifier_may_contain_underscores() {
        assert_token_lexes("_hello", TokenKind::Identifier("_hello".to_string()))
    }

    /// ChocoPy Language Reference: 3.2
    #[test]
    fn identifier_may_contain_uppercase() {
        assert_token_lexes("SOMETHING", TokenKind::Identifier("SOMETHING".to_string()))
    }

    /// ChocoPy Language Reference: 3.2
    /// TODO: Assert that this lexes as a single invalid token (it currently doesn't)
    #[test]
    fn identifier_may_not_contain_non_ascii_alphabetic() {
        assert_lex_fails("`hoi abc")
    }

    #[test]
    fn if_block() {
        assert_lexes(
            "if True:\n\tx",
            vec![
                TokenKind::Keyword(Keyword::If),
                TokenKind::Keyword(Keyword::True),
                TokenKind::Symbol(Symbol::Colon),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Indent),
                TokenKind::Identifier("x".to_string()),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Dedent),
            ],
        )
    }

    #[test]
    fn function_block() {
        assert_lexes(
            "def hello():\n\tTrue\n",
            vec![
                TokenKind::Keyword(Keyword::Def),
                TokenKind::Identifier("hello".to_string()),
                TokenKind::Symbol(Symbol::OpenParen),
                TokenKind::Symbol(Symbol::CloseParen),
                TokenKind::Symbol(Symbol::Colon),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Indent),
                TokenKind::Keyword(Keyword::True),
                TokenKind::Structure(Structure::Newline),
                TokenKind::Structure(Structure::Dedent),
            ],
        )
    }
}

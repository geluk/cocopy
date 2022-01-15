use std::{iter::Peekable, str::Chars};

/// Abstraction over a peekable char iterator with position information.
///
/// Since it only holds an iterator and a position, this type is very
/// lightweight, making it easy to clone in order to handle branching
/// and `LL(k)` lookahead.
#[derive(Clone)]
pub struct CharLexer<'a> {
    chars: Peekable<Chars<'a>>,
    position: usize,
}

impl<'a> CharLexer<'a> {
    /// Constructs a new [`CharLexer`] for the given source string,
    /// starting at position `0`.
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            position: 0,
        }
    }

    /// Reads the next character.
    /// Panics if the lexer cannot advance any further.
    pub fn next(&mut self) -> char {
        self.try_next().expect("Unable to advance the lexer")
    }

    /// Tries to advance the lexer by one character.
    /// Returns the character wrapped in an [`Option`] if it was successful,
    /// or [`None`] if the lexer cannot advance any further.
    pub fn try_next(&mut self) -> Option<char> {
        let next = self.chars.next();
        if next.is_some() {
            self.position += 1;
        }
        next
    }

    /// Returns the next character without consuming it.
    /// Returns [`None`] if the lexer cannot advance any further.
    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    /// Retrieves the position of the lexer.
    pub fn position(&self) -> usize {
        self.position
    }

    /// Peeks at the next character, and consumes it if it matches the provided character.
    /// Returns true if the character was consumed, false otherwise.
    pub fn recognise(&mut self, character: char) -> bool {
        match self.chars.peek() {
            Some(ch) if ch == &character => {
                self.next();
                true
            }
            _ => false,
        }
    }

    /// Consumes `count` characters. Panics if less than `count` characters could be consumed.
    pub fn consume(&mut self, count: usize) {
        for _ in 0..count {
            self.next();
        }
    }

    /// Consumes characters while `P(char)` evaluates to `true`.
    /// Returns a [`String`] containing the consumed characters.
    pub fn consume_while<P>(&mut self, mut predicate: P) -> String
    where
        Self: Sized,
        P: FnMut(&char) -> bool,
    {
        let mut matches = String::new();
        while let Some(ch) = self.chars.peek() {
            if predicate(ch) {
                matches.push(*ch);
                self.next();
            } else {
                break;
            }
        }
        matches
    }

    /// Advances the lexer to the end of its input.
    pub fn finish(&mut self) {
        while let Some(_) = self.try_next() {}
    }

    /// Checks if the lexer is finished.
    pub fn is_finished(&mut self) -> bool {
        self.peek().is_none()
    }
}

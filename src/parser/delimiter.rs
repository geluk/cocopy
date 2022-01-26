//! Describes what tokens a parser will consider acceptable to finish a parseable token sequence.
use crate::lexer::tokens::*;

use super::error::Stage;

pub struct Delimiter {
    token_kind: TokenKind,
    stage: Stage,
    may_consume: bool,
    include_in_span: bool,
}
impl Delimiter {
    /// Construct a delimiter for an expression surrounded by parentheses: `(x)`.
    pub fn parentheses() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::CloseParen),
            stage: Stage::ParenExprEnd,
            may_consume: true,
            include_in_span: true,
        }
    }
    /// Construct a delimiter for an expression terminated by a newline: `x\n`.
    pub fn newline() -> Self {
        Self {
            token_kind: TokenKind::Structure(Structure::Newline),
            stage: Stage::StatementEnd,
            may_consume: true,
            include_in_span: false,
        }
    }
    /// Construct a delimiter for an indexing sub-expression: `[x]`.
    pub fn index() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::CloseBracket),
            stage: Stage::IndexEnd,
            may_consume: true,
            include_in_span: true, // TODO: false?
        }
    }
    /// Construct a delimiter for a function call sub-expression: `(x)`.
    pub fn function_call() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::CloseParen),
            stage: Stage::ParameterList,
            may_consume: true,
            include_in_span: false,
        }
    }
    /// Construct a delimiter for an assignment target expression: `x : int =`.
    pub fn assign() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::Assign),
            stage: Stage::AssignTarget,
            may_consume: true,
            include_in_span: false,
        }
    }
    /// Construct a delimiter for the middle expression of a ternary if-expression: `x if y else`.
    pub fn ternary_if() -> Self {
        Self {
            token_kind: TokenKind::Keyword(Keyword::Else),
            stage: Stage::TernaryElse,
            may_consume: true,
            include_in_span: false,
        }
    }

    /// The parsing stage in which this delimiter is used.
    pub fn stage(&self) -> Stage {
        self.stage
    }

    /// If true, an expression parser must consume this delimiter when finished.
    /// If false, an expression parser may use this delimiter to determine when it should stop
    /// parsing, but it must not consume it. This is used to allow subparsers to recognise
    /// where they should stop parsing, while leaving the token intact so their parent parser
    /// is able to consume it.
    pub fn may_consume(&self) -> bool {
        self.may_consume
    }

    /// The token kind represented by this delimiter.
    pub fn token_kind(&self) -> &TokenKind {
        &self.token_kind
    }

    /// Returns `true` if the delimiter token should be considered part
    /// of the expression it delimits.
    pub fn include_in_span(&self) -> bool {
        self.include_in_span
    }

    /// Returns `true` if the supplied token satisfies the required delimiter type.
    pub fn is_satisfied_by(&self, token: &Token) -> bool {
        token.kind == self.token_kind
    }

    /// Constructs a new delimiter for a subexpression parser. This delimiter will be a copy of the
    /// current delimiter, but with [`self.may_consume`] set to `false` to indicate that the subparser
    /// must leave the token for the parent parser to consume.
    pub fn for_subexpr(&self) -> Self {
        Self {
            token_kind: self.token_kind.clone(),
            stage: self.stage,
            may_consume: false,
            include_in_span: false, // Doesn't really matter since it won't be consumed anyway.
        }
    }
}

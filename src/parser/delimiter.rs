use crate::lexer::tokens::*;

use super::error::Stage;

pub struct Delimiter {
    token_kind: TokenKind,
    stage: Stage,
    required: bool,
}
impl Delimiter {
    /// Construct a delimiter for an expression surrounded by parentheses.
    pub fn parentheses() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::CloseParen),
            stage: Stage::ParenExprEnd,
            required: true,
        }
    }
    /// Construct a delimiter for an expression terminated by a newline.
    pub fn newline() -> Self {
        Self {
            token_kind: TokenKind::Structure(Structure::Newline),
            stage: Stage::StatementEnd,
            required: true,
        }
    }
    /// Construct a delimiter for an indexing sub-expression.
    pub fn index() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::CloseBracket),
            stage: Stage::IndexEnd,
            required: true,
        }
    }
    /// Construct a delimiter for an indexing sub-expression.
    pub fn function_call() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::CloseParen),
            stage: Stage::ParameterList,
            required: true,
        }
    }
    /// Construct a delimiter for an assignment target expression.
    pub fn assign() -> Self {
        Self {
            token_kind: TokenKind::Symbol(Symbol::Assign),
            stage: Stage::AssignTarget,
            required: true,
        }
    }
    /// Construct a delimiter for the middle expression of a ternary if-expression.
    pub fn ternary() -> Self {
        Self {
            token_kind: TokenKind::Keyword(Keyword::Else),
            stage: Stage::TernaryElse,
            required: true,
        }
    }

    pub fn stage(&self) -> Stage {
        self.stage
    }

    pub fn required(&self) -> bool {
        self.required
    }

    pub fn token_kind(&self) -> &TokenKind {
        &self.token_kind
    }

    /// Returns `true` if this delimiter expects a token of the same kind as the token that was
    /// provided.
    pub fn accepts_token(&self, token: &Token) -> bool {
        token.kind == self.token_kind
    }

    pub fn for_subexpr(&self) -> Self {
        Self {
            token_kind: self.token_kind.clone(),
            stage: self.stage,
            required: false,
        }
    }
}

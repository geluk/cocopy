use crate::lexer::tokens::*;

use super::combinator::*;
use super::fixity::Fixity;
use super::syntax_tree::{self, *};

/// Tries to recognise a pattern, returning the expression wrapped in an [`Option`]
/// if it succeeds, or [`None`] if it doesn't.
macro_rules! recognise {
    ($expression:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? => $result_expr:expr $(,)?) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Some($result_expr),
            _ => None
        }
    }
}

/// Tries to recognise a token from the provided parser, returning the token and modified parser
/// state wrapped in an [`Option`] if it matches, or [`None`] if it doesn't.
macro_rules! recognise_token {
    ($state_expr:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? => $result_expr:expr $(,)?) => {{
        expect_token($state_expr).and_then(|(token, state)| {
            recognise!(&token.kind, $( $pattern )|+ $( if $guard )? => (*$result_expr, state))
                .ok_or_else(|| Reason::UnexpectedToken(token))
        })
    }};
}

pub fn parse(token_stream: &[Token]) -> Result<Expr, ParseError> {
    parse_internal(token_stream)
}

/// An N-ary operator, with N > 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NAryOp {
    Binary(BinOp),
    Ternary(TerOp),
}

fn parse_internal(tokens: &[Token]) -> Result<Expr, ParseError> {
    let state = ParserState::new(tokens);

    let (expr, state) = expression(Fixity::none())(state)?;

    recognise_token!(state, TokenKind::Structure(s) if *s == Structure::Newline => s)
        .add_stage(Stage::ProgramEnd)
        .map(|(_, _)| expr)
}

fn expression(left_fix: Fixity) -> impl Fn(ParserState) -> ParseResult<Expr> {
    move |state| {
        let (tk, state) = annotate(Stage::ExprStart, token())(state)?;

        let (lhs, state) = match tk.kind {
            TokenKind::Identifier(id) => (Expr::Identifier(id), state),
            TokenKind::Literal(l) => (Expr::Literal(l.to_syntax_node()), state),
            TokenKind::Keyword(Keyword::True) => {
                (Expr::Literal(syntax_tree::Literal::Boolean(true)), state)
            }
            TokenKind::Keyword(Keyword::False) => {
                (Expr::Literal(syntax_tree::Literal::Boolean(false)), state)
            }
            TokenKind::Symbol(Symbol::OpenParen) => {
                let (lhs, state) = expression(Fixity::none())(state)?;
                let (_, state) =
                    expect_symbol(Symbol::CloseParen, state).add_stage(Stage::ParenExprEnd)?;
                (lhs, state)
            }
            TokenKind::Symbol(Symbol::Minus) => un_expr(UnOp::Negate, state)?,
            TokenKind::Keyword(Keyword::Not) => un_expr(UnOp::Not, state)?,
            _ => return failure(Stage::ExprStart, Reason::UnexpectedToken(tk)),
        };

        pratt_parse(lhs, left_fix.clone(), state)
    }
}

fn expression_start(token: Token) -> impl FnOnce(ParserState) -> ParseResult<Expr> {
    move |state| {
        let t = match token.kind {
            TokenKind::Identifier(id) => success_p(Expr::Identifier(id)),
            TokenKind::Literal(l) => success_p(Expr::Literal(l.to_syntax_node())),
            TokenKind::Keyword(Keyword::True) => {
                success_p(Expr::Literal(syntax_tree::Literal::Boolean(true)))
            }
            TokenKind::Keyword(Keyword::False) => {
                success_p(Expr::Literal(syntax_tree::Literal::Boolean(false)))
            }
            TokenKind::Symbol(Symbol::OpenParen) => {
                delimited(expression(Fixity::none()), move |state| {
                    expect_symbol(Symbol::CloseParen, state).add_stage(Stage::ParenExprEnd)
                })
            }
            TokenKind::Symbol(Symbol::Minus) => un_expr(UnOp::Negate, state)?,
            TokenKind::Keyword(Keyword::Not) => un_expr(UnOp::Not, state)?,
            _ => return failure(Stage::ExprStart, Reason::UnexpectedToken(token)),
        }(state)?;

        Ok(t)
    }
}

fn un_expr(op: UnOp, state: ParserState) -> ParseResult<Expr> {
    let fixity = Fixity::for_unop(op);
    let (rhs, state) = expression(fixity)(state)?;

    let un_expr = UnExpr { op, rhs };
    success(Expr::Unary(Box::new(un_expr)), state)
}

fn pratt_parse(mut lhs: Expr, prev_fix: Fixity, mut state: ParserState) -> ParseResult<Expr> {
    loop {
        // Look for the next operator that's coming up
        let op = match state.next() {
            None => return success(lhs, state),
            Some((token, _)) => {
                // Handle a valid binary operation
                if let Some(op) = as_n_ary_op(&token.kind) {
                    op
                }
                // Handle a valid end of expression
                // TODO: Lift this into a parameter specifying which token may end the expression.
                else if matches!(
                    token.kind,
                    TokenKind::Structure(Structure::Newline)
                        | TokenKind::Symbol(Symbol::CloseParen)
                        | TokenKind::Symbol(Symbol::CloseBracket)
                        | TokenKind::Keyword(Keyword::Else)
                ) {
                    return success(lhs, state);
                }
                // Anything else is an error
                else {
                    return failure(Stage::BinExprEnd, Reason::UnexpectedToken(token));
                }
            }
        };

        let cur_fix = Fixity::for_n_ary_op(op);
        if prev_fix.precedes_rhs(&cur_fix) {
            return success(lhs, state);
        }

        // Only advance the state once we're sure we'll use the operator.
        state = state.advance();

        match op {
            NAryOp::Binary(bin) => {
                let (rhs, next) = expression(cur_fix)(state)?;
                state = next;

                if bin == BinOp::Index {
                    let (_, next) =
                        expect_symbol(Symbol::CloseBracket, state).add_stage(Stage::IndexEnd)?;
                    state = next;
                }
                let bin_expr = BinExpr { lhs, op: bin, rhs };
                lhs = Expr::Binary(Box::new(bin_expr));
            }
            NAryOp::Ternary(TerOp::If) => {
                let (mhs, next) = expression(Fixity::none())(state)?;

                let (_, next) =
                    expect_keyword(Keyword::Else, next).add_stage(Stage::TernaryElse)?;

                let (rhs, next) = expression(cur_fix)(next)?;
                state = next;

                let ter_expr = TerExpr {
                    lhs,
                    op: TerOp::If,
                    mhs,
                    rhs,
                };
                lhs = Expr::Ternary(Box::new(ter_expr));
            }
        }
    }
}

fn as_n_ary_op(kind: &TokenKind) -> Option<NAryOp> {
    recognise!(kind, TokenKind::Symbol(s) => s)
        .and_then(|&s| {
            Some(match s {
                Symbol::Plus => BinOp::Add,
                Symbol::Minus => BinOp::Subtract,
                Symbol::Asterisk => BinOp::Multiply,
                Symbol::DoubleSlash => BinOp::IntDiv,
                Symbol::Percent => BinOp::Remainder,
                Symbol::Lt => BinOp::LessThan,
                Symbol::Gt => BinOp::GreaterThan,
                Symbol::Lte => BinOp::LessThanEqual,
                Symbol::Gte => BinOp::GreaterThanEqual,
                Symbol::Eq => BinOp::Equal,
                Symbol::Neq => BinOp::NotEqual,
                Symbol::OpenBracket => BinOp::Index,
                Symbol::Period => BinOp::MemberAccess,
                _ => return None,
            })
        })
        .map(NAryOp::Binary)
        .or_else(|| {
            recognise!(kind, TokenKind::Keyword(k) => k).and_then(|&k| {
                Some(match k {
                    Keyword::Or => NAryOp::Binary(BinOp::Or),
                    Keyword::And => NAryOp::Binary(BinOp::And),
                    Keyword::Is => NAryOp::Binary(BinOp::Is),
                    Keyword::If => NAryOp::Ternary(TerOp::If),
                    _ => return None,
                })
            })
        })
}

fn expect_symbol(symbol: Symbol, state: ParserState) -> Fallible<Symbol> {
    map(expect(|t| t.kind == TokenKind::Symbol(symbol)), |_| symbol)(state)
}

fn expect_keyword(keyword: Keyword, state: ParserState) -> Fallible<Keyword> {
    map(expect(|t| t.kind == TokenKind::Keyword(keyword)), |_| {
        keyword
    })(state)
}

#[cfg(test)]
mod tests {
    use crate::lexer::lex;

    use super::*;

    macro_rules! assert_parses {
        ($source:expr, $expected:expr) => {{
            let tokens = lex($source).unwrap();
            let expr = match parse(&tokens) {
                Ok(expr) => expr,
                Err(err) => {
                    panic!(
                        "\n\nEncountered error:\n{:#?}\nWhen parsing:\n    {}\n\n",
                        err, $source,
                    );
                }
            };
            let pretty = expr.to_string();

            assert_eq!(
                $expected, pretty,
                "\n\nWhen parsing:\n    {}\nExpected to find:\n    {}\nBut found:\n    {}\n\n",
                $source, $expected, pretty
            );

            println!(
                "\n\nSuccessfully parsed:\n    {}\nInto:\n    {}\n\n",
                $source, pretty
            )
        }};
    }

    fn assert_fails(source: &str, expected_stage: Stage) {
        let tokens = lex(source).unwrap();
        let error = parse(&tokens).unwrap_err();

        assert_eq!(expected_stage, error.stage());
    }

    #[test]
    fn simple_expression() {
        assert_parses!("1 + 1000", "(1 + 1000)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn or_is_left_associative() {
        assert_parses!("True or False or True", "((True or False) or True)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn and_is_left_associative() {
        assert_parses!("a and b and c", "((a and b) and c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn add_sub_are_left_associative() {
        assert_parses!("a + b + c", "((a + b) + c)");
        assert_parses!("a - b - c", "((a - b) - c)");
        assert_parses!("a + b - c", "((a + b) - c)");
        assert_parses!("a - b + c", "((a - b) + c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn mult_intdiv_rem_are_left_associative() {
        assert_parses!("a * b * c", "((a * b) * c)");
        assert_parses!("a // b // c", "((a // b) // c)");
        assert_parses!("a % b % c", "((a % b) % c)");

        assert_parses!("a * b % c", "((a * b) % c)");
        assert_parses!("a % b * c", "((a % b) * c)");

        assert_parses!("a // b % c", "((a // b) % c)");
        assert_parses!("a % b // c", "((a % b) // c)");

        assert_parses!("a // b * c", "((a // b) * c)");
        assert_parses!("a * b // c", "((a * b) // c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn member_access() {
        assert_parses!("a.b and c", "((a.b) and c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn index() {
        assert_parses!("a[b] and c", "(a[b] and c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn ternary_if() {
        assert_parses!(
            "a if b > 10 or True else c",
            "(a if ((b > 10) or True) else c)"
        );
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn unclosed_ternary_fails() {
        assert_fails("a if b > 10 or True", Stage::TernaryElse);
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn combined_operators() {
        assert_parses!("1 + 10 + 9", "((1 + 10) + 9)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn combined_unop() {
        assert_parses!("a + not 9999 and 9", "((a + (not 9999)) and 9)");
    }
}

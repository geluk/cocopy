use crate::lexer::tokens::{Keyword, Structure, Symbol, Token, TokenKind};

use super::{
    delimiter::*,
    error::*,
    fixity::{Fixity, NAryOp},
    parser_base::*,
    syntax_tree::*,
};

pub fn parse(token_stream: &[Token]) -> Result<Expr, ParseError> {
    Parser::new(token_stream).parse_internal()
}

/// An N-ary operator, with N > 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NAryOp {
    Binary(BinOp),
    Ternary(TerOp),
}

struct Delimiter {
    token_kind: TokenKind,
    stage: Stage,
    required: bool,
}
impl Delimiter {
    /// Construct a delimiter for an expression surrounded by parentheses.
    pub fn parentheses() -> Option<Self> {
        Some(Self {
            token_kind: TokenKind::Symbol(Symbol::CloseParen),
            stage: Stage::ParenExprEnd,
            required: true,
        })
    }
    /// Construct a delimiter for an expression terminated by a newline.
    pub fn newline() -> Option<Self> {
        Some(Self {
            token_kind: TokenKind::Structure(Structure::Newline),
            stage: Stage::ProgramEnd,
            required: true,
        })
    }
    /// Construct a delimiter for an indexing sub-expression.
    pub fn index() -> Option<Self> {
        Some(Self {
            token_kind: TokenKind::Symbol(Symbol::CloseBracket),
            stage: Stage::IndexEnd,
            required: true,
        })
    }
    /// Construct a delimiter for an indexing sub-expression.
    pub fn function_call() -> Option<Self> {
        Some(Self {
            token_kind: TokenKind::Symbol(Symbol::CloseParen),
            stage: Stage::CallEnd,
            required: true,
        })
    }
    /// Construct a delimiter for the middle expression of a ternary if-expression.
    pub fn ternary() -> Option<Self> {
        Some(Self {
            token_kind: TokenKind::Keyword(Keyword::Else),
            stage: Stage::TernaryElse,
            required: true,
        })
    }

    /// Returns `true` if this delimiter expects a token of the same kind as the token that was
    /// provided.
    pub fn accepts_token(&self, token: &Token) -> bool {
        token.kind == self.token_kind
    }
}

pub trait OptionalDelimiter {
    fn for_subexpr(&self) -> Self;
}

impl OptionalDelimiter for Option<Delimiter> {
    fn for_subexpr(&self) -> Self {
        self.as_ref().map(|delimiter| Delimiter {
            token_kind: delimiter.token_kind.clone(),
            stage: delimiter.stage,
            required: false,
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_internal(&mut self) -> Result<Expr, ParseError> {
        let expr = self.expression(Fixity::none(), Delimiter::newline())?;

        Ok(expr)
    }

    fn expression(
        &mut self,
        left_fix: Fixity,
        delimiter: Option<Delimiter>,
    ) -> Result<Expr, ParseError> {
        let token = self.next().add_stage(Stage::ExprStart)?;

        let lhs = match &token.kind {
            TokenKind::Identifier(id) => (Expr::Identifier(id.clone())),
            TokenKind::Literal(l) => (Expr::Literal(l.to_syntax_node())),
            TokenKind::Keyword(Keyword::True) => Expr::Literal(Literal::Boolean(true)),
            TokenKind::Keyword(Keyword::False) => Expr::Literal(Literal::Boolean(false)),
            TokenKind::Symbol(Symbol::OpenParen) => {
                self.expression(Fixity::none(), Delimiter::parentheses())?
            }
            TokenKind::Symbol(Symbol::Minus) => {
                self.un_expr(UnOp::Negate, delimiter.for_subexpr())?
            }
            TokenKind::Keyword(Keyword::Not) => self.un_expr(UnOp::Not, delimiter.for_subexpr())?,
            _ => return failure(Stage::ExprStart, Reason::UnexpectedToken(token.clone())),
        };

        let expr = self.pratt_parse(lhs, left_fix, delimiter.for_subexpr())?;

        self.satisfy_delimiter(delimiter)?;
        Ok(expr)
    }

    /// Parse a unary expression using the provided unary operator
    fn un_expr(&mut self, op: UnOp, delimiter: Option<Delimiter>) -> Result<Expr, ParseError> {
        let rhs = self.expression(Fixity::for_unop(op), delimiter)?;

        let un_expr = UnExpr { op, rhs };
        Ok(Expr::Unary(Box::new(un_expr)))
    }

    fn pratt_parse(
        &mut self,
        mut lhs: Expr,
        prev_fix: Fixity,
        delimiter: Option<Delimiter>,
    ) -> Result<Expr, ParseError> {
        loop {
            // Look for the next operator that's coming up
            let op = match self.peek() {
                None => return Ok(lhs),
                Some(token) => {
                    match (token.kind.as_n_ary_op(), &delimiter) {
                        // Process a valid binary operator
                        (Some(op), _) => op,
                        // Process a recognised expression delimiter
                        (_, Some(delim)) if delim.accepts_token(token) => return Ok(lhs),
                        // Anything else is an error
                        _ => {
                            let stage = delimiter.map(|d| d.stage).unwrap_or(Stage::BinExprEnd);
                            return failure(stage, Reason::UnexpectedToken(token.clone()));
                        }
                    }
                }
            };

            let cur_fix = Fixity::for_n_ary_op(op);
            if prev_fix.precedes_rhs(&cur_fix) {
                return Ok(lhs);
            }

            // Only advance the parser once we're sure we'll use the operator.
            self.next().unwrap();

            match op {
                NAryOp::Binary(bin) => {
                    let rhs = if bin == BinOp::Index {
                        self.expression(cur_fix, Delimiter::index())?
                    } else if bin == BinOp::FunctionCall {
                        self.expression(cur_fix, Delimiter::function_call())?
                    } else {
                        self.expression(cur_fix, delimiter.for_subexpr())?
                    };
                    let bin_expr = BinExpr { lhs, op: bin, rhs };
                    lhs = Expr::Binary(Box::new(bin_expr));
                }
                NAryOp::Ternary(TerOp::If) => {
                    let mhs = self.expression(Fixity::none(), Delimiter::ternary())?;

                    let rhs = self.expression(cur_fix, delimiter.for_subexpr())?;

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

    fn satisfy_delimiter(&mut self, delimiter: Option<Delimiter>) -> Result<(), ParseError> {
        match delimiter {
            Some(delim) if delim.required => self
                .expect(delim.token_kind)
                .add_stage(delim.stage)
                .map(|_| ()),
            _ => Ok(()),
        }
    }
}

impl TokenKind {
    fn as_n_ary_op(&self) -> Option<NAryOp> {
        let op = match self {
            TokenKind::Keyword(Keyword::If) => NAryOp::Ternary(TerOp::If),
            TokenKind::Keyword(kw) => NAryOp::Binary(match kw {
                Keyword::Or => BinOp::Or,
                Keyword::And => BinOp::And,
                Keyword::Is => BinOp::Is,
                _ => return None,
            }),
            TokenKind::Symbol(s) => NAryOp::Binary(match s {
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
                Symbol::OpenParen => BinOp::FunctionCall,
                Symbol::Period => BinOp::MemberAccess,
                _ => return None,
            }),
            _ => return None,
        };
        Some(op)
    }
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

        assert_eq!(
            expected_stage,
            error.stage(),
            "Expected an error in {:?}, but found {:?}",
            expected_stage,
            error.stage()
        );
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
    fn nested_ternary_is_right_associative() {
        assert_parses!(
            "a if p1 else b if p2 else c",
            "(a if p1 else (b if p2 else c))"
        );
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn unclosed_binary_fails() {
        assert_fails("a or", Stage::ExprStart);
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

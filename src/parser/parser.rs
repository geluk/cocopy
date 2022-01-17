use crate::{
    lexer::tokens::{Keyword, Structure, Symbol, Token, TokenKind},
    span::Bytes,
};

use super::{
    delimiter::*,
    error::*,
    fixity::{Fixity, NAryOp},
    parser_base::*,
    syntax_tree::*,
};

pub fn parse(token_stream: &[Token]) -> Result<Program, ParseError> {
    Parser::new(token_stream).parse()
}

impl<'a> Parser<'a> {
    fn parse(&mut self) -> Result<Program, ParseError> {
        self.program()
    }

    /// Parse a complete program (a single ChocoPy file).
    fn program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        // TODO: Be smarter about this. If parsing a var_def fails,
        // it will try to parse a statement instead.
        while let Some(var_def) = self.recognise_parser(Self::var_def) {
            program.add_var_def(var_def);
        }

        while self.has_next() {
            let stmt = self.statement()?;
            program.add_statement(stmt);
        }

        Ok(program)
    }

    /// Parse a variable definition. Variable definitions are only allowed at the top of the file,
    /// and they may only be initialised with a literal.
    fn var_def(&mut self) -> Result<VarDef, ParseError> {
        let start = self.position();
        const ST: Stage = Stage::VarDef;
        let name = self.recognise_identifier().add_stage(ST)?.clone();

        self.recognise_symbol(Symbol::Colon).add_stage(ST)?;
        let type_spec = self.type_specification()?;
        self.recognise_symbol(Symbol::Assign).add_stage(ST)?;

        let next = self.next().add_stage(ST)?;
        let value = match &next.kind {
            TokenKind::Literal(l) => l.to_syntax_node(),
            TokenKind::Keyword(Keyword::True) => Literal::Boolean(true),
            TokenKind::Keyword(Keyword::False) => Literal::Boolean(false),
            _ => return failure(Stage::VarDef, Reason::UnexpectedToken(next.clone())),
        };
        self.recognise_structure(Structure::Newline).add_stage(ST)?;

        Ok(VarDef {
            name,
            type_spec,
            value,
            span: self.span_from(start),
        })
    }

    /// Parse a statement. A statement could be an expression evaluation, a variable assignment,
    /// control flow, or a `pass` or `return` statement.
    fn statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.position();
        // If we encounter a `pass` keyword, we can immediately emit it and finish the statement.
        if self.recognise_keyword(Keyword::Pass).is_ok() {
            self.recognise_structure(Structure::Newline)
                .add_stage(Stage::Statement)?;
            return Ok(Statement {
                stmt_type: StmtKind::Pass,
                span: self.span_from(start),
            });
        }
        // The same applies if we encounter a `return` keyword.
        if let Ok(_) = self.recognise_keyword(Keyword::Return) {
            let return_type = if self.recognise_structure(Structure::Newline).is_ok() {
                None
            } else {
                Some(self.toplevel_expression()?)
            };
            return Ok(Statement {
                stmt_type: StmtKind::Return(return_type),
                span: self.span_from(start),
            });
        }
        // TODO: The same can be said for `if`, `while`, and `for`.

        // At this point, we have two options left. We could either encounter an assignment,
        // or we could encounter an expression evaluation. There is no easy way to find out which
        // one we need to parse without significant lookahead, so we'll try both parsers and return
        // the result of the one that parsed the most.
        let stmt_type = self.alt(
            |p| Self::assign_statement(p).map(StmtKind::Assign),
            |p| Self::toplevel_expression(p).map(StmtKind::Evaluate),
        )?;
        Ok(Statement {
            stmt_type,
            span: self.span_from(start),
        })
    }

    /// Parse a variable assignment statement.
    fn assign_statement(&mut self) -> Result<Assign, ParseError> {
        let start = self.position();
        let target = self.expression(Fixity::none(), Delimiter::assign())?;
        let value = self.toplevel_expression()?;
        Ok(Assign {
            target,
            value,
            span: self.span_from(start),
        })
    }

    fn type_specification(&mut self) -> Result<TypeSpec, ParseError> {
        if self.recognise_symbol(Symbol::OpenBracket).is_ok() {
            let inner_array = self.type_specification()?;

            self.recognise_symbol(Symbol::CloseBracket)
                .add_stage(Stage::TypeSpec)?;

            Ok(TypeSpec::Array(Box::new(inner_array)))
        } else {
            let type_name = self.recognise_identifier().add_stage(Stage::TypeSpec)?;
            let type_spec = type_name.parse().add_stage(Stage::TypeSpec)?;
            Ok(type_spec)
        }
    }

    fn toplevel_expression(&mut self) -> Result<Expr, ParseError> {
        self.expression(Fixity::none(), Delimiter::newline())
    }

    fn expression(&mut self, left_fix: Fixity, delimiter: Delimiter) -> Result<Expr, ParseError> {
        let start = self.position();
        // This clone seems to be necessary because of the mutable borrow of &self.
        let token = self.next().add_stage(Stage::Expr)?.clone();

        let make = |expr_type: ExprKind| Expr::new(expr_type, self.span_from(start));

        let lhs = match &token.kind {
            TokenKind::Identifier(id) => make(ExprKind::Identifier(id.clone())),
            TokenKind::Literal(l) => make(ExprKind::Literal(l.to_syntax_node())),
            TokenKind::Keyword(Keyword::True) => make(ExprKind::Literal(Literal::Boolean(true))),
            TokenKind::Keyword(Keyword::False) => make(ExprKind::Literal(Literal::Boolean(false))),
            TokenKind::Keyword(Keyword::None) => make(ExprKind::Literal(Literal::None)),
            TokenKind::Symbol(Symbol::OpenParen) => {
                self.expression(Fixity::none(), Delimiter::parentheses())?
            }
            TokenKind::Symbol(Symbol::Minus) => {
                self.un_expr(UnOp::Negate, delimiter.for_subexpr(), start)?
            }
            TokenKind::Keyword(Keyword::Not) => {
                self.un_expr(UnOp::Not, delimiter.for_subexpr(), start)?
            }
            _ => return failure(Stage::Expr, Reason::UnexpectedToken(token)),
        };

        let expr_type = self.pratt_parse(lhs, left_fix, delimiter.for_subexpr(), start)?;

        self.satisfy_delimiter(delimiter)?;
        Ok(Expr {
            expr_type,
            span: self.span_from(start),
        })
    }

    /// Parse a unary expression using the provided unary operator
    fn un_expr(
        &mut self,
        op: UnOp,
        delimiter: Delimiter,
        start: Bytes,
    ) -> Result<Expr, ParseError> {
        let rhs = self.expression(Fixity::for_unop(op), delimiter)?;

        let un_expr = UnExpr { op, rhs };
        Ok(Expr::new(
            ExprKind::Unary(Box::new(un_expr)),
            self.span_from(start),
        ))
    }

    fn pratt_parse(
        &mut self,
        mut lhs: Expr,
        prev_fix: Fixity,
        delimiter: Delimiter,
        start: Bytes,
    ) -> Result<ExprKind, ParseError> {
        loop {
            // Look for the next operator that's coming up
            let op = match self.peek() {
                // TODO: Check if we should produce the entire expression instead
                None => return Ok(lhs.expr_type),
                Some(token) => {
                    match (token.kind.as_n_ary_op(), &delimiter) {
                        // Process a valid binary operator
                        (Some(op), _) => op,
                        // Process a recognised expression delimiter
                        (_, delim) if delim.accepts_token(token) => return Ok(lhs.expr_type),
                        // Anything else is an error
                        _ => {
                            return failure(
                                delimiter.stage(),
                                Reason::UnexpectedToken(token.clone()),
                            );
                        }
                    }
                }
            };

            let cur_fix = Fixity::for_n_ary_op(op);
            if prev_fix.precedes_rhs(&cur_fix) {
                return Ok(lhs.expr_type);
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
                    lhs = Expr::new(ExprKind::Binary(Box::new(bin_expr)), self.span_from(start));
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
                    lhs = Expr::new(ExprKind::Ternary(Box::new(ter_expr)), self.span_from(start));
                }
            }
        }
    }

    fn satisfy_delimiter(&mut self, delimiter: Delimiter) -> Result<(), ParseError> {
        if delimiter.required() {
            self.recognise(delimiter.token_kind().clone())
                .add_stage(delimiter.stage())?;
        }
        Ok(())
    }
}

impl TokenKind {
    fn as_n_ary_op(&self) -> Option<NAryOp> {
        use Symbol::*;
        let op = match self {
            TokenKind::Keyword(Keyword::If) => NAryOp::Ternary(TerOp::If),
            TokenKind::Keyword(kw) => NAryOp::Binary(match kw {
                Keyword::Or => BinOp::Or,
                Keyword::And => BinOp::And,
                Keyword::Is => BinOp::Is,
                _ => return None,
            }),
            TokenKind::Symbol(s) => NAryOp::Binary(match s {
                Plus => BinOp::Add,
                Minus => BinOp::Subtract,
                Asterisk => BinOp::Multiply,
                DoubleSlash => BinOp::IntDiv,
                Percent => BinOp::Remainder,
                Lt => BinOp::LessThan,
                Gt => BinOp::GreaterThan,
                Lte => BinOp::LessThanEqual,
                Gte => BinOp::GreaterThanEqual,
                Eq => BinOp::Equal,
                Neq => BinOp::NotEqual,
                OpenBracket => BinOp::Index,
                OpenParen => BinOp::FunctionCall,
                Period => BinOp::MemberAccess,
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

    macro_rules! assert_parses_with {
        ($target:ident, $source:expr, $expected:expr) => {{
            let tokens = lex($source).unwrap();
            let expr = match Parser::new(&tokens).$target() {
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

    macro_rules! assert_expr_parses {
        ($source:expr, $expected:expr) => {{
            assert_parses_with!(toplevel_expression, $source, $expected)
        }};
    }

    macro_rules! assert_fails_with {
        ($target:ident, $source:expr, $expected_stage:expr) => {{
            let tokens = lex($source).unwrap();
            let error = Parser::new(&tokens).$target().unwrap_err();

            assert_eq!(
                $expected_stage,
                error.stage(),
                "Expected error stage to be {:?}, but was {:?}",
                $expected_stage,
                error.stage()
            );
        }};
    }

    macro_rules! assert_expr_fails {
        ($source:expr, $expected:expr) => {{
            assert_fails_with!(toplevel_expression, $source, $expected)
        }};
    }

    // =============
    // # EXPRESSIONS
    // =============

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn simple_expression() {
        assert_expr_parses!("1 + 1000", "(1 + 1000)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn or_is_left_associative() {
        assert_expr_parses!("True or False or True", "((True or False) or True)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn and_is_left_associative() {
        assert_expr_parses!("a and b and c", "((a and b) and c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn add_sub_are_left_associative() {
        assert_expr_parses!("a + b + c", "((a + b) + c)");
        assert_expr_parses!("a - b - c", "((a - b) - c)");
        assert_expr_parses!("a + b - c", "((a + b) - c)");
        assert_expr_parses!("a - b + c", "((a - b) + c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn mult_intdiv_rem_are_left_associative() {
        assert_expr_parses!("a * b * c", "((a * b) * c)");
        assert_expr_parses!("a // b // c", "((a // b) // c)");
        assert_expr_parses!("a % b % c", "((a % b) % c)");

        assert_expr_parses!("a * b % c", "((a * b) % c)");
        assert_expr_parses!("a % b * c", "((a % b) * c)");

        assert_expr_parses!("a // b % c", "((a // b) % c)");
        assert_expr_parses!("a % b // c", "((a % b) // c)");

        assert_expr_parses!("a // b * c", "((a // b) * c)");
        assert_expr_parses!("a * b // c", "((a * b) // c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn member_access() {
        assert_expr_parses!("a.b and c", "((a.b) and c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn index() {
        assert_expr_parses!("a[b] and c", "((a[b]) and c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn index_and_member_access_are_left_associative() {
        assert_expr_parses!("a.b.c", "((a.b).c)");
        assert_expr_parses!("a[b][c]", "((a[b])[c])");
        assert_expr_parses!("a.b[c]", "((a.b)[c])");
        assert_expr_parses!("a[b].c", "((a[b]).c)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn ternary_if() {
        assert_expr_parses!(
            "a if b > 10 or True else c",
            "(a if ((b > 10) or True) else c)"
        );
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn ternary_if_is_right_associative() {
        assert_expr_parses!(
            "a if p1 else b if p2 else c",
            "(a if p1 else (b if p2 else c))"
        );
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn unclosed_ternary_fails() {
        assert_expr_fails!("a if b > 10 or True", Stage::TernaryElse);
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn combined_unop() {
        assert_expr_parses!("a + not 9999 and 9", "((a + (not 9999)) and 9)");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn unclosed_binary_fails() {
        assert_expr_fails!("a or", Stage::Expr);
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn combined_operators() {
        assert_expr_parses!("1 + 10 + 9", "((1 + 10) + 9)");
    }

    // =============
    // # DEFINITIONS
    // =============

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn var_def() {
        assert_parses_with!(var_def, "a:int=10", "a : int = 10");
    }

    // ============
    // # STATEMENTS
    // ============

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn return_statement() {
        assert_parses_with!(statement, "return", "return\n");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn return_value_statement() {
        assert_parses_with!(statement, "return 10", "return 10\n");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn pass_statement() {
        assert_parses_with!(statement, "pass", "pass\n");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn assign_statement() {
        assert_parses_with!(
            statement,
            "a = 10 if hello[99].c else something(10)",
            "a = (10 if ((hello[99]).c) else (something(10)))\n"
        );
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn evaluation_statement() {
        assert_parses_with!(statement, "a.b.c(10)", "(((a.b).c)(10))\n");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn invalid_assign_statement() {
        assert_fails_with!(statement, "a = 10 or", Stage::Expr);
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn invalid_evaluation_statement() {
        assert_fails_with!(statement, "a + 10 +", Stage::Expr);
    }
}

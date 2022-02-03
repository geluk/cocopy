//! High-level parsing functions for building an AST.
use crate::{
    ast::{untyped::*, TypeSpec},
    lexer::tokens::{Keyword, Structure, Symbol, Token, TokenKind},
    span::Bytes,
};

use super::{
    delimiter::*,
    error::*,
    fixity::{Fixity, NAryOp},
    parser_base::*,
};

/// Parse a token stream into a syntax tree.
pub fn parse(token_stream: &[Token]) -> Result<Program, ParseError> {
    Parser::new(token_stream).program()
}

impl<'a> Parser<'a> {
    /// Parse a complete program (a single ChocoPy file).
    fn program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        while self.has_next() {
            // Keep parsing variable definitions...
            if let Ok(var_def) = self.recognise_parser(Self::var_def) {
                program.add_var_def(var_def);
            }
            // ... until we encounter an error. Then try a statement...
            else if let Ok(stmt) = self.recognise_parser(Self::statement) {
                // If that succeeds, add it. From now on, recognise only statements.
                program.add_statement(stmt);
                break;
            }
            // ... and if that doesn't work either, return the longest parse error.
            else {
                self.alt(
                    |p1| p1.var_def().map(|_| ()),
                    |p2| p2.statement().map(|_| ()),
                )?;
                unreachable!("Compiler state borxed! (failed to reproduce parse error)")
            }
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
        let (name, _) = self.recognise_identifier().add_stage(ST)?;

        self.recognise_symbol(Symbol::Colon).add_stage(ST)?;
        let type_spec = self.type_specification()?;
        self.recognise_symbol(Symbol::Assign).add_stage(ST)?;

        let next = self.next().add_stage(ST)?;
        let value = Self::as_literal(next)
            .ok_or_else(|| ParseError::new(ST, Reason::UnexpectedToken(next.clone())))?;

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
                stmt_kind: StmtKind::Pass,
                span: self.span_from(start),
            });
        }
        // The same applies if we encounter a `return` keyword.
        if self.recognise_keyword(Keyword::Return).is_ok() {
            let return_type = if self.recognise_structure(Structure::Newline).is_ok() {
                None
            } else {
                Some(self.toplevel_expression()?)
            };
            return Ok(Statement {
                stmt_kind: StmtKind::Return(return_type),
                span: self.span_from(start),
            });
        }
        if let Some(TokenKind::Keyword(Keyword::If)) = self.peek_kind() {
            return self.if_statement();
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
            stmt_kind: stmt_type,
            span: self.span_from(start),
        })
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.position();
        const ST: Stage = Stage::IfStatement;
        self.recognise_keyword(Keyword::If).add_stage(ST)?;

        let condition = self.expression(Fixity::none(), Delimiter::condition())?;
        self.recognise_structure(Structure::Newline).add_stage(ST)?;

        let body = self.block()?;

        // TODO: elif
        let else_body = if self.recognise_keyword(Keyword::Else).is_ok() {
            self.recognise_symbol(Symbol::Colon).add_stage(ST)?;
            self.recognise_structure(Structure::Newline).add_stage(ST)?;

            Some(self.block()?)
        } else {
            None
        };

        let if_st = If {
            condition,
            body,
            elifs: vec![],
            else_body,
        };

        Ok(Statement {
            span: self.span_from(start),
            stmt_kind: StmtKind::If(if_st),
        })
    }

    fn block(&mut self) -> Result<Block, ParseError> {
        let start = self.position();
        self.recognise_structure(Structure::Indent)
            .add_stage(Stage::Block)?;
        let mut body = Vec::new();
        loop {
            match self.recognise_parser(Self::statement) {
                Ok(st) => body.push(st),
                Err(err) => {
                    if self.recognise_structure(Structure::Dedent).is_ok() {
                        break;
                    } else {
                        return Err(err);
                    }
                }
            }
        }
        Ok(Block::new(body, self.span_from(start)))
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

    /// Parse a type specification.
    fn type_specification(&mut self) -> Result<TypeSpec, ParseError> {
        if self.recognise_symbol(Symbol::OpenBracket).is_ok() {
            let inner_type = self.type_specification()?;

            self.recognise_symbol(Symbol::CloseBracket)
                .add_stage(Stage::TypeSpec)?;

            Ok(TypeSpec::Array(Box::new(inner_type)))
        } else {
            let (type_name, token) = self.recognise_identifier().add_stage(Stage::TypeSpec)?;
            type_name.parse().map_err(|_| {
                ParseError::new(Stage::TypeSpec, Reason::UnknownType(type_name, token))
            })
        }
    }

    /// Parse a top-level expression, delimited by a newline.
    fn toplevel_expression(&mut self) -> Result<Expr, ParseError> {
        self.expression(Fixity::none(), Delimiter::newline())
    }

    /// Parse an expression, delimited by the given delimiter.
    fn expression(&mut self, left_fix: Fixity, delimiter: Delimiter) -> Result<Expr, ParseError> {
        let start = self.position();
        self.expression_from(start, left_fix, delimiter)
    }

    /// Parse an expression, supplying a custom start position to be used when annotating the
    /// range of the expression.
    fn expression_from(
        &mut self,
        start: Bytes,
        left_fix: Fixity,
        delimiter: Delimiter,
    ) -> Result<Expr, ParseError> {
        let token = self.next().add_stage(Stage::Expr)?;

        let lhs = if let Some(lit) = Self::as_literal(token) {
            Expr::new(ExprKind::Literal(lit), self.span_from(start))
        } else {
            match &token.kind {
                TokenKind::Identifier(id) => {
                    Expr::new(ExprKind::Identifier(id.clone()), self.span_from(start))
                }
                TokenKind::Symbol(Symbol::OpenParen) => {
                    self.expression_from(start, Fixity::none(), Delimiter::parentheses())?
                }
                TokenKind::Symbol(Symbol::Minus) => {
                    self.un_expr(UnOp::Negate, delimiter.for_subexpr(), start)?
                }
                TokenKind::Keyword(Keyword::Not) => {
                    self.un_expr(UnOp::Not, delimiter.for_subexpr(), start)?
                }
                _ => return failure(Stage::Expr, Reason::UnexpectedToken(token.clone())),
            }
        };

        let expr_kind = self.pratt_parse(lhs, left_fix, delimiter.for_subexpr(), start)?;

        let mut span = self.span_from(start);
        self.satisfy_delimiter(&delimiter)?;
        if delimiter.include_in_span() {
            span = self.span_from(start);
        }

        Ok(Expr { expr_kind, span })
    }

    /// Parse a unary expression using the provided unary operator.
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

    /// Keep grouping stronger operators until a weaker operator is found, then return.
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
                None => return Ok(lhs.expr_kind),
                Some(token) => {
                    match (token.kind.as_n_ary_op(), &delimiter) {
                        // Process a valid n-ary operation
                        (Some(op), _) => op,
                        // Process a recognised expression delimiter
                        (_, delim) if delim.is_satisfied_by(token) => return Ok(lhs.expr_kind),
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
                return Ok(lhs.expr_kind);
            }

            // Only advance the parser once we're sure we'll use the operator.
            self.next().unwrap();

            let kind = match op {
                NAryOp::Member => self.parse_member_expr(lhs)?,
                NAryOp::Index => self.parse_index_expr(lhs)?,
                NAryOp::FunctionCall => self.parse_function_call(lhs)?,
                NAryOp::Binary(bin) => {
                    let rhs = self.expression(cur_fix, delimiter.for_subexpr())?;
                    let bin_expr = BinExpr { lhs, op: bin, rhs };
                    ExprKind::Binary(Box::new(bin_expr))
                }
                NAryOp::Ternary(TerOp::If) => {
                    let mhs = self.expression(Fixity::none(), Delimiter::ternary_if())?;

                    let rhs = self.expression(cur_fix, delimiter.for_subexpr())?;

                    let ter_expr = TerExpr {
                        lhs,
                        op: TerOp::If,
                        mhs,
                        rhs,
                    };
                    ExprKind::Ternary(Box::new(ter_expr))
                }
            };
            lhs = Expr::new(kind, self.span_from(start));
        }
    }

    fn parse_member_expr(&mut self, lhs: Expr) -> Result<ExprKind, ParseError> {
        let (rhs, _) = self.recognise_identifier().add_stage(Stage::MemberExpr)?;
        let expr = MemberExpr { lhs, rhs };
        Ok(ExprKind::Member(Box::new(expr)))
    }

    fn parse_index_expr(&mut self, lhs: Expr) -> Result<ExprKind, ParseError> {
        let rhs = self.expression(Fixity::none(), Delimiter::index())?;
        let expr = IndexExpr { lhs, rhs };
        Ok(ExprKind::Index(Box::new(expr)))
    }

    fn parse_function_call(&mut self, lhs: Expr) -> Result<ExprKind, ParseError> {
        Ok(match lhs.expr_kind {
            ExprKind::Identifier(name) => {
                // TODO: Parse parameter list instead
                let params = self.expression(Fixity::none(), Delimiter::function_call())?;
                let call = FunCallExpr {
                    name,
                    name_span: lhs.span,
                    params,
                };
                ExprKind::FunctionCall(Box::new(call))
            }
            ExprKind::Member(member) => {
                // TODO: Parse parameter list instead
                let params = self.expression(Fixity::none(), Delimiter::function_call())?;
                let call = MetCallExpr {
                    member: *member,
                    params,
                };
                ExprKind::MethodCall(Box::new(call))
            }
            _ => return failure(Stage::Call, Reason::NotCallable(lhs.span)),
        })
    }

    fn as_literal(token: &Token) -> Option<Literal> {
        Some(match &token.kind {
            TokenKind::Literal(l) => l.to_syntax_node(),
            TokenKind::Keyword(Keyword::True) => Literal::Boolean(true),
            TokenKind::Keyword(Keyword::False) => Literal::Boolean(false),
            TokenKind::Keyword(Keyword::None) => Literal::None,
            _ => return None,
        })
    }

    fn satisfy_delimiter(&mut self, delimiter: &Delimiter) -> Result<(), ParseError> {
        if delimiter.may_consume() {
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
            TokenKind::Symbol(Symbol::Period) => NAryOp::Member,
            TokenKind::Symbol(Symbol::OpenBracket) => NAryOp::Index,
            TokenKind::Symbol(Symbol::OpenParen) => NAryOp::FunctionCall,
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
    fn function() {
        assert_expr_parses!("a(b)", "(a(b))");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn function_may_have_expression_as_parameter() {
        assert_expr_parses!("a(1 + 1)", "(a((1 + 1)))");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn method() {
        assert_expr_parses!("a.b(b)", "((a.b)(b))");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn complex_method() {
        assert_expr_parses!("(a if True else b).c(d)", "(((a if True else b).c)(d))");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn call_on_uncallable_type_fails() {
        assert_expr_fails!("(a if True else b)(d)", Stage::Call);
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn call_on_int_fails() {
        assert_expr_fails!("10(True)", Stage::Call);
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
    fn var_def_int() {
        assert_parses_with!(var_def, "a:int=10", "a : int = 10");
    }

    /// ChocoPy Language Reference: 4.1
    #[test]
    fn var_def_bool() {
        assert_parses_with!(var_def, "a : bool = 10", "a : bool = 10");
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

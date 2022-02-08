use std::collections::hash_map::Entry;

use crate::{
    ast::{typed, typed::Environment, untyped, untyped::*, TypeSpec},
    builtins,
    ext::Sequence,
};

use super::{bin_op_checker::BinOpChecker, error::*};

pub fn verify_well_typed(program: untyped::Program) -> Result<typed::Program, Vec<TypeError>> {
    TypeChecker::run(program)
}

struct TypeChecker {
    program: typed::Program,
}
impl TypeChecker {
    fn new() -> Self {
        Self {
            program: typed::Program::new(),
        }
    }

    fn run(program: untyped::Program) -> Result<typed::Program, Vec<TypeError>> {
        let mut checker = Self::new();
        let type_errors = checker.check(program);
        if type_errors.is_empty() {
            Ok(checker.program)
        } else {
            Err(type_errors)
        }
    }

    fn check(&mut self, program: untyped::Program) -> Vec<TypeError> {
        let mut type_errors = self.assign_var_defs(program.var_defs);
        type_errors.append(&mut self.assign_func_defs(program.func_defs));
        type_errors.append(&mut self.check_statements(program.statements));
        type_errors
    }

    /// Check variable definitions and write them to the global environment.
    fn assign_var_defs(&mut self, var_defs: Vec<VarDef>) -> Vec<TypeError> {
        let mut type_errors = vec![];
        for var_def in var_defs {
            if let Err(kind) = self
                .program
                .set_type(var_def.name.clone(), var_def.type_spec.clone())
            {
                type_errors.push(TypeError::new(kind, var_def.span));
            }

            let lit_type = self.check_literal(&var_def.value);

            if let Err(kind) = Self::check_assignment(&var_def.type_spec, &lit_type) {
                type_errors.push(TypeError::new(kind, var_def.span))
            }

            self.program.var_defs.push(var_def.into_typed());
        }

        type_errors
    }

    /// Check function definitions and write them to the global environment.
    fn assign_func_defs(&mut self, func_defs: Vec<FuncDef>) -> Vec<TypeError> {
        let mut type_errors = vec![];
        for func_def in func_defs {
            match self.assign_func_def(func_def) {
                Ok(func_def) => {
                    self.program.func_defs.push(func_def);
                }
                Err(mut errors) => type_errors.append(&mut errors),
            }
        }
        type_errors
    }

    /// Check a function definition and write it to the global environment.
    fn assign_func_def(&mut self, func_def: FuncDef) -> Result<typed::FuncDef, Vec<TypeError>> {
        let mut type_errors = vec![];
        let mut param_types = vec![];
        for param in func_def.parameters.iter() {
            param_types.push(param.type_spec.clone());

            if let Err(err) = self
                .program
                .set_type(param.name.clone(), param.type_spec.clone())
                .add_span(param.span)
            {
                type_errors.push(err)
            }
        }

        let func_type = TypeSpec::Function(param_types, Box::new(func_def.return_type.clone()));
        if let Err(kind) = self.program.set_type(func_def.name.clone(), func_type) {
            type_errors.push(TypeError::new(kind, func_def.decl_span));
        }

        match self.check_block(func_def.body) {
            Ok(body) if type_errors.is_empty() => Ok({
                typed::FuncDef {
                    name: func_def.name,
                    return_type: func_def.return_type,
                    parameters: func_def.parameters,
                    decl_span: func_def.decl_span,
                    body,
                    span: func_def.span,
                }
            }),
            Ok(_) => Err(type_errors),
            Err(mut errors) => {
                type_errors.append(&mut errors);
                Err(type_errors)
            }
        }
    }

    /// Verify that all statements are well-typed.
    fn check_statements(&mut self, statements: Vec<Statement>) -> Vec<TypeError> {
        let mut all_errors = vec![];
        for statement in statements {
            match self.check_statement(statement) {
                Ok(stmt) => {
                    self.program.statements.push(stmt);
                }
                Err(mut errors) => all_errors.append(&mut errors),
            }
        }
        all_errors
    }

    /// Verify that a statement is well-typed.
    fn check_statement(
        &mut self,
        statement: Statement,
    ) -> Result<typed::Statement, Vec<TypeError>> {
        let stmt_kind = match statement.stmt_kind {
            StmtKind::Pass => typed::StmtKind::Pass,
            StmtKind::Evaluate(expr) => typed::StmtKind::Evaluate(self.check_expression(expr)?),
            StmtKind::Return(_) => todo!("return statements are not supported yet"),
            StmtKind::Assign(assign) => typed::StmtKind::Assign(self.check_assign_stmt(assign)?),
            StmtKind::If(if_stmt) => typed::StmtKind::If(self.check_if(if_stmt)?),
        };
        Ok(typed::Statement {
            stmt_kind,
            span: statement.span,
        })
    }

    fn check_if(&mut self, if_stmt: If) -> Result<typed::If, Vec<TypeError>> {
        let condition_result = self.check_expression(if_stmt.condition).and_then(|expr| {
            if expr.type_spec != TypeSpec::Bool {
                singleton_error(TypeErrorKind::IfCondition(expr.type_spec), expr.span)
            } else {
                Ok(expr)
            }
        });

        let ((condition, body), else_body) = condition_result
            .concat_result(self.check_block(if_stmt.body))
            .concat_result(if_stmt.else_body.map(|b| self.check_block(b)).sequence())?;

        Ok(typed::If {
            condition,
            body,
            elifs: vec![],
            else_body,
        })
    }

    fn check_block(&mut self, block: Block) -> Result<typed::Block, Vec<TypeError>> {
        let statements: Vec<_> = block
            .statements
            .into_iter()
            .flat_map(|stmt| self.check_statement(stmt))
            .collect();

        Ok(typed::Block {
            statements,
            span: block.span,
        })
    }

    /// Verify that assignment to a variable is well-typed.
    fn check_assign_stmt(&mut self, assign: Assign) -> Result<typed::Assign, Vec<TypeError>> {
        let value = self.check_expression(assign.value)?;

        match assign.target.expr_kind {
            ExprKind::Identifier(id) => {
                let target_type = self.program.lookup(&id).add_span(assign.span)?;

                Self::check_assignment(&target_type, &value.type_spec).add_span(value.span)?;

                Ok(typed::Assign {
                    span: assign.span,
                    target: typed::Expr {
                        expr_kind: typed::ExprKind::Identifier(id),
                        type_spec: target_type,
                        span: assign.target.span,
                    },
                    value,
                })
            }
            k => todo!("Cannot check assignment to {}", k),
        }
    }

    /// Verify that an expression is well-typed.
    fn check_expression(&mut self, expression: Expr) -> Result<typed::Expr, Vec<TypeError>> {
        let span = expression.span;
        let (expr_kind, type_spec) = match expression.expr_kind {
            ExprKind::Literal(l) => {
                let type_spec = self.check_literal(&l);
                (typed::ExprKind::Literal(l), type_spec)
            }
            ExprKind::Identifier(id) => {
                let type_spec = self.program.lookup(&id).add_span(span)?;
                (typed::ExprKind::Identifier(id), type_spec)
            }
            ExprKind::Member(_) => todo!("Type check member expressions"),
            ExprKind::Index(_) => todo!("Type check index expressions"),
            ExprKind::FunctionCall(call) => {
                let call = self.check_function_call(*call)?;
                let type_spec = call.type_spec.clone();
                (typed::ExprKind::FunctionCall(Box::new(call)), type_spec)
            }
            ExprKind::MethodCall(_) => todo!("Type check method call expressions"),
            ExprKind::Unary(un) => {
                let un = self.check_unary(*un)?;
                let type_spec = un.type_spec.clone();
                (typed::ExprKind::Unary(Box::new(un)), type_spec)
            }
            ExprKind::Binary(bin) => {
                let bin = self.check_binary(*bin)?;
                let type_spec = bin.type_spec.clone();
                (typed::ExprKind::Binary(Box::new(bin)), type_spec)
            }
            ExprKind::Ternary(ter) => {
                let ter = self.check_ternary(*ter)?;
                let type_spec = ter.type_spec.clone();
                (typed::ExprKind::Ternary(Box::new(ter)), type_spec)
            }
        };

        Ok(typed::Expr {
            span,
            expr_kind,
            type_spec,
        })
    }

    /// Verify that a function call references an existing function, and that
    /// its arguments have a matching parameter in the function's type.
    fn check_function_call(
        &mut self,
        call: FunCallExpr,
    ) -> Result<typed::FunCallExpr, Vec<TypeError>> {
        let (receiver_type, given_args) = self
            .lookup_function(&call.name)
            .add_span(call.name_span)
            .concat_result(
                call.args
                    .into_iter()
                    .map(|p| self.check_expression(p))
                    .collect_all()
                    .map_err(|vecs| vecs.into_iter().flatten().collect()),
            )?;

        match receiver_type {
            TypeSpec::Function(expected_params, ret_type) => {
                if expected_params.len() != given_args.len() {
                    return singleton_error(
                        TypeErrorKind::ParamCountMismatch(
                            call.name.clone(),
                            expected_params.len(),
                            given_args.len(),
                        ),
                        call.args_span,
                    );
                }
                let args = expected_params
                    .into_iter()
                    .zip(given_args.into_iter())
                    .map(|(expected, actual)| {
                        if expected == actual.type_spec {
                            Ok(actual)
                        } else {
                            Err(TypeError::new(
                                TypeErrorKind::ParamTypeMismatch(expected, actual.type_spec),
                                actual.span,
                            ))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(typed::FunCallExpr {
                    name: call.name,
                    name_span: call.name_span,
                    args,
                    args_span: call.args_span,
                    type_spec: *ret_type,
                })
            }
            _ => Err(vec![TypeError::new(
                TypeErrorKind::NotCallable(receiver_type),
                call.name_span,
            )]),
        }
    }

    /// Verify that a unary expression is well-typed.
    fn check_unary(&mut self, un: UnExpr) -> Result<typed::UnExpr, Vec<TypeError>> {
        let rhs = self.check_expression(un.rhs)?;

        Ok(match (un.op, &rhs.type_spec) {
            (UnOp::Not, TypeSpec::Bool) => typed::UnExpr {
                rhs,
                op: un.op,
                type_spec: TypeSpec::Bool,
            },
            (UnOp::Negate, TypeSpec::Int) => typed::UnExpr {
                rhs,
                op: un.op,
                type_spec: TypeSpec::Int,
            },
            (op, ty) => error(TypeErrorKind::UnOperandType(op, ty.clone()), rhs.span)?,
        })
    }

    /// Verify that a binary expression is well-typed. This may produce multiple errors, if
    /// evaluating both the left-hand side and the right-hand side results in type errors.
    fn check_binary(&mut self, bin: BinExpr) -> Result<typed::BinExpr, Vec<TypeError>> {
        let (lhs, rhs) = self
            .check_expression(bin.lhs)
            .concat_result(self.check_expression(bin.rhs))?;

        let (type_spec, op) = BinOpChecker::check(
            bin.op,
            lhs.type_spec.clone(),
            rhs.type_spec.clone(),
            lhs.span,
            rhs.span,
        )?;
        Ok(typed::BinExpr {
            lhs,
            op,
            rhs,
            type_spec,
        })
    }

    fn check_ternary(&mut self, ter: TerExpr) -> Result<typed::TerExpr, Vec<TypeError>> {
        let ((lhs, mhs), rhs) = self
            .check_expression(ter.lhs)
            .concat_result(self.check_expression(ter.mhs))
            .concat_result(self.check_expression(ter.rhs))?;

        match ter.op {
            TerOp::If => {
                let mut errors = vec![];
                if mhs.type_spec != TypeSpec::Bool {
                    errors.push(TypeError::new(
                        TypeErrorKind::TernaryIfCondition(mhs.type_spec.clone()),
                        mhs.span,
                    ));
                }
                if lhs.type_spec != rhs.type_spec {
                    errors.push(TypeError::new(
                        TypeErrorKind::TernaryIfBranchMismatch(
                            lhs.type_spec.clone(),
                            rhs.type_spec.clone(),
                        ),
                        lhs.span.extend_to(rhs.span),
                    ));
                }
                if errors.is_empty() {
                    let type_spec = lhs.type_spec.clone();
                    Ok(typed::TerExpr {
                        lhs,
                        op: ter.op,
                        mhs,
                        rhs,
                        type_spec,
                    })
                } else {
                    Err(errors)
                }
            }
        }
    }

    /// Look up a builtin or global function.
    fn lookup_function(&mut self, name: &str) -> Result<TypeSpec, TypeErrorKind> {
        match builtins::get_type_map().get(name) {
            Some((builtin, ty)) => {
                self.program.used_builtins.push(*builtin);
                Ok(ty.clone())
            }
            None => self.program.lookup(name),
        }
    }

    fn check_assignment(target: &TypeSpec, value: &TypeSpec) -> Result<(), TypeErrorKind> {
        use TypeSpec::*;
        match (target, value) {
            // Assigning the same type to itself is always allowed.
            (t, v) if t == v => Ok(()),
            // Assigning `None` to a primitive type is not allowed.
            (Int | Bool, None) => Err(TypeErrorKind::AssignNoneToPrimitive(target.clone())),
            // The <None> type is not nameable, so the only way this branch can be reached
            // is as the result of a compiler bug.
            (None, _) => unreachable!("Illegal <None>-typed variable found"),
            // All remaining assignments are invalid
            (Int | Bool, Int | Bool) => Err(TypeErrorKind::Assign(target.clone(), value.clone())),
            _ => todo!("check this assignment {} <- {}", target, value),
        }
    }

    fn check_literal(&self, literal: &Literal) -> TypeSpec {
        match literal {
            Literal::Boolean(_) => TypeSpec::Bool,
            Literal::Integer(_) => TypeSpec::Int,
            Literal::None => TypeSpec::None,
        }
    }
}

impl typed::Program {
    pub fn set_type(&mut self, name: String, type_spec: TypeSpec) -> Result<(), TypeErrorKind> {
        self.global_environment.set_type(name, type_spec)
    }

    pub fn lookup(&self, name: &str) -> Result<TypeSpec, TypeErrorKind> {
        self.global_environment.lookup(name)
    }
}

impl Environment {
    /// Associate an identifier with a type. Returns an error if the identifier
    /// was already present.
    pub fn set_type(&mut self, name: String, type_spec: TypeSpec) -> Result<(), TypeErrorKind> {
        if let Entry::Vacant(e) = self.type_map.entry(name.clone()) {
            e.insert(type_spec);
            Ok(())
        } else {
            Err(TypeErrorKind::DuplicateIdentifier(name))
        }
    }

    pub fn lookup(&self, name: &str) -> Result<TypeSpec, TypeErrorKind> {
        self.type_map
            .get(name)
            .cloned()
            .ok_or_else(|| TypeErrorKind::UnknownIdentifier(name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex, parser::parse, span::Span};

    use super::*;

    macro_rules! make_program {
        ($source:expr) => {{
            let tokens = lex($source).unwrap();
            parse(&tokens).unwrap()
        }};
    }

    macro_rules! make_type_checker {
        ($source:expr) => {{
            let program = make_program!($source);
            let mut checker = TypeChecker::new();
            let res = checker.check(program);
            (res, checker)
        }};
    }

    macro_rules! check_with {
        ($method:ident, $source:expr, $param:expr) => {{
            let (_, mut checker) = make_type_checker!($source);

            checker.$method($param)
        }};
    }

    macro_rules! assert_variable_type {
        ($source:expr, $type_name:expr, $type_spec:expr) => {{
            let prg = make_program!($source);
            let prg = TypeChecker::run(prg).unwrap();

            let ty = &prg.global_environment.type_map[$type_name];
            assert_eq!(
                ty, &$type_spec,
                "\n\nExpected the type of '{}' to be '{}', but it was '{}'\n\n",
                $type_name, $type_spec, ty,
            );
        }};
    }

    macro_rules! assert_type_checks {
        ($source:expr) => {{
            let prg = make_program!($source);
            let res = TypeChecker::run(prg).collect_errors();

            let errors: Vec<_> = res.iter().map(|e| e.to_string()).collect();
            assert!(res.is_empty(), "\n\nExpected this type check to succeed, but found the following error(s): \n{:#?}\n\n", errors);
        }};
    }

    macro_rules! assert_type_error {
        ($source:expr, $expected_err:expr) => {{
            let prg = make_program!($source);
            let res = TypeChecker::run(prg).collect_errors();

            let errors: Vec<_> = res.iter().map(|e| e.to_string()).collect();
            assert!(
                res.len() != 0,
                "\n\nExpected to find one type error, but found none.\n\n"
            );
            assert!(
                res.len() == 1,
                "\n\nExpected to find one type error, but found multiple: \n{:#?}\n\n",
                errors
            );
            assert_eq!(
                &$expected_err,
                res[0].kind(),
                "\n\nExpected to find this type error:\n\t{}\n\nbut found:\n\t{}\n\n",
                &$expected_err,
                res[0].kind(),
            );
        }};
    }

    /// ChocoPy reference: 5.2 - [VAR-READ]
    #[test]
    fn var_read() {
        let expr_type = check_with!(
            check_expression,
            "a : int = 10",
            Expr::new(ExprKind::Identifier("a".to_string()), Span::zero())
        )
        .unwrap();

        assert_eq!(expr_type.type_spec, TypeSpec::Int)
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn var_def_int_assigns_correct_type() {
        assert_variable_type!("a : int = 10", "a", TypeSpec::Int);
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn var_def_bool_assigns_correct_type() {
        assert_variable_type!("a : bool = False", "a", TypeSpec::Bool);
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn var_def_none_not_allowed() {
        assert_type_error!(
            "a : int = None",
            TypeErrorKind::AssignNoneToPrimitive(TypeSpec::Int)
        );
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn valid_program_typechecks() {
        assert_type_checks!(
            r#"
a: int = 10
b: int = 9
c: int = 0
c = (a + a) * b
"#
        )
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn if_without_elif_else_typechecks() {
        assert_type_checks!(
            r#"
if True:
	10 * 10
"#
        )
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn if_with_non_bool_condition_fails() {
        assert_type_error!(
            r#"
if 10 * 10:
	20
"#,
            TypeErrorKind::IfCondition(TypeSpec::Int)
        )
    }

    /// ChocoPy reference: 5.2
    #[test]
    #[ignore = "TODO: should we do this in the type checker? or in the parser"]
    fn var_def_non_literal_not_allowed() {
        assert_type_error!(
            "a : int = None",
            TypeErrorKind::AssignNoneToPrimitive(TypeSpec::Bool)
        );
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn duplicate_identifier_not_allowed() {
        assert_type_error!(
            "a:int = 10\na:int = 99",
            TypeErrorKind::DuplicateIdentifier("a".to_string())
        );
    }
}

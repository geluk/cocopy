use std::collections::hash_map::Entry;

use crate::{
    ast::{typed, typed::Environment, untyped, untyped::*, TypeSpec},
    builtins,
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

            self.program.var_defs.push(var_def);
        }

        type_errors
    }

    /// Check function definitions and write them to the global environment.
    fn assign_func_defs(&mut self, func_defs: Vec<FuncDef>) -> Vec<TypeError> {
        let mut type_errors = vec![];
        for func_def in func_defs {
            let mut param_types = vec![];
            for param in func_def.parameters.iter() {
                param_types.push(param.type_spec.clone());
                if let Err(kind) = self
                    .program
                    .set_type(param.name.clone(), param.type_spec.clone())
                {
                    type_errors.push(TypeError::new(kind, param.span));
                }
            }

            let param_types = func_def
                .parameters
                .iter()
                .map(|p| p.type_spec.clone())
                .collect();

            let func_type = TypeSpec::Function(param_types, Box::new(func_def.return_type.clone()));

            if let Err(kind) = self.program.set_type(func_def.name.clone(), func_type) {
                type_errors.push(TypeError::new(kind, func_def.decl_span));
            }

            type_errors.append(&mut self.check_block(&func_def.body));

            self.program.func_defs.push(func_def);
        }
        type_errors
    }

    /// Verify that all statements are well-typed.
    fn check_statements(&mut self, statements: Vec<Statement>) -> Vec<TypeError> {
        let errors = statements
            .iter()
            .flat_map(|s| self.check_statement(s))
            .collect();
        self.program.statements = statements;
        errors
    }

    /// Verify that a statement is well-typed.
    fn check_statement(&mut self, statement: &Statement) -> Vec<TypeError> {
        match &statement.stmt_kind {
            StmtKind::Pass => vec![],
            StmtKind::Evaluate(expr) => self.check_expression(expr).collect_errors(),
            StmtKind::Return(_) => todo!("return statements are not supported yet"),
            StmtKind::Assign(assign) => self.check_assign_stmt(assign).collect_errors(),
            StmtKind::If(if_stmt) => self.check_if(if_stmt),
        }
    }

    fn check_if(&mut self, if_stmt: &If) -> Vec<TypeError> {
        let mut errors = self
            .check_expression(&if_stmt.condition)
            .and_then(|ty| {
                if ty != TypeSpec::Bool {
                    singleton_error(TypeErrorKind::IfCondition(ty), if_stmt.condition.span)
                } else {
                    Ok(())
                }
            })
            .collect_errors();

        let mut body_errors = self.check_block(&if_stmt.body);
        errors.append(&mut body_errors);

        if let Some(else_body) = &if_stmt.else_body {
            let mut else_body_errors = self.check_block(else_body);
            errors.append(&mut else_body_errors);
        }

        errors
    }

    fn check_block(&mut self, block: &Block) -> Vec<TypeError> {
        block
            .statements
            .iter()
            .flat_map(|stmt| self.check_statement(stmt))
            .collect()
    }

    /// Verify that assignment to a variable is well-typed.
    fn check_assign_stmt(&mut self, assign: &Assign) -> Result<(), Vec<TypeError>> {
        let expr_type = self.check_expression(&assign.value)?;

        match &assign.target.expr_kind {
            ExprKind::Identifier(id) => {
                let target_type = self.program.lookup(id).add_span(assign.span)?;

                Self::check_assignment(&target_type, &expr_type).add_span(assign.value.span)?;
            }
            k => todo!("Cannot check assignment to {}", k),
        }
        Ok(())
    }

    /// Verify that an expression is well-typed.
    fn check_expression(&mut self, expression: &Expr) -> Result<TypeSpec, Vec<TypeError>> {
        let span = expression.span;
        match &expression.expr_kind {
            ExprKind::Literal(l) => Ok(self.check_literal(l)),
            ExprKind::Identifier(id) => Ok(self.program.lookup(id).add_span(span)?),
            ExprKind::Member(_) => todo!("Type check member expressions"),
            ExprKind::Index(_) => todo!("Type check index expressions"),
            ExprKind::FunctionCall(call) => self.check_function_call(call),
            ExprKind::MethodCall(_) => todo!("Type check method call expressions"),
            ExprKind::Unary(un) => self.check_unary(un),
            ExprKind::Binary(bin) => self.check_binary(bin),
            ExprKind::Ternary(ter) => self.check_ternary(ter),
        }
    }

    /// Verify that a function call references an existing function, and that
    /// its arguments have a matching parameter in the function's type.
    fn check_function_call(&mut self, call: &FunCallExpr) -> Result<TypeSpec, Vec<TypeError>> {
        let (receiver_type, given_params) = self
            .lookup_function(&call.name)
            .add_span(call.name_span)
            .concat_result(
                call.params
                    .iter()
                    .map(|p| self.check_expression(p))
                    .collect_all()
                    .map_err(|vecs| vecs.into_iter().flatten().collect()),
            )?;

        match receiver_type {
            TypeSpec::Function(expected_params, ret_type) => {
                if expected_params.len() != given_params.len() {
                    return singleton_error(
                        TypeErrorKind::ParamCountMismatch(
                            call.name.clone(),
                            expected_params.len(),
                            given_params.len(),
                        ),
                        call.params_span,
                    );
                }
                let param_errors: Vec<_> = expected_params
                    .into_iter()
                    .zip(given_params.into_iter())
                    .zip(&call.params)
                    .filter(|((exp, act), _)| exp != act)
                    .map(|((exp, act), expr)| {
                        TypeError::new(TypeErrorKind::ParamTypeMismatch(exp, act), expr.span)
                    })
                    .collect();

                if param_errors.is_empty() {
                    Ok(*ret_type)
                } else {
                    Err(param_errors)
                }
            }
            _ => Err(vec![TypeError::new(
                TypeErrorKind::NotCallable(receiver_type),
                call.name_span,
            )]),
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

    /// Verify that a unary expression is well-typed.
    fn check_unary(&mut self, un: &UnExpr) -> Result<TypeSpec, Vec<TypeError>> {
        let expr_type = self.check_expression(&un.rhs)?;

        Ok(match (un.op, expr_type) {
            (UnOp::Not, TypeSpec::Bool) => TypeSpec::Bool,
            (UnOp::Negate, TypeSpec::Int) => TypeSpec::Int,
            (op, ty) => error(TypeErrorKind::UnOperandType(op, ty), un.rhs.span)?,
        })
    }

    /// Verify that a binary expression is well-typed. This may produce multiple errors, if
    /// evaluating both the left-hand side and the right-hand side results in type errors.
    fn check_binary(&mut self, bin: &BinExpr) -> Result<TypeSpec, Vec<TypeError>> {
        let (lhs, rhs) = self
            .check_expression(&bin.lhs)
            .concat_result(self.check_expression(&bin.rhs))?;

        BinOpChecker::check(bin, lhs, rhs)
    }

    fn check_ternary(&mut self, ter: &TerExpr) -> Result<TypeSpec, Vec<TypeError>> {
        let ((lhs, mhs), rhs) = self
            .check_expression(&ter.lhs)
            .concat_result(self.check_expression(&ter.mhs))
            .concat_result(self.check_expression(&ter.rhs))?;

        match ter.op {
            TerOp::If => {
                let mut errors = vec![];
                if mhs != TypeSpec::Bool {
                    errors.push(TypeError::new(
                        TypeErrorKind::TernaryIfCondition(mhs),
                        ter.mhs.span,
                    ));
                }
                if lhs != rhs {
                    errors.push(TypeError::new(
                        TypeErrorKind::TernaryIfBranchMismatch(lhs.clone(), rhs),
                        ter.lhs.span.extend_to(ter.rhs.span),
                    ));
                }
                if errors.is_empty() {
                    Ok(lhs)
                } else {
                    Err(errors)
                }
            }
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
            &Expr::new(ExprKind::Identifier("a".to_string()), Span::zero())
        )
        .unwrap();

        assert_eq!(expr_type, TypeSpec::Int)
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

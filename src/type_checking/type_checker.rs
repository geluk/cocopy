use std::collections::HashMap;

use crate::{
    ast::{untyped::*, TypeSpec},
    builtins,
};

use super::{bin_op_checker::BinOpChecker, error::*};

pub fn verify_well_typed(program: &Program) -> Result<(), Vec<TypeError>> {
    let type_errors = TypeChecker::new(program).run();

    if type_errors.is_empty() {
        Ok(())
    } else {
        Err(type_errors)
    }
}

struct TypeChecker<'a> {
    global_environment: Environment,
    program: &'a Program,
}

impl<'a> TypeChecker<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            global_environment: Environment::global(),
            program,
        }
    }

    fn run(&mut self) -> Vec<TypeError> {
        let mut type_errors = self.assign_var_defs();

        type_errors.append(&mut self.check_statements());

        type_errors
    }

    /// Check variable definitions and write them to the global environment.
    fn assign_var_defs(&mut self) -> Vec<TypeError> {
        let mut type_errors = vec![];
        for var_def in &self.program.var_defs {
            if let Err(kind) = self
                .global_environment
                .set_type(var_def.name.to_string(), var_def.type_spec.clone())
            {
                type_errors.push(TypeError::new(kind, var_def.span));
            }

            let lit_type = self.check_literal(&var_def.value);

            if let Err(kind) = Self::check_assignment(&var_def.type_spec, &lit_type) {
                type_errors.push(TypeError::new(kind, var_def.span))
            }
        }

        type_errors
    }

    /// Verify that all statements are well-typed.
    fn check_statements(&self) -> Vec<TypeError> {
        self.program
            .statements
            .iter()
            .flat_map(|s| self.check_statement(s).collect_errors())
            .collect()
    }

    /// Verify that a statement is well-typed.
    fn check_statement(&self, statement: &Statement) -> Result<(), Vec<TypeError>> {
        match &statement.stmt_kind {
            StmtKind::Pass => (),
            StmtKind::Evaluate(expr) => {
                self.check_expression(expr)?;
            }
            StmtKind::Return(_) => todo!("return statements are not supported yet"),
            StmtKind::Assign(assign) => self.check_assign_stmt(assign)?,
        }
        Ok(())
    }

    /// Verify that assignment to a variable is well-typed.
    fn check_assign_stmt(&self, assign: &Assign) -> Result<(), Vec<TypeError>> {
        let expr_type = self.check_expression(&assign.value)?;

        match &assign.target.expr_kind {
            ExprKind::Identifier(id) => {
                let target_type = self.global_environment.lookup(id).add_span(assign.span)?;

                Self::check_assignment(&target_type, &expr_type).add_span(assign.value.span)?;
            }
            k => todo!("Cannot check assignment to {}", k),
        }
        Ok(())
    }

    /// Verify that an expression is well-typed.
    fn check_expression(&self, expression: &Expr) -> Result<TypeSpec, Vec<TypeError>> {
        let span = expression.span;
        match &expression.expr_kind {
            ExprKind::Literal(l) => Ok(self.check_literal(l)),
            ExprKind::Identifier(id) => Ok(self.global_environment.lookup(id).add_span(span)?),
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
    fn check_function_call(&self, call: &FunCallExpr) -> Result<TypeSpec, Vec<TypeError>> {
        let (func_type, param_type) = self
            .global_environment
            .lookup(&call.name)
            .add_span(call.name_span)
            .concat_result(self.check_expression(&call.params))?;

        if let TypeSpec::Function(mut args, ret) = func_type {
            if args.len() != 1 {
                singleton_error(
                    TypeErrorKind::ParamCountMismatch(call.name.clone(), 1, args.len()),
                    call.params.span,
                )
            } else if param_type != args[0] {
                singleton_error(
                    TypeErrorKind::ParamTypeMismatch(args.remove(0), param_type),
                    call.params.span,
                )
            } else {
                Ok(*ret)
            }
        } else {
            return Err(vec![TypeError::new(
                TypeErrorKind::NotCallable(func_type),
                call.name_span,
            )]);
        }
    }

    /// Verify that a unary expression is well-typed.
    fn check_unary(&self, un: &UnExpr) -> Result<TypeSpec, Vec<TypeError>> {
        let expr_type = self.check_expression(&un.rhs)?;

        Ok(match (un.op, expr_type) {
            (UnOp::Not, TypeSpec::Bool) => TypeSpec::Bool,
            (UnOp::Negate, TypeSpec::Int) => TypeSpec::Int,
            (op, ty) => error(TypeErrorKind::UnOperandType(op, ty), un.rhs.span)?,
        })
    }

    /// Verify that a binary expression is well-typed. This may produce multiple errors, if
    /// evaluating both the left-hand side and the right-hand side results in type errors.
    fn check_binary(&self, bin: &BinExpr) -> Result<TypeSpec, Vec<TypeError>> {
        let (lhs, rhs) = self
            .check_expression(&bin.lhs)
            .concat_result(self.check_expression(&bin.rhs))?;

        BinOpChecker::check(bin, lhs, rhs)
    }

    fn check_ternary(&self, ter: &TerExpr) -> Result<TypeSpec, Vec<TypeError>> {
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

pub struct Environment {
    type_map: HashMap<String, TypeSpec>,
}
impl Environment {
    pub fn global() -> Self {
        Self {
            type_map: builtins::get_type_map(),
        }
    }

    pub fn set_type(&mut self, name: String, type_spec: TypeSpec) -> Result<(), TypeErrorKind> {
        if self.type_map.contains_key(&name) {
            Err(TypeErrorKind::DuplicateIdentifier(name))
        } else {
            self.type_map.insert(name, type_spec);
            Ok(())
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
    use crate::{lexer::lex, parser::parse};

    use super::*;

    macro_rules! make_program {
        ($source:expr) => {{
            let tokens = lex($source).unwrap();
            parse(&tokens).unwrap()
        }};
    }

    macro_rules! assert_variable_type {
        ($source:expr, $type_name:expr, $type_spec:expr) => {{
            let prg = make_program!($source);
            let mut checker = TypeChecker::new(&prg);
            checker.run();

            let ty = &checker.global_environment.type_map[$type_name];
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
            let mut checker = TypeChecker::new(&prg);
            let res = checker.run();

            let errors: Vec<_> = res.iter().map(|e| e.to_string()).collect();
            assert!(res.is_empty(), "\n\nExpected this type check to succeed, but found the following error(s): \n{:#?}\n\n", errors);
        }};
    }

    macro_rules! assert_type_error {
        ($source:expr, $expected_err:expr) => {{
            let prg = make_program!($source);
            let mut checker = TypeChecker::new(&prg);
            let res = checker.run();

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

    /// ChocoPy reference: 2.4
    #[test]
    fn test() {}

    /// ChocoPy reference: 5.2
    #[test]
    fn var_def_int_assigns_correct_type() {
        assert_variable_type!("a : int = 10", "a", TypeSpec::Int);
    }

    /// ChocoPy reference: 5.2
    #[test]
    fn var_def_bool_assigns_correct_type() {
        assert_variable_type!("a : bool = 10", "a", TypeSpec::Bool);
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

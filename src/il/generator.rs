use crate::{ast::untyped::*, builtins::Builtin};

use super::{name_generator::*, tac::*};

pub fn generate(program: Program) -> TacListing {
    Tac::generate(program)
}

struct Tac {
    listing: TacListing,
    name_generator: NameGenerator,
}
impl Tac {
    fn generate(program: Program) -> TacListing {
        let mut tac = Self {
            listing: TacListing::new(),
            name_generator: NameGenerator::new(),
        };

        for var_def in program.var_defs {
            tac.lower_var_def(var_def);
        }

        for stmt in program.statements {
            tac.lower_stmt(stmt);
        }

        tac.listing
    }

    fn lower_var_def(&mut self, var_def: VarDef) {
        self.emit_assign(var_def.name, self.lower_literal(var_def.value));
    }

    fn lower_stmt(&mut self, stmt: Statement) {
        match stmt.stmt_kind {
            StmtKind::Pass => (),
            StmtKind::Evaluate(expr) => {
                self.lower_expr(expr);
            }
            StmtKind::Return(_) => todo!(),
            StmtKind::Assign(assign) => self.lower_assign(assign),
        };
    }

    fn lower_assign(&mut self, assign: Assign) {
        let result = self.lower_expr(assign.value);

        if let ExprKind::Identifier(tgt) = assign.target.expr_kind {
            self.emit_assign(tgt, result);
        } else {
            todo!("Implement member assignment");
        }
    }

    fn lower_expr(&mut self, expr: Expr) -> Value {
        match expr.expr_kind {
            ExprKind::Literal(lit) => self.lower_literal(lit),
            ExprKind::Identifier(id) => self.lower_identifier(id),
            ExprKind::Member(_) => todo!(),
            ExprKind::Index(_) => todo!(),
            ExprKind::Unary(_) => todo!(),
            ExprKind::FunctionCall(call) => self.lower_function_call(*call),
            ExprKind::MethodCall(_) => todo!(),
            ExprKind::Binary(bin) => self.lower_binexpr(*bin),
            ExprKind::Ternary(_) => todo!(),
        }
    }

    fn lower_literal(&self, lit: Literal) -> Value {
        match lit {
            Literal::Integer(i) => Value::Const(i as TargetSize),
            Literal::Boolean(b) => Value::Const(b as TargetSize),
            Literal::None => todo!(),
        }
    }

    fn lower_identifier(&self, id: String) -> Value {
        if let Ok(builtin) = id.parse() {
            Value::Name(Name::Builtin(builtin))
        } else {
            Value::Name(self.name_generator.last_subscript(id))
        }
    }

    fn lower_function_call(&mut self, call: FunCallExpr) -> Value {
        let expr_value = self.lower_expr(call.params);

        self.emit(Instruction::Param(expr_value));

        let temp_name = self.name_generator.next_temp();
        // TODO: allow calls to other types of functions here.
        self.emit(Instruction::Call(
            temp_name.clone(),
            Builtin::Print,
            // We'll need to know the function's type to determine the number of parameters.
            1,
        ));

        Value::Name(temp_name)
    }

    fn lower_binexpr(&mut self, expr: BinExpr) -> Value {
        let lhs = self.lower_expr(expr.lhs);
        let rhs = self.lower_expr(expr.rhs);

        let temp_name = self.name_generator.next_temp();

        self.emit(Instruction::Bin(temp_name.clone(), expr.op, lhs, rhs));

        Value::Name(temp_name)
    }

    fn emit_assign(&mut self, id: String, value: Value) {
        let name = self.name_generator.next_subscript(id);
        self.emit(Instruction::Assign(name, value));
    }

    /// Emit an instruction, adding it to the listing.
    fn emit(&mut self, instr: Instruction) {
        self.listing.push(instr);
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex, parser::parse};

    use super::*;

    macro_rules! assert_generates {
        ($source:expr, $il:expr) => {{
            let tokens = lex($source).unwrap();
            let program = parse(&tokens).unwrap();
            let instrs = Tac::generate(program);

            let instr_lines: Vec<_> = instrs.into_vec().iter().map(|i| i.to_string()).collect();

            assert_eq!($il, instr_lines)
        }};
    }

    #[test]
    fn simple_program_generates_tac() {
        assert_generates!(
            "a:int = 10\na = a + (100 + 1)",
            vec!["a^1 = 10", "%t1 = 100 + 1", "%t2 = a^1 + %t1", "a^2 = %t2",]
        )
    }

    #[test]
    fn function_call_generates_param_and_call() {
        assert_generates!("print(999)", vec!["param 999", "%t1 = call print, 1",])
    }
}

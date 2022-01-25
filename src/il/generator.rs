use crate::ast::untyped::*;

use super::{name_generator::*, tac::*};

pub fn generate(program: Program) -> Vec<Instruction> {
    Tac::generate(program)
}

struct Tac {
    instructions: Vec<Instruction>,
    name_generator: NameGenerator,
}
impl Tac {
    fn generate(program: Program) -> Vec<Instruction> {
        let mut tac = Self {
            instructions: vec![],
            name_generator: NameGenerator::new(),
        };

        for var_def in program.var_defs {
            tac.lower_var_def(var_def);
        }

        for stmt in program.statements {
            tac.lower_stmt(stmt);
        }

        tac.instructions
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

        if let ExprKind::Identifier(tgt) = assign.target.expr_type {
            self.emit_assign(tgt, result);
        } else {
            todo!("Implement member assignment");
        }
    }

    fn lower_expr(&mut self, expr: Expr) -> Value {
        match expr.expr_type {
            ExprKind::Literal(lit) => self.lower_literal(lit),
            ExprKind::Identifier(id) => Value::Name(self.name_generator.last_subscript(id)),
            ExprKind::Unary(_) => todo!(),
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

    /// Emit an instruction, adding it to the block.
    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr);
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

            let instr_lines: Vec<_> = instrs.iter().map(|i| i.to_string()).collect();

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
}

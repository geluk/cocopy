use crate::parser::syntax_tree::*;

use super::{name_generator::*, tac::*};

pub fn generate(program: &Program) -> Vec<Instruction> {
    Tac::lower_program(program)
}

struct Tac {
    instructions: Vec<Instruction>,
    name_generator: NameGenerator,
}
impl Tac {
    fn lower_program(program: &Program) -> Vec<Instruction> {
        let mut tac = Self {
            instructions: vec![],
            name_generator: NameGenerator::new(),
        };

        for var_def in &program.var_defs {
            tac.lower_var_def(var_def);
        }

        for stmt in &program.statements {
            tac.lower_stmt(stmt);
        }

        tac.instructions
    }

    fn lower_var_def(&mut self, var_def: &VarDef) {
        self.emit(Instruction::Assign(
            Name::for_id(&var_def.name),
            self.lower_literal(var_def.value.clone()),
        ));
    }

    fn lower_stmt(&mut self, stmt: &Statement) {
        match &stmt.stmt_kind {
            StmtKind::Pass => (),
            StmtKind::Evaluate(expr) => {
                self.lower_expr(expr);
            }
            StmtKind::Return(_) => todo!(),
            StmtKind::Assign(assign) => self.lower_assign(assign),
        };
    }

    fn lower_assign(&mut self, assign: &Assign) {
        let result = self.lower_expr(&assign.value);

        if let ExprKind::Identifier(tgt) = &assign.target.expr_type {
            self.emit(Instruction::Assign(Name::for_id(tgt), result));
        } else {
            todo!();
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> Value {
        match &expr.expr_type {
            ExprKind::Literal(lit) => self.lower_literal(lit.clone()),
            ExprKind::Identifier(id) => Value::Name(Name::for_id(id)),
            ExprKind::Unary(_) => todo!(),
            ExprKind::Binary(bin) => self.lower_binexpr(bin),
            ExprKind::Ternary(_) => todo!(),
        }
    }

    fn lower_literal(&self, lit: Literal) -> Value {
        match lit {
            Literal::Integer(i) => Value::Lit(i as TargetSize),
            Literal::Boolean(b) => Value::Lit(b as TargetSize),
            Literal::None => todo!(),
        }
    }

    fn lower_binexpr(&mut self, expr: &BinExpr) -> Value {
        let lhs = self.lower_expr(&expr.lhs);
        let rhs = self.lower_expr(&expr.rhs);

        let temp_name = self.name_generator.next();

        self.emit(Instruction::Bin(temp_name.clone(), expr.op, lhs, rhs));

        Value::Name(temp_name)
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
            let instrs = Tac::lower_program(&program);

            let instr_lines: Vec<_> = instrs.iter().map(|i| i.to_string()).collect();

            assert_eq!($il, instr_lines)
        }};
    }

    #[test]
    fn simple_program() {
        assert_generates!(
            "a:int = 10\na = a + (100 + 1)",
            vec!["a = 10", "t1 = 100 + 1", "t2 = a + %t1", "a = %t2",]
        )
    }
}

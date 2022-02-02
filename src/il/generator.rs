use crate::{ast::untyped::*, builtins::Builtin};

use super::{label_generator::*, name_generator::*, tac::*};

pub fn generate(program: Program) -> TacListing {
    TacGenerator::generate(program)
}

struct TacGenerator {
    listing: TacListing,
    name_generator: NameGenerator,
    label_generator: LabelGenerator,
}
impl TacGenerator {
    /// Generate a three-address code listing for a program.
    fn generate(program: Program) -> TacListing {
        let mut tac = Self {
            listing: TacListing::new(),
            name_generator: NameGenerator::new(),
            label_generator: LabelGenerator::new(),
        };

        for var_def in program.var_defs {
            tac.lower_var_def(var_def);
        }

        for stmt in program.statements {
            tac.lower_stmt(stmt);
        }

        tac.listing
    }

    /// Lower a variable definition to an assignment instruction.
    fn lower_var_def(&mut self, var_def: VarDef) {
        self.emit_assign(var_def.name, self.convert_literal(var_def.value));
    }

    /// Lower a statement.
    fn lower_stmt(&mut self, stmt: Statement) {
        match stmt.stmt_kind {
            StmtKind::Pass => (),
            StmtKind::Evaluate(expr) => {
                self.lower_expr(expr);
            }
            StmtKind::Return(_) => todo!("return statements are not supported yet"),
            StmtKind::Assign(assign) => self.lower_assign(assign),
            StmtKind::If(if_stmt) => self.lower_if(if_stmt),
        };
    }

    /// Lower a block of statements.
    fn lower_block(&mut self, block: Block) {
        for stmt in block.statements {
            self.lower_stmt(stmt);
        }
    }

    /// Lower an if-statement. If-statements are lowered to one or more conditional jumps, to
    /// jump into the correct code block.
    fn lower_if(&mut self, if_stmt: If) {
        let cond = self.lower_expr(if_stmt.condition);
        let end_lbl = self.label_generator.next_label("if_end");
        self.emit(InstrKind::IfFalse(cond, end_lbl.clone()));

        self.lower_block(if_stmt.body);

        self.emit_label(InstrKind::Nop, end_lbl);
    }

    /// Lower a variable assignment by evaluating an expression and assigning its result to a
    /// variable.
    fn lower_assign(&mut self, assign: Assign) {
        let result = self.lower_expr(assign.value);

        if let ExprKind::Identifier(tgt) = assign.target.expr_kind {
            self.emit_assign(tgt, result);
        } else {
            todo!("Implement member assignment");
        }
    }

    /// Lower an expression.
    fn lower_expr(&mut self, expr: Expr) -> Value {
        match expr.expr_kind {
            ExprKind::Literal(lit) => self.convert_literal(lit),
            ExprKind::Identifier(id) => self.convert_identifier(id),
            ExprKind::Member(_) => todo!(),
            ExprKind::Index(_) => todo!(),
            ExprKind::Unary(_) => todo!(),
            ExprKind::FunctionCall(call) => self.lower_function_call(*call),
            ExprKind::MethodCall(_) => todo!(),
            ExprKind::Binary(bin) => self.lower_binexpr(*bin),
            ExprKind::Ternary(_) => todo!(),
        }
    }

    /// Convert a literal to a [`Value`]. This does not result in the emission
    /// of intermediate code.
    fn convert_literal(&self, lit: Literal) -> Value {
        match lit {
            Literal::Integer(i) => Value::Const(i as TargetSize),
            Literal::Boolean(b) => Value::Const(b as TargetSize),
            Literal::None => todo!(),
        }
    }

    /// Convert an identifier to a [`Value`]. This does not result in the emission
    /// of intermediate code.
    fn convert_identifier(&self, id: String) -> Value {
        if let Ok(builtin) = id.parse() {
            Value::Name(Name::Builtin(builtin))
        } else {
            Value::Name(self.name_generator.last_subscript(id))
        }
    }

    /// Lower a function call. Its parameters are pushed onto the parameter stack,
    /// then the function is called.
    fn lower_function_call(&mut self, call: FunCallExpr) -> Value {
        let expr_value = self.lower_expr(call.params);

        self.emit(InstrKind::Param(expr_value));

        let temp_name = self.name_generator.next_temp();
        // TODO: allow calls to other types of functions here.
        self.emit(InstrKind::Call(
            temp_name.clone(),
            Builtin::Print,
            // We'll need to know the function's type to determine the number of parameters.
            1,
        ));

        Value::Name(temp_name)
    }

    /// Lower a binary expression. If the expression contains a boolean operator, it is dispatched
    /// to [`Self::lower_bool_expr`].
    fn lower_binexpr(&mut self, expr: BinExpr) -> Value {
        if expr.op == BinOp::And {
            return self.lower_bool_expr(expr, BoolOp::And);
        } else if expr.op == BinOp::Or {
            return self.lower_bool_expr(expr, BoolOp::Or);
        }

        let lhs = self.lower_expr(expr.lhs);
        let rhs = self.lower_expr(expr.rhs);

        let res_name = self.name_generator.next_temp();
        self.emit(InstrKind::Bin(res_name.clone(), expr.op, lhs, rhs));

        Value::Name(res_name)
    }

    /// Lower a boolean expression. Boolean expressions are special-cased because they result in
    /// the generation of conditional jumps to allow for short-circuiting.
    fn lower_bool_expr(&mut self, expr: BinExpr, op: BoolOp) -> Value {
        let true_lbl = self.label_generator.next_label("bexpr_t");
        let false_lbl = self.label_generator.next_label("bexpr_f");
        let end_lbl = self.label_generator.next_label("bexpr_e");

        let lhs = self.lower_expr(expr.lhs);
        match op {
            BoolOp::Or => {
                self.emit(InstrKind::IfTrue(lhs, true_lbl.clone()));
            }
            BoolOp::And => {
                self.emit(InstrKind::IfFalse(lhs, false_lbl.clone()));
            }
        }

        let rhs = self.lower_expr(expr.rhs);
        match op {
            BoolOp::Or => {
                self.emit(InstrKind::IfFalse(rhs, false_lbl.clone()));
            }
            BoolOp::And => {
                self.emit(InstrKind::IfFalse(rhs, false_lbl.clone()));
            }
        }

        // Should we use ɸ(t, f) here?
        let res_name = self.name_generator.next_temp();
        self.emit_label(
            InstrKind::Assign(res_name.clone(), Value::Const(1)),
            true_lbl,
        );
        self.emit(InstrKind::Goto(end_lbl.clone()));
        self.emit_label(
            InstrKind::Assign(res_name.clone(), Value::Const(0)),
            false_lbl,
        );
        self.emit_label(InstrKind::Nop, end_lbl);
        Value::Name(res_name)
    }

    /// Emit an assignment, assigning `value` to `id`.
    fn emit_assign(&mut self, id: String, value: Value) {
        let name = self.name_generator.next_subscript(id);
        self.emit(InstrKind::Assign(name, value));
    }

    /// Emit an instruction, adding it to the listing.
    fn emit(&mut self, kind: InstrKind) {
        let instr = Instruction::new(kind);
        self.listing.push(instr);
    }

    /// Emit an instruction, adding it to the listing.
    fn emit_label(&mut self, kind: InstrKind, label: Label) {
        let mut instr = Instruction::new(kind);
        instr.add_label(label);
        self.listing.push(instr);
    }
}

enum BoolOp {
    Or,
    And,
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex, parser::parse};

    use super::*;

    macro_rules! assert_generates {
        ($source:expr, $il:expr) => {{
            let tokens = lex($source).unwrap();
            let program = parse(&tokens).unwrap();
            let instrs = TacGenerator::generate(program);

            let instr_lines: Vec<_> = instrs.into_vec().iter().map(|i| i.to_string()).collect();

            assert_eq!(&$il[..], instr_lines)
        }};
    }

    #[test]
    fn simple_program_generates_tac() {
        assert_generates!(
            "a:int = 10\na = a + (100 + 1)",
            ["a^1 = 10", "%t1 = 100 + 1", "%t2 = a^1 + %t1", "a^2 = %t2",]
        )
    }

    #[test]
    fn if_stmt_generates_tac() {
        assert_generates!(
            "a:bool = True\nif a:\n\tprint(1)",
            [
                "a^1 = 1",
                "if_false a^1 goto if_end_1",
                "param 1",
                "%t1 = call print, 1",
                "if_end_1: nop"
            ]
        )
    }

    #[test]
    fn function_call_generates_param_and_call() {
        assert_generates!("print(999)", ["param 999", "%t1 = call print, 1",])
    }
}

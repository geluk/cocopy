use crate::ast::{
    typed::*,
    untyped::{Literal, Parameter},
};

use super::{label_generator::*, name_generator::*, phi::Variables, tac::*};

pub fn generate(program: Program) -> TacProgram {
    let mut tac = TacGenerator::<TacProgram> {
        procedure: TacProgram::new(),
        name_generator: NameGenerator::new(),
        label_generator: LabelGenerator::new(),
    };

    for var_def in program.var_defs {
        tac.lower_var_def(var_def);
    }

    for stmt in program.statements {
        tac.lower_stmt(stmt);
    }

    let mut tac_program = tac.procedure;
    let mut label_generator = tac.label_generator;

    for func in program.func_defs {
        let mut func_gen = TacGenerator::<TacListing> {
            procedure: TacListing::new(),
            name_generator: NameGenerator::new(),
            // To ensure that we generate globally unique jump labels, all
            // function bodies (including the main function) should share the
            // same label generator.
            // TODO: once we generate function-scoped labels,
            // we can drop this requirement.
            label_generator,
        };

        for parameter in func.parameters {
            func_gen.lower_parameter(parameter);
        }
        func_gen.lower_block(func.body);

        tac_program.functions.insert(func.name, func_gen.procedure);
        label_generator = func_gen.label_generator;
    }

    tac_program
}

trait TacProcedure {
    fn push(&mut self, instruction: Instruction);
}

impl TacProcedure for TacProgram {
    fn push(&mut self, instruction: Instruction) {
        self.top_level.push(instruction)
    }
}
impl TacProcedure for TacListing {
    fn push(&mut self, instruction: Instruction) {
        self.push(instruction)
    }
}

struct TacGenerator<P> {
    procedure: P,
    name_generator: NameGenerator,
    label_generator: LabelGenerator,
}
impl<P: TacProcedure> TacGenerator<P> {
    /// Lower a variable definition to an assignment instruction.
    fn lower_var_def(&mut self, var_def: VarDef) {
        self.emit_assign(var_def.name, self.convert_literal(var_def.value));
    }

    /// Lower a parameter definition, retrieving it and storing it in a local variable.
    fn lower_parameter(&mut self, parameter: Parameter) {
        let variable = self.name_generator.next_subscript(parameter.name);
        self.emit(InstrKind::Param(Name::Sub(variable)));
    }

    /// Lower a statement. Returns the variables that were assigned in this statement.
    fn lower_stmt(&mut self, stmt: Statement) -> Variables {
        match stmt.stmt_kind {
            StmtKind::Pass => (),
            StmtKind::Evaluate(expr) => {
                self.lower_expr(expr);
            }
            StmtKind::Return(_) => todo!("return statements are not supported yet"),
            StmtKind::Assign(assign) => return Variables::one(self.lower_assign(assign)),
            StmtKind::If(if_stmt) => return self.lower_if(if_stmt),
            StmtKind::While(while_stmt) => return self.lower_while(while_stmt),
        };

        Variables::none()
    }

    /// Lower a block of statements. Returns the variables that were assigned in this block.
    fn lower_block(&mut self, block: Block) -> Variables {
        Variables::collect(
            block
                .statements
                .into_iter()
                .map(|stmt| self.lower_stmt(stmt)),
        )
    }

    /// Lower an if-statement. If-statements are lowered to one or more conditional jumps, to
    /// jump into the correct code block. Returns the variables that the statement's ɸ-functions
    /// assigned to.
    fn lower_if(&mut self, if_stmt: If) -> Variables {
        let end_lbl = self.label_generator.next_label("if_end");
        let else_lbl = self.label_generator.next_label("if_else");

        let false_label = match if_stmt.else_body.is_some() {
            true => else_lbl.clone(),
            false => end_lbl.clone(),
        };

        self.lower_condition(if_stmt.condition, false_label);

        let live_variables = self.name_generator.get_live_variables();
        let true_variables = self.lower_block(if_stmt.body);
        let mut false_variables = Variables::none();

        if let Some(else_body) = if_stmt.else_body {
            self.emit(InstrKind::Goto(end_lbl.clone()));
            self.emit_label(InstrKind::Nop, else_lbl);
            false_variables = self.lower_block(else_body);
            self.emit(InstrKind::Goto(end_lbl.clone()));
        }

        self.emit_label(InstrKind::Nop, end_lbl);
        self.emit_phi(live_variables, [true_variables, false_variables])
    }

    /// Lower a while-statement. While-statements are lowered to an inverted
    /// conditional jump, which skips past the while-statement body, followed first
    /// by the body, then by a goto that jumps back to the beginning.
    /// Returns the variables that the statement's ɸ-functions assigned to.
    fn lower_while(&mut self, while_stmt: While) -> Variables {
        let start_lbl = self.label_generator.next_label("while_start");
        let end_lbl = self.label_generator.next_label("while_end");

        self.emit_label(InstrKind::Nop, start_lbl.clone());

        self.lower_condition(while_stmt.condition, end_lbl.clone());

        let live_variables = self.name_generator.get_live_variables();
        let block_variables = self.lower_block(while_stmt.body);

        self.emit(InstrKind::Goto(start_lbl));
        self.emit_label(InstrKind::Nop, end_lbl);
        // For the purpose of liveness analysis, a while loop has two branches:
        // in one it is entered at least once (so variables in its block are assigned),
        // in the other it is never entered (so no variables are ever assigned).
        self.emit_phi(live_variables, [block_variables, Variables::none()])
    }

    /// Lower a condition expression as used in an if-statement or a while-statement.
    fn lower_condition(&mut self, mut condition: Expr, false_label: Label) {
        match condition.expr_kind {
            // Ideally we would pattern match against the box here, but we can't.
            ExprKind::Binary(bin) if bin.op.as_compare().is_some() => {
                let op = bin.op.as_compare().unwrap();

                let lhs = self.lower_expr(bin.lhs);
                let rhs = self.lower_expr(bin.rhs);

                // Negate here, because we should jump to the else label if the
                // comparison fails.
                self.emit(InstrKind::IfCmp(lhs, op.negate(), rhs, false_label));
            }
            other => {
                condition.expr_kind = other;
                let cond = self.lower_expr(condition);
                self.emit(InstrKind::IfFalse(cond, false_label));
            }
        };
    }

    /// Given a collection of live variables and the variables assigned in two or
    /// more branches, emit a ɸ-function for each variable assigned in one or more
    /// branches. Returns the variables to which the outcomes of the ɸ-functions
    /// were assigned.
    fn emit_phi<const N: usize>(
        &mut self,
        live_variables: Variables,
        // Ideally we would use const generic bounds to provide a compile-time guarantee
        // that N > 1 as analysing only one branch does not make sense:
        // no ɸ-function would be needed.
        others: [Variables; N],
    ) -> Variables {
        let phi_functions = live_variables.calculate_phi(others.to_vec());
        let mut return_vars = Variables::none();

        for (name, args) in phi_functions {
            let next = self.name_generator.next_subscript(&name);
            let subscripts = args
                .into_iter()
                .map(|sub| Name::Sub(Variable::new(name.clone(), sub)))
                .collect();

            return_vars.insert(next.clone());
            self.emit(InstrKind::Phi(Name::Sub(next), subscripts))
        }
        return_vars
    }

    /// Lower a variable assignment by evaluating an expression and assigning its result to a
    /// variable. Returns the variable that was assigned to.
    fn lower_assign(&mut self, assign: Assign) -> Variable {
        let result = self.lower_expr(assign.value);

        if let ExprKind::Identifier(tgt) = assign.target.expr_kind {
            self.emit_assign(tgt, result)
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
        Value::Name(Name::Sub(self.name_generator.last_subscript(id)))
    }

    /// Lower a function call. Its parameters are pushed onto the parameter stack,
    /// then the function is called.
    fn lower_function_call(&mut self, call: FunCallExpr) -> Value {
        let expr_values: Vec<_> = call.args.into_iter().map(|p| self.lower_expr(p)).collect();

        let arg_count = expr_values.len();
        for arg in expr_values {
            self.emit(InstrKind::Arg(arg));
        }

        let temp_name = self.name_generator.next_temp();
        // TODO: allow calls to other types of functions here.
        self.emit(InstrKind::Call(temp_name.clone(), call.name, arg_count));

        Value::Name(temp_name)
    }

    /// Lower a binary expression. If the expression contains a boolean operator, it is dispatched
    /// to [`Self::lower_bool_expr`].
    fn lower_binexpr(&mut self, expr: BinExpr) -> Value {
        if let BinOp::Bool(bop) = expr.op {
            return self.lower_bool_expr(expr, bop);
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

    /// Emit an assignment, assigning `value` to `id`. Returns the variable that was assigned to.
    fn emit_assign(&mut self, id: String, value: Value) -> Variable {
        let variable = self.name_generator.next_subscript(id);
        self.emit(InstrKind::Assign(Name::Sub(variable.clone()), value));
        variable
    }

    /// Emit an instruction, adding it to the listing.
    fn emit(&mut self, kind: InstrKind) {
        let instr = Instruction::new(kind);
        self.procedure.push(instr);
    }

    /// Emit an instruction, adding it to the listing.
    fn emit_label(&mut self, kind: InstrKind, label: Label) {
        let mut instr = Instruction::new(kind);
        instr.add_label(label);
        self.procedure.push(instr);
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex, parser::parse, type_checking::verify_well_typed};

    use super::*;

    macro_rules! assert_generates {
        ($source:expr, $il:expr) => {{
            let tokens = lex($source).unwrap();
            let program = parse(&tokens).unwrap();
            let program = verify_well_typed(program).unwrap();
            let instrs = generate(program).top_level;

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
                "arg 1",
                "%t1 = call print, 1",
                "if_end_1: nop"
            ]
        )
    }

    #[test]
    fn function_call_generates_param_and_call() {
        assert_generates!("print(999)", ["arg 999", "%t1 = call print, 1",])
    }
}

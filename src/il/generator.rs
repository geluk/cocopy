use std::collections::HashSet;

use crate::{
    ast::{typed::*, untyped::Parameter},
    listing::Position,
};

use super::{label_generator::*, name_generator::*, phi::Variables, tac::*};

pub fn generate(program: Program) -> TacProgram {
    let mut tac = TacGenerator::<TacProgram> {
        procedure: TacProgram::new(),
        name_generator: NameGenerator::new(),
        label_generator: LabelGenerator::new(),
        accesses: vec![],
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
            // TODO: It should now be possible to use a fresh label generator,
            // since our labels are now function-scoped.
            label_generator,
            accesses: vec![],
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
    fn push(&mut self, instruction: TacInstr);
    fn current_line_pos(&self) -> Position;
    fn next_line_pos(&self) -> Position;
}

impl TacProcedure for TacProgram {
    fn push(&mut self, instruction: TacInstr) {
        self.top_level.push(instruction)
    }

    fn current_line_pos(&self) -> Position {
        self.top_level.current_line_pos()
    }

    fn next_line_pos(&self) -> Position {
        self.top_level.next_line_pos()
    }
}
impl TacProcedure for TacListing {
    fn push(&mut self, instruction: TacInstr) {
        self.push(instruction)
    }

    fn current_line_pos(&self) -> Position {
        self.len() - 1
    }

    fn next_line_pos(&self) -> Position {
        self.len()
    }
}

#[derive(Debug, Clone)]
enum VariableAccess {
    Write(Position, Name),
    Read(Position, Name),
}
impl VariableAccess {
    fn position(&self) -> Position {
        match self {
            Self::Write(pos, _) => *pos,
            Self::Read(pos, _) => *pos,
        }
    }
    fn into_write(self) -> Option<(Position, Name)> {
        match self {
            Self::Write(p, n) => Some((p, n)),
            _ => None,
        }
    }
    fn into_read(self) -> Option<(Position, Name)> {
        match self {
            Self::Read(p, n) => Some((p, n)),
            _ => None,
        }
    }
}

struct TacGenerator<P> {
    procedure: P,
    name_generator: NameGenerator,
    label_generator: LabelGenerator,
    accesses: Vec<VariableAccess>,
}
impl<P: TacProcedure> TacGenerator<P> {
    /// Lower a variable definition to an assignment instruction.
    fn lower_var_def(&mut self, var_def: VarDef) {
        self.emit_assign(var_def.name, var_def.value.into());
    }

    /// Lower a parameter definition, retrieving it and storing it in a local variable.
    fn lower_parameter(&mut self, parameter: Parameter) {
        let variable = self.name_generator.next_subscript(parameter.name);
        self.emit(TacInstr::Param(Name::Sub(variable)));
    }

    /// Lower a statement.
    fn lower_stmt(&mut self, stmt: Statement) {
        match stmt.stmt_kind {
            StmtKind::Pass => (),
            StmtKind::Evaluate(expr) => {
                self.lower_expr(expr);
            }
            StmtKind::Return(expr) => self.lower_return(expr),
            StmtKind::Assign(assign) => self.lower_assign(assign),
            StmtKind::If(if_stmt) => self.lower_if(if_stmt),
            StmtKind::While(while_stmt) => self.lower_while(while_stmt),
        }
    }

    fn lower_return(&mut self, opt_expr: Option<Expr>) {
        let return_value = opt_expr.map(|e| self.lower_expr(e));
        self.emit(TacInstr::Return(return_value));
    }

    /// Lower a block of statements. Returns the variables that were assigned in this block.
    fn lower_block(&mut self, block: Block) -> Vec<VariableAccess> {
        let first_line = self.mark_next();

        for stmt in block.statements.into_iter() {
            self.lower_stmt(stmt);
        }

        let last_line = self.mark_current();
        self.accesses_within(first_line, last_line)
    }

    /// Lower an if-statement. If-statements are lowered to one or more conditional jumps, to
    /// jump into the correct code block. Returns the variables that the statement's ɸ-functions
    /// assigned to.
    fn lower_if(&mut self, if_stmt: If) {
        let end_lbl = self.label_generator.next_label("if_end");
        let else_lbl = self.label_generator.next_label("if_else");

        let false_label = match if_stmt.else_body.is_some() {
            true => else_lbl.clone(),
            false => end_lbl.clone(),
        };

        self.lower_condition(if_stmt.condition, false_label);

        let live_variables = self.name_generator.get_live_variables();
        let true_accesses = self.lower_block(if_stmt.body);
        let mut false_accesses = vec![];

        if let Some(else_body) = if_stmt.else_body {
            self.emit(TacInstr::Goto(end_lbl.clone(), vec![]));
            self.emit_label(else_lbl);
            false_accesses = self.lower_block(else_body);
            self.emit(TacInstr::Goto(end_lbl.clone(), vec![]));
        }

        self.emit_label(end_lbl);
        self.emit_phi(live_variables, [true_accesses, false_accesses]);
    }

    /// Lower a while-statement. While-statements are lowered to an inverted
    /// conditional jump, which skips past the while-statement body, followed first
    /// by the body, then by a goto that jumps back to the beginning.
    fn lower_while(&mut self, while_stmt: While) {
        let start_lbl = self.label_generator.next_label("while_start");
        let end_lbl = self.label_generator.next_label("while_end");

        self.emit_label(start_lbl.clone());

        let cond_ops =
            self.record_accesses(|s| s.lower_condition(while_stmt.condition, end_lbl.clone()));

        let live_variables = self.name_generator.get_live_variables();
        let body_accesses = self.lower_block(while_stmt.body);

        let reads = cond_ops
            .into_iter()
            .chain(body_accesses.clone().into_iter())
            .filter_map(|o| o.into_read().map(|(_, n)| n))
            // Poor man's `unique()`
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();

        self.emit(TacInstr::Goto(start_lbl, reads));

        self.emit_label(end_lbl);
        // For the purpose of liveness analysis, a while loop has two branches:
        // in one it is entered at least once (so variables in its body are assigned),
        // in the other it is never entered (so no variables are ever assigned).
        self.emit_phi(live_variables, [body_accesses, vec![]]);
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
                self.emit(TacInstr::IfCmp(lhs, op.negate(), rhs, false_label));
            }
            other => {
                condition.expr_kind = other;
                let cond = self.lower_expr(condition);
                self.emit(TacInstr::IfFalse(cond, false_label));
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
        others: [Vec<VariableAccess>; N],
    ) {
        // Ideally we would use const generic bounds to provide a compile-time guarantee
        // that N > 1 as analysing only one branch does not make sense:
        // no ɸ-function would be needed.
        assert!(N > 1);

        fn collect_writes(operations: Vec<VariableAccess>) -> Variables {
            let writes = operations
                .into_iter()
                .filter_map(|o| o.into_write())
                .filter_map(|(_, n)| n.into_sub())
                .map(|s| (s.name, s.subscript))
                .collect();

            Variables::new(writes)
        }
        let branches = others.into_iter().map(collect_writes).collect();
        let phi_functions = live_variables.calculate_phi(branches);

        for (name, args) in phi_functions {
            let next = self.name_generator.next_subscript(&name);
            let subscripts = args
                .into_iter()
                .map(|sub| Name::Sub(Variable::new(name.clone(), sub)))
                .collect();

            self.emit(TacInstr::Phi(Name::Sub(next), subscripts))
        }
    }

    /// Lower a variable assignment by evaluating an expression and assigning its
    /// result to the given variable.
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
            ExprKind::Literal(lit) => lit.into(),
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

    /// Lower a function call. Its parameters are pushed onto the parameter stack,
    /// then the function is called.
    fn lower_function_call(&mut self, call: FunCallExpr) -> Value {
        let expr_values: Vec<_> = call.args.into_iter().map(|p| self.lower_expr(p)).collect();

        let temp_name = self.name_generator.next_temp();
        // TODO: allow calls to other types of functions here.
        self.emit(TacInstr::Call(
            Some(temp_name.clone()),
            call.name,
            expr_values,
        ));

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
        self.emit(TacInstr::Bin(res_name.clone(), expr.op, lhs, rhs));
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
                self.emit(TacInstr::IfTrue(lhs, true_lbl.clone()));
            }
            BoolOp::And => {
                self.emit(TacInstr::IfFalse(lhs, false_lbl.clone()));
            }
        }

        let rhs = self.lower_expr(expr.rhs);
        match op {
            BoolOp::Or => {
                self.emit(TacInstr::IfFalse(rhs, false_lbl.clone()));
            }
            BoolOp::And => {
                self.emit(TacInstr::IfFalse(rhs, false_lbl.clone()));
            }
        }

        let res_name = self.name_generator.next_temp();
        self.emit_label(true_lbl);
        self.emit(TacInstr::Assign(res_name.clone(), Value::Const(1)));
        self.emit(TacInstr::Goto(end_lbl.clone(), vec![]));
        self.emit_label(false_lbl);
        self.emit(TacInstr::Assign(res_name.clone(), Value::Const(0)));
        self.emit_label(end_lbl);
        self.emit(TacInstr::Phi(res_name.clone(), vec![res_name.clone()]));
        Value::Name(res_name)
    }

    /// Emit an assignment, assigning `value` to `id`. Returns the variable that was assigned to.
    fn emit_assign(&mut self, id: String, value: Value) {
        let variable = self.name_generator.next_subscript(id);
        self.emit(TacInstr::Assign(Name::Sub(variable), value));
    }

    /// Emit an instruction, adding it to the listing.
    fn emit(&mut self, instr: TacInstr) {
        let reads = instr.reads();
        let write = instr.write().cloned();

        self.procedure.push(instr);
        let position = self.mark_current();

        for name in reads {
            self.accesses
                .push(VariableAccess::Read(position, name.clone()));
        }
        if let Some(name) = write {
            self.accesses.push(VariableAccess::Write(position, name))
        }
    }

    /// Emit a label, adding it to the listing.
    fn emit_label(&mut self, label: Label) {
        let instr = TacInstr::Label(label);
        self.procedure.push(instr);
    }

    /// Convert an identifier to a [`Value`]. This does not result in the emission
    /// of intermediate code.
    fn convert_identifier(&self, id: String) -> Value {
        Value::Name(Name::Sub(self.name_generator.last_subscript(id)))
    }

    /// Record variable accesses that occur during execution of the given closure.
    fn record_accesses<F: FnOnce(&mut Self)>(&mut self, f: F) -> Vec<VariableAccess> {
        let first_line = self.mark_next();

        f(self);

        let last_line = self.mark_current();
        self.accesses_within(first_line, last_line)
    }

    /// Get a position marker for the current (already emitted) line.
    fn mark_current(&self) -> Position {
        self.procedure.current_line_pos()
    }

    /// Get a position marker for the next line to be emitted.
    fn mark_next(&self) -> Position {
        self.procedure.next_line_pos()
    }

    /// Returns the variable accesses performed within the given range (inclusive, inclusive).
    fn accesses_within(&self, start: Position, end: Position) -> Vec<VariableAccess> {
        self.accesses
            .iter()
            .filter(|o| o.position() >= start && o.position() <= end)
            .cloned()
            .collect()
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

            let instr_lines: Vec<_> = instrs.into_instructions().map(|i| i.to_string()).collect();

            assert_eq!(&$il[..], instr_lines)
        }};
    }

    #[test]
    fn simple_program_generates_tac() {
        assert_generates!(
            "a:int = 10\na = a + (100 + 1)",
            ["a^1 = 10", "%1 = 100 + 1", "%2 = a^1 + %1", "a^2 = %2",]
        )
    }

    #[test]
    fn if_stmt_generates_tac() {
        assert_generates!(
            "a:bool = True\nif a:\n\tprint(1)",
            [
                "a^1 = 1",
                "if_false a^1 goto if_end_1",
                "%1 = call print (1)",
                "if_end_1:"
            ]
        )
    }

    #[test]
    fn function_call_generates_param_and_call() {
        assert_generates!("print(999)", ["%1 = call print (999)",])
    }
}

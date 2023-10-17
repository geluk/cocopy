use std::{collections::VecDeque, mem};

use crate::{ast::typed::*, ext::ordered_hash_map::OrderedHashMap, listing::Position};

use super::{label_generator::*, name_generator::*, tac::*};

#[derive(Default)]
struct IterableBlock {
    var_defs: VecDeque<VarDef>,
    statements: VecDeque<Statement>,
}

pub fn generate(program: Program) -> TacProgram {
    let main_block = IterableBlock {
        var_defs: program.var_defs.into(),
        statements: program.statements.into(),
    };
    let (entry_block, child_blocks) = TacGenerator::generate(vec![], main_block);

    let mut tac_program = TacProgram::new(TacProcedure::new(entry_block, child_blocks));

    for func_def in program.func_defs {
        let (name, func) = generate_func(func_def);

        tac_program.functions.insert(name, func);
    }

    tac_program
}

fn generate_func(func_def: FuncDef) -> (String, TacProcedure) {
    let parameters = func_def.parameters.into_iter().map(|p| p.name).collect();
    let func_block = IterableBlock {
        var_defs: VecDeque::new(),
        statements: func_def.body.statements.into(),
    };

    let (entry_block, child_blocks) = TacGenerator::generate(parameters, func_block);

    (func_def.name, TacProcedure::new(entry_block, child_blocks))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BlockLabel {
    Entry,
    Label(Label),
}

struct Accesses {
    reads: Vec<(Position, Name)>,
    writes: Vec<(Position, Name)>,
}
impl Accesses {
    pub fn new() -> Self {
        Self {
            reads: vec![],
            writes: vec![],
        }
    }

    fn push_read(&mut self, position: Position, name: Name) {
        self.reads.push((position, name));
    }

    fn push_write(&mut self, position: Position, name: Name) {
        self.writes.push((position, name));
    }
}

struct TacGenerator {
    current_label: BlockLabel,
    current_block: BasicBlock,
    child_blocks: OrderedHashMap<BlockLabel, BasicBlock>,
    name_generator: NameGenerator,
    label_generator: LabelGenerator,
    iterable: IterableBlock,
    accesses: Accesses,
}
impl TacGenerator {
    fn new(
        parameters: Vec<Name>,
        iterable: IterableBlock,
        label_generator: LabelGenerator,
    ) -> Self {
        let mut name_generator = NameGenerator::new();

        for param in parameters.iter() {
            if let Name::Sub(sub) = &param {
                name_generator.register_generation(sub);
            }
        }

        let start_block = BasicBlock::new(parameters);

        Self {
            current_label: BlockLabel::Entry,
            current_block: start_block,
            child_blocks: Default::default(),
            name_generator,
            label_generator,
            iterable,
            accesses: Accesses::new(),
        }
    }

    fn generate(
        parameters: Vec<String>,
        iterable: IterableBlock,
    ) -> (BasicBlock, OrderedHashMap<Label, BasicBlock>) {
        let mut name_generator = NameGenerator::new();

        let parameters = parameters
            .into_iter()
            .map(|n| Name::Sub(name_generator.next_subscript(n)))
            .collect();

        let generator = Self::new(parameters, iterable, LabelGenerator::new());
        generator.generate_internal()
    }

    fn generate_internal(mut self) -> (BasicBlock, OrderedHashMap<Label, BasicBlock>) {
        while let Some(var_def) = self.iterable.var_defs.pop_front() {
            self.lower_var_def(var_def);
        }

        while let Some(stmt) = self.iterable.statements.pop_front() {
            self.lower_stmt(stmt);
        }

        let (_, blocks) = self.finish();

        let mut entry_block = None;
        let others = blocks
            .into_iter()
            .filter_map(|(label, block)| match label {
                BlockLabel::Entry => {
                    entry_block = Some(block);
                    None
                }
                BlockLabel::Label(lbl) => Some((lbl, block)),
            })
            .collect();

        (entry_block.expect("No entry block was present"), others)
    }

    fn finish(mut self) -> (Accesses, OrderedHashMap<BlockLabel, BasicBlock>) {
        self.child_blocks
            .insert(self.current_label, self.current_block);

        (self.accesses, self.child_blocks)
    }

    /// Lower a variable definition to an assignment instruction.
    fn lower_var_def(&mut self, var_def: VarDef) {
        let variable = self.name_generator.next_subscript(var_def.name);
        self.emit_assign(variable, var_def.value.into());
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
    fn lower_block(&mut self, block: Block, label: Label, next: Label) -> Accesses {
        let live_variables = self.live_variables();
        self.branch(label, live_variables, |inner| {
            for stmt in block.statements.into_iter() {
                inner.lower_stmt(stmt);
            }

            let live_variables = inner.live_variables();
            inner.emit(TacInstr::Goto(next, live_variables));
        })
    }

    /// Lower an if-statement. If-statements are lowered to one or more conditional jumps, to
    /// jump into the correct code block. Returns the variables that the statement's ɸ-functions
    /// assigned to.
    fn lower_if(&mut self, if_stmt: If) {
        let true_lbl = self.label_generator.next_label("if_body");
        let else_lbl = self.label_generator.next_label("if_else");
        let end_lbl = self.label_generator.next_label("if_end");

        let false_label = match if_stmt.else_body.is_some() {
            true => else_lbl.clone(),
            false => end_lbl.clone(),
        };

        self.lower_condition(if_stmt.condition, true_lbl.clone(), false_label);

        self.lower_block(if_stmt.body, true_lbl, end_lbl.clone());

        if let Some(else_body) = if_stmt.else_body {
            self.lower_block(else_body, else_lbl, end_lbl.clone());
        }

        let live_variables = self.live_variables();
        self.next(end_lbl, live_variables);
    }

    /// Lower a while-statement. While-statements are lowered to an inverted
    /// conditional jump, which skips past the while-statement body, followed first
    /// by the body, then by a goto that jumps back to the beginning.
    fn lower_while(&mut self, while_stmt: While) {
        let start_lbl = self.label_generator.next_label("while_start");
        let body_lbl = self.label_generator.next_label("while_body");
        let end_lbl = self.label_generator.next_label("while_end");

        let live_variables = self.live_variables();
        self.emit(TacInstr::Goto(start_lbl.clone(), live_variables.clone()));

        self.branch(start_lbl.clone(), live_variables.clone(), |inner| {
            inner.lower_condition(while_stmt.condition, body_lbl.clone(), end_lbl.clone());
        });

        self.lower_block(while_stmt.body, body_lbl, start_lbl);

        self.next(end_lbl, live_variables);
    }

    /// Lower a condition expression as used in an if-statement or a while-statement.
    fn lower_condition(&mut self, mut condition: Expr, true_label: Label, false_label: Label) {
        match condition.expr_kind {
            // Ideally we would pattern match against the box here, but we can't.
            ExprKind::Binary(bin) if bin.op.as_compare().is_some() => {
                let op = bin.op.as_compare().unwrap();

                let lhs = self.lower_expr(bin.lhs);
                let rhs = self.lower_expr(bin.rhs);

                let live_variables = self.live_variables();

                // Negate here, because we should jump to the else label if the
                // comparison fails.
                self.emit(TacInstr::IfCmp(
                    lhs,
                    op,
                    rhs,
                    true_label,
                    false_label,
                    live_variables,
                ));
            }
            other => {
                condition.expr_kind = other;
                let cond = self.lower_expr(condition);
                let live_variables = self.live_variables();
                self.emit(TacInstr::If(cond, true_label, false_label, live_variables));
            }
        };
    }

    /// Lower a variable assignment by evaluating an expression and assigning its
    /// result to the given variable.
    fn lower_assign(&mut self, assign: Assign) {
        if let ExprKind::Identifier(tgt) = assign.target.expr_kind {
            let tgt = self.name_generator.next_subscript(tgt);
            let result = self.lower_expr(assign.value);
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

    /// Lower a function call.
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
        let next_lbl = self.label_generator.next_label("bexpr_n");
        let end_lbl = self.label_generator.next_label("bexpr_e");

        let lhs = self.lower_expr(expr.lhs);

        let live_variables = self.live_variables();

        match op {
            BoolOp::Or => {
                self.emit(TacInstr::If(
                    lhs,
                    true_lbl.clone(),
                    next_lbl.clone(),
                    live_variables.clone(),
                ));
            }
            BoolOp::And => {
                self.emit(TacInstr::If(
                    lhs,
                    next_lbl.clone(),
                    false_lbl.clone(),
                    live_variables.clone(),
                ));
            }
        }

        self.branch(next_lbl, live_variables.clone(), |branch| {
            let rhs = branch.lower_expr(expr.rhs);
            match op {
                BoolOp::Or => {
                    branch.emit(TacInstr::If(
                        rhs,
                        true_lbl.clone(),
                        false_lbl.clone(),
                        live_variables.clone(),
                    ));
                }
                BoolOp::And => {
                    branch.emit(TacInstr::If(
                        rhs,
                        true_lbl.clone(),
                        false_lbl.clone(),
                        live_variables.clone(),
                    ));
                }
            }
        });

        let res_name = self.name_generator.next_temp();
        let mut live_with_temp = live_variables.clone();
        live_with_temp.push(res_name.clone());

        self.branch(true_lbl, live_variables.clone(), |branch| {
            branch.emit(TacInstr::Assign(res_name.clone(), Value::Const(1)));

            branch.emit(TacInstr::Goto(end_lbl.clone(), live_with_temp.clone()));
        });
        self.branch(false_lbl, live_variables.clone(), |branch| {
            branch.emit(TacInstr::Assign(res_name.clone(), Value::Const(0)));

            branch.emit(TacInstr::Goto(end_lbl.clone(), live_with_temp.clone()));
        });

        // Signal emission of new basic block
        self.next(end_lbl, live_with_temp);

        Value::Name(res_name)
    }

    /// Create a branching block.
    fn branch<F>(&mut self, label: Label, parameters: Vec<Name>, f: F) -> Accesses
    where
        F: FnOnce(&mut Self),
    {
        let label_generator = mem::replace(&mut self.label_generator, LabelGenerator::new());

        let mut inner = Self::new(parameters, IterableBlock::default(), label_generator);
        f(&mut inner);

        mem::swap(&mut self.label_generator, &mut inner.label_generator);
        let (accesses, mut blocks) = inner.finish();

        let inner_entry = blocks.remove(&BlockLabel::Entry).unwrap();
        blocks.insert(BlockLabel::Label(label), inner_entry);

        self.child_blocks.extend(blocks.into_iter());

        accesses
    }

    /// Signal the start of a new block.
    fn next(&mut self, label: Label, live_names: Vec<Name>) {
        let previous_label = mem::replace(&mut self.current_label, BlockLabel::Label(label));
        let previous_block = mem::replace(&mut self.current_block, BasicBlock::new(live_names));

        self.child_blocks.insert(previous_label, previous_block);
    }

    fn live_variables(&self) -> Vec<Name> {
        self.name_generator
            .get_live_variables()
            .map(Name::Sub)
            .collect()
    }

    /// Emit an assignment, assigning `value` to `id`. Returns the variable that was assigned to.
    fn emit_assign(&mut self, variable: Variable, value: Value) {
        self.emit(TacInstr::Assign(Name::Sub(variable), value));
    }

    /// Emit an instruction, adding it to the listing.
    fn emit(&mut self, instr: TacInstr) {
        let reads = instr.reads();
        let write = instr.write().cloned();

        self.current_block.push(instr);
        let position = self.mark_current();

        for name in reads {
            self.accesses.push_read(position, name.clone());
        }
        if let Some(name) = write {
            self.accesses.push_write(position, name)
        }
    }

    /// Convert an identifier to a [`Value`]. This does not result in the emission
    /// of intermediate code.
    fn convert_identifier(&self, id: String) -> Value {
        Value::Name(Name::Sub(self.name_generator.last_subscript(id)))
    }

    /// Get a position marker for the current (already emitted) line.
    fn mark_current(&self) -> Position {
        self.current_block.current_line_pos()
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
            let mut program = generate(program);
            let instrs = program.top_level.entry_block_mut();

            let instr_lines: Vec<_> = instrs
                .listing()
                .iter_instructions()
                .map(|i| i.to_string())
                .collect();

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

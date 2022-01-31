use crate::{ast::untyped::BinOp, builtins::Builtin, il::*};

use super::{assembly::*, register_allocator::RegisterAllocator, x86::*};

use Op::*;
use Operand::*;
use Register::*;

pub struct ProcedureCompiler {
    allocator: RegisterAllocator,
    procedure: Procedure,
    param_stack: Vec<Operand>,
    pending_label: Option<Label>,
}
impl ProcedureCompiler {
    pub fn compile(listing: TacListing, procedure: Procedure) -> Procedure {
        let mut compiler = Self {
            allocator: RegisterAllocator::new(),
            procedure,
            param_stack: vec![],
            pending_label: None,
        };
        compiler.compile_tac(listing);
        compiler.procedure
    }

    /// Compile a three-address code listing.
    fn compile_tac(&mut self, listing: TacListing) {
        for (_, instr) in listing.iter_lines() {
            self.compile_instr(instr);
            // TODO: After each instruction has been compiled, iterate over allocated registers,
            // and free those whose value will no longer be used.
        }
    }

    /// Compile a single TAC instruction.
    fn compile_instr(&mut self, instr: &Instruction) {
        if let Some(lbl) = &instr.label {
            self.pending_label = Some(lbl.clone());
        }
        let comment = instr.to_string();
        match &instr.kind {
            InstrKind::Assign(tgt, value) => {
                self.compile_assign(tgt.clone(), value.clone(), comment)
            }
            InstrKind::Bin(tgt, op, left, right) => {
                self.compile_bin(tgt.clone(), *op, left.clone(), right.clone(), comment)
            }
            InstrKind::Param(param) => self.compile_param(param.clone()),
            InstrKind::Call(tgt, name, params) => self.compile_call(tgt.clone(), *name, *params),
            InstrKind::Nop => {
                self.emit(Nop, vec![]);
            }
            InstrKind::IfTrue(value, lbl) => self.compile_jump(value.clone(), lbl.clone(), true),
            InstrKind::IfFalse(value, lbl) => self.compile_jump(value.clone(), lbl.clone(), false),
        }
    }

    /// Compile a conditional jump. A jump will be made if `value` evaluates to `jump_when`.
    /// This allows this function to compile both `if_true x goto label` and `if_false y goto label`.
    fn compile_jump(&mut self, value: Value, label: Label, jump_when: bool) {
        let jump_type = match jump_when {
            true => Jnz,
            false => Jz,
        };
        let val_op = self.get_operand(value.clone());
        // RAX may be occupied, so we should free it first.
        // This can be removed once we support operator semantics.
        self.free(Rax);
        self.emit_cmt(Mov, vec![Reg(Rax), val_op], "place operand in RAX")
            .emit_cmt(
                Test,
                vec![Reg(Rax), Reg(Rax)],
                format!("<if_true> {}", value),
            );
        // We're done with RAX, restore it.
        self.restore(Rax);
        self.emit_cmt(
            jump_type,
            vec![Lbl(label.to_string())],
            format!("jump when condition is {}", jump_when),
        );
    }

    /// Compile an assignment.
    fn compile_assign(&mut self, target: Name, value: Value, comment: String) {
        let target = self.allocator.bind(target);
        let value = self.get_operand(value);

        self.emit_cmt(Mov, vec![Reg(target), value], comment);
    }

    /// Compile a binary operation.
    fn compile_bin(&mut self, tgt: Name, op: BinOp, left: Value, right: Value, comment: String) {
        let target = self.allocator.bind(tgt);
        let op = translate_binop(op);
        let left = self.get_operand(left);
        let right = self.get_operand(right);

        // Binary operations of the form `x = y <> z` (where <> is some operation) cannot be
        // compiled in one go, so we translate them to the following sequence:
        // x <- y
        // x <>= z
        self.emit_cmt(Mov, vec![Reg(target), left], format!("<store> {}", comment));
        self.emit_cmt(op, vec![Reg(target), right], format!("<apply> {}", comment));
    }

    /// Compile a function call.
    fn compile_call(&mut self, tgt: Name, name: Builtin, param_count: usize) {
        let tgt = self.allocator.bind(tgt);

        if param_count > 4 {
            todo!("Can't deal with more than 4 parameters yet.");
        }

        // TODO: this should be made to work for any calling convention, not just fastcall.
        let target_regs = [Rcx, Rdx, R8, R9];
        for (idx, &reg) in target_regs[0..param_count].iter().enumerate() {
            let value = self.param_stack.pop().expect("Parameter count mismatch!");
            self.emit_cmt(
                Mov,
                vec![Reg(reg), value],
                format!("set parameter #{}", idx + 1),
            );
        }

        let name = match name {
            Builtin::Print => "print",
        };

        self.emit_cmt(Call, vec![Id(name)], "call print").emit_cmt(
            Mov,
            vec![Reg(tgt), Reg(Rax)],
            "store return value",
        );
    }

    /// Compile a function parameter.
    fn compile_param(&mut self, param: Value) {
        let param = self.get_operand(param);

        self.param_stack.push(param);
    }

    /// Retrieves the operand for a value.
    fn get_operand(&mut self, value: Value) -> Operand {
        match value {
            Value::Const(lit) => Lit(lit as i128),
            Value::Name(name) => Reg(self.allocator.bind(name)),
        }
    }

    /// Free a register, storing its value on the stack.
    fn free(&mut self, register: Register) {
        self.emit_cmt(Push, vec![Reg(register)], format!("free {}", register));
    }

    /// Restore a register, popping its value from the top of the stack.
    fn restore(&mut self, register: Register) {
        self.emit_cmt(Pop, vec![Reg(register)], format!("restore {}", register));
    }

    /// Emit an operation.
    fn emit(&mut self, op: Op, operands: Vec<Operand>) -> &mut Self {
        self.procedure.body.push(op, operands);
        if let Some(label) = self.pending_label.take() {
            self.procedure.body.add_label(label.to_string());
        }
        self
    }

    /// Emit an operation with comment.
    fn emit_cmt<S: Into<String>>(
        &mut self,
        op: Op,
        operands: Vec<Operand>,
        comment: S,
    ) -> &mut Self {
        self.procedure.body.push_cmt(op, operands, comment);
        if let Some(label) = self.pending_label.take() {
            self.procedure.body.add_label(label.to_string());
        }
        self
    }
}

fn translate_binop(op: BinOp) -> Op {
    match op {
        BinOp::Add => Add,
        BinOp::Multiply => Imul,
        BinOp::Subtract => Sub,
        _ => todo!("Don't know how to translate {:?} to assembly", op),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! sub {
        ($name:expr) => {
            Name::Sub($name.to_string(), 1)
        };
    }

    macro_rules! cnst {
        ($value:expr) => {
            Value::Const($value)
        };
    }

    macro_rules! listing {
        ($instr:expr) => {{
            let mut listing = TacListing::new();
            listing.push(Instruction::new($instr));
            listing
        }};
    }

    macro_rules! compile_listing {
        ($instr:expr) => {{
            let listing = listing!($instr);
            let proc = Procedure::new("main", Block::new(), Block::new());
            let proc = ProcedureCompiler::compile(listing, proc);
            println!(
                "Listing source:\n===============\n{}===============\n",
                proc
            );
            proc
        }};
    }

    #[test]
    fn compile_tac_compiles_instruction_to_assembly() {
        let procedure =
            compile_listing!(InstrKind::Bin(sub!("x"), BinOp::Add, cnst!(10), cnst!(99),));

        let mut expected = Block::new();
        expected.push(Mov, vec![Reg(Rax), Lit(10)]);
        expected.push(Add, vec![Reg(Rax), Lit(99)]);

        assert_eq!(expected, procedure.body)
    }

    #[test]
    fn can_compile_multiplication() {
        compile_listing!(InstrKind::Bin(
            sub!("x"),
            BinOp::Multiply,
            cnst!(10),
            cnst!(3),
        ));
    }
}

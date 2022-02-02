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
    /// Compile the given source code into the given procedure.
    pub fn compile(listing: TacListing, procedure: Procedure) -> Procedure {
        let mut compiler = Self::new(procedure);
        compiler.compile_tac(listing);
        compiler.procedure
    }

    /// Construct a new procedure compiler for the given procedure.
    fn new(procedure: Procedure) -> Self {
        Self {
            allocator: RegisterAllocator::new(),
            procedure,
            param_stack: vec![],
            pending_label: None,
        }
    }

    /// Compile a three-address code listing.
    fn compile_tac(&mut self, listing: TacListing) {
        for (idx, instr) in listing.iter_lines() {
            self.compile_instr(instr);

            let may_unbind: Vec<_> = self
                .allocator
                .iter_bound_names()
                .filter(|name| !listing.is_used_after(name, idx))
                .cloned()
                .collect();
            for name in may_unbind {
                self.allocator.unbind(&name);
            }
        }
        if self.pending_label.is_some() {
            self.emit_cmt(Nop, [], "insert trailing label");
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
            InstrKind::Nop => (),
            InstrKind::IfTrue(value, lbl) => {
                self.compile_jump(value.clone(), lbl.clone(), true, comment)
            }
            InstrKind::IfFalse(value, lbl) => {
                self.compile_jump(value.clone(), lbl.clone(), false, comment)
            }
            InstrKind::Phi(_) => (), // Phi is a no-op
            InstrKind::Goto(tgt) => {
                self.emit(Jmp, [Lbl(tgt.to_string())]);
            }
        }
    }

    /// Compile a conditional jump. A jump will be made if `value` evaluates to `jump_when`.
    /// This allows this function to compile both `if_true x goto label` and `if_false y goto label`.
    fn compile_jump(&mut self, value: Value, label: Label, jump_when: bool, comment: String) {
        let jump_type = match jump_when {
            true => Jnz,
            false => Jz,
        };

        let val_op = self.prepare_operand(value, Test.op1_semantics());
        self.emit_cmt(
            Test,
            [val_op.operand(), val_op.operand()],
            format!("<jump> {}", comment),
        );
        self.release_prepared(val_op);
        self.emit(jump_type, [Lbl(label.to_string())]);
    }

    /// Compile an assignment.
    fn compile_assign(&mut self, target: Name, value: Value, comment: String) {
        let target = self.allocator.bind(target);
        let value = self.get_operand(value);

        self.emit_cmt(Mov, [Reg(target), value], comment);
    }

    /// Compile a binary operation. Dispatches the operation for the correct compile fuction
    /// if the operation represents a comparison or (TODO) boolean operation.
    fn compile_bin(&mut self, tgt: Name, op: BinOp, left: Value, right: Value, comment: String) {
        // Comparisons are handled separately, since they should produce a boolean.
        if let Ok(cmp) = op.try_into() {
            self.compile_cmp(tgt, cmp, left, right, comment);
            return;
        }

        // Integer division and remainder are handled specially.
        if op == BinOp::IntDiv || op == BinOp::Remainder {
            // The lower half of the dividend goes in RDX.
            // This is also where the remainder is stored.
            self.reserve(Rdx);
            // The upper half of the dividend goes in RAX.
            // This is also where the quotient is stored.
            let dividend = self.prepare_operand(left, OpSemantics::rax_only());
            let divisor = self.prepare_operand(right, Idiv.op1_semantics());

            self.emit_cmt(Xor, [Reg(Rdx), Reg(Rdx)], "<div> clear upper half")
                .emit_cmt(Idiv, [divisor.operand()], format!("<div> {}", comment));

            match op {
                BinOp::IntDiv => {
                    self.allocator.bind_to(tgt, Rax);
                    self.allocator.release(Rdx);
                }
                BinOp::Remainder => {
                    self.allocator.bind_to(tgt, Rdx);
                    self.release_prepared(dividend);
                }
                _ => unreachable!(),
            }

            self.release_prepared(divisor);
            return;
        }

        let target = self.allocator.bind(tgt);
        let op = translate_binop(op);
        let left = self.get_operand(left);
        let right = self.get_operand(right);

        // Binary operations of the form `x = y <> z` (where <> is some operation) cannot be
        // compiled in one go, so we translate them to the following sequence:
        // x <- y
        // x <>= z
        self.emit_cmt(Mov, [Reg(target), left], format!("<store> {}", comment));
        self.emit_cmt(op, [Reg(target), right], format!("<apply> {}", comment));
    }

    /// Compile a comparison between two values.
    fn compile_cmp(&mut self, tgt: Name, cmp: Cmp, left: Value, right: Value, comment: String) {
        let left_op = self.prepare_operand(left, OpSemantics::any_reg());
        let right_op = self.get_operand(right);

        let target = self.allocator.bind(tgt);
        self.emit_cmt(
            Xor,
            [Reg(target), Reg(target)],
            format!("clear upper bytes of {}", target),
        )
        .emit_cmt(
            Cmp,
            [left_op.operand(), right_op],
            format!("<compare> {}", comment),
        );
        // The x86 `set` operation copies a comparison flag into a register,
        // allowing us to treat the value as a boolean.
        let set_op = match cmp {
            Cmp::Gt => Setg,
            Cmp::Gte => Setge,
            Cmp::Lt => Setl,
            Cmp::Lte => Setle,
            Cmp::Eq => Sete,
            Cmp::Neq => Setne,
        };
        self.emit_cmt(
            set_op,
            [Reg(target.into_byte())],
            format!("<store> {}", comment),
        );

        self.release_prepared(left_op);
    }

    /// Compile a call to a function with `param_count` parameters.
    fn compile_call(&mut self, tgt: Name, name: Builtin, param_count: usize) {
        if param_count > 4 {
            todo!("Can't deal with more than 4 parameters yet.");
        }

        // TODO: this should be made to work for any calling convention, not just fastcall.
        let target_regs = [Rcx, Rdx, R8, R9];
        for (idx, &reg) in target_regs[0..param_count].iter().enumerate() {
            let value = self.param_stack.pop().expect("Parameter count mismatch!");

            self.reserve(reg);
            self.emit_cmt(
                Mov,
                [Reg(reg), value],
                format!("set parameter #{}", idx + 1),
            );
        }

        // The return value will be placed in RAX.
        self.reserve(Rax);
        self.allocator.bind_to(tgt, Rax);

        let name = match name {
            Builtin::Print => "print",
        };

        self.emit_cmt(Call, [Id(name)], "call print");

        for &reg in target_regs[0..param_count].iter() {
            self.allocator.release(reg);
        }
    }

    /// Compile a function parameter.
    fn compile_param(&mut self, param: Value) {
        let param = self.get_operand(param);

        self.param_stack.push(param);
    }

    /// Prepares an operand with a value according to the given operand semantics.
    /// If the value is a literal, but the operand should be a register,
    /// it is first placed in a temporary register. Call `free_operand` after using
    /// the operand to ensure the temporary register is freed again.
    fn prepare_operand(&mut self, value: Value, op_smt: OpSemantics) -> PreparedOperand {
        match &value {
            Value::Const(cnst) if op_smt.immediate => {
                // An immediate value operand is accepted, so we can just pass it on.
                return PreparedOperand::other(Operand::Lit(*cnst as i128));
            }
            Value::Name(name) if op_smt.register == AcceptsReg::AnyReg => {
                if let Some(reg) = self.allocator.lookup(name) {
                    // Any register is accepted, and we already have a register binding for this
                    // operand.
                    return PreparedOperand::other(Operand::Reg(reg));
                }
            }
            Value::Name(name)
                if op_smt.register == AcceptsReg::RaxOnly
                    && self.allocator.lookup(name) == Some(Rax) =>
            {
                // Only RAX is accepted, but this variable already happens to be bound to RAX.
                return PreparedOperand::other(Operand::Reg(Rax));
            }
            _ => (),
        }

        // We now know we need to put the operand in some register.
        // First, let's allocate the right register.
        let target_reg = match op_smt.register {
            AcceptsReg::AnyReg => self
                .allocator
                .allocate()
                .expect("Ran out of registers to allocate"),
            AcceptsReg::RaxOnly => {
                self.reserve(Rax);
                Rax
            }
        };

        match value {
            Value::Const(cnst) => {
                self.emit_cmt(
                    Mov,
                    [Reg(target_reg), Lit(cnst as i128)],
                    format!("<op_sem> prepare operand {}", value),
                );
            }
            Value::Name(name) => match self.allocator.lookup(&name) {
                Some(orig_reg) => {
                    self.emit_cmt(
                        Mov,
                        [Reg(target_reg), Reg(orig_reg)],
                        "<op_sem> move operand into correct register".to_string(),
                    );
                }
                None => {
                    panic!("Attempted to prepare an operand for a name that was not allocated yet.")
                }
            },
        }
        PreparedOperand::temp(Reg(target_reg))
    }

    /// Releases a prepared operand. If a temporary register was allocated for this operand,
    /// it is released.
    fn release_prepared(&mut self, prep_op: PreparedOperand) {
        if let (true, Reg(reg)) = (prep_op.is_temp, prep_op.operand) {
            self.allocator.release(reg);
        }
    }

    /// Retrieves the operand for a value. Constants are passed through as literal operands,
    /// while names are bound to a register before being returned as register operands.
    fn get_operand(&mut self, value: Value) -> Operand {
        match value {
            Value::Const(lit) => Lit(lit as i128),
            Value::Name(name) => Reg(self.allocator.bind(name)),
        }
    }

    /// Reserves a register for usage, moving its value somewhere else if it was already allocated.
    fn reserve(&mut self, register: Register) {
        if self.allocator.is_free(register) {
            self.allocator.allocate_reg(register);
            return;
        }

        let renamed_reg = self
            .allocator
            .allocate()
            .expect("Ran out of registers to allocate");
        self.emit_cmt(
            Mov,
            [Reg(renamed_reg), Reg(register)],
            format!("<reserve> {} -> {}", register, renamed_reg),
        );
        self.allocator.notify_move(register, renamed_reg);
    }

    /// Emit an operation.
    fn emit<V: Into<Vec<Operand>>>(&mut self, op: Op, operands: V) -> &mut Self {
        self.procedure.body.push(op, operands.into());
        if let Some(label) = self.pending_label.take() {
            self.procedure.body.add_label(label.to_string());
        }
        self
    }

    /// Emit an operation with comment.
    fn emit_cmt<V: Into<Vec<Operand>>, S: Into<String>>(
        &mut self,
        op: Op,
        operands: V,
        comment: S,
    ) -> &mut Self {
        self.procedure.body.push_cmt(op, operands.into(), comment);
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
        BinOp::IntDiv => Idiv,
        BinOp::Subtract => Sub,
        _ => todo!("Don't know how to translate {:?} to assembly", op),
    }
}

struct PreparedOperand {
    operand: Operand,
    is_temp: bool,
}
impl PreparedOperand {
    fn other(operand: Operand) -> Self {
        Self {
            operand,
            is_temp: false,
        }
    }
    fn temp(operand: Operand) -> Self {
        Self {
            operand,
            is_temp: true,
        }
    }
    fn operand(&self) -> Operand {
        self.operand.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AcceptsReg {
    AnyReg,
    RaxOnly,
}
impl Default for AcceptsReg {
    fn default() -> Self {
        Self::AnyReg
    }
}

/// Describes how an operand may be used.
#[derive(Debug, Default, Clone, Copy)]
struct OpSemantics {
    immediate: bool,
    register: AcceptsReg,
}
impl OpSemantics {
    /// The operand may be placed in memory or in any register.
    fn any_reg_or_mem() -> Self {
        Self {
            register: AcceptsReg::AnyReg,
            // memory: true,
            ..Default::default()
        }
    }
    /// The operand may be placed in any register.
    fn any_reg() -> Self {
        Self {
            register: AcceptsReg::AnyReg,
            ..Default::default()
        }
    }
    /// The operand may only be placed in RAX.
    fn rax_only() -> Self {
        Self {
            register: AcceptsReg::RaxOnly,
            ..Default::default()
        }
    }
}

trait OpSemanticsFor {
    /// Get the operand semantics for the first operand of this instruction.
    fn op1_semantics(&self) -> OpSemantics;
}
impl OpSemanticsFor for Op {
    /// Retrieves the operand semantics for the first operand.
    fn op1_semantics(&self) -> OpSemantics {
        match self {
            Test => OpSemantics::any_reg(),
            Idiv => OpSemantics::any_reg_or_mem(),
            _ => todo!("Determine operand 1 semantics for {}", self),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cmp {
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
}
impl TryFrom<BinOp> for Cmp {
    type Error = ();

    fn try_from(value: BinOp) -> Result<Self, ()> {
        Ok(match value {
            BinOp::LessThan => Cmp::Lt,
            BinOp::GreaterThan => Cmp::Gt,
            BinOp::LessThanEqual => Cmp::Lte,
            BinOp::GreaterThanEqual => Cmp::Gte,
            BinOp::Equal => Cmp::Eq,
            BinOp::NotEqual => Cmp::Neq,
            _ => return Err(()),
        })
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

    macro_rules! compile_instrs {
        ($instrs:expr) => {{
            let proc = Procedure::new("main", Block::new(), Block::new());
            let mut compiler = ProcedureCompiler::new(proc);

            for instr in $instrs.into_iter() {
                compiler.compile_instr(&Instruction::new(instr));
            }
            println!(
                "Listing source:\n===============\n{}===============\n",
                compiler.procedure
            );
            compiler.allocator.debug();
            compiler
        }};
    }

    macro_rules! compile_instr {
        ($instr:expr) => {
            compile_instrs!([$instr])
        };
    }

    macro_rules! first_reg {
        ($compiler:expr) => {
            $compiler
                .allocator
                .iter_allocations()
                .next()
                .copied()
                .unwrap()
        };
    }

    macro_rules! assert_allocates {
        ($compiler:expr, $expected:expr) => {
            let allocations: Vec<_> = $compiler.allocator.iter_allocations().copied().collect();
            assert_eq!(
                &$expected[..],
                &allocations,
                "\n\nExpected allocations:\n\t{:?}\nBut found:\n\t{:?}\n\n",
                $expected,
                &allocations,
            )
        };
    }

    #[test]
    fn compile_tac_compiles_instruction_to_assembly() {
        let compiler = compile_instr!(InstrKind::Bin(sub!("x"), BinOp::Add, cnst!(10), cnst!(99)));
        let target_reg = first_reg!(compiler);

        let mut expected = Block::new();
        expected.push(Mov, vec![Reg(target_reg), Lit(10)]);
        expected.push(Add, vec![Reg(target_reg), Lit(99)]);

        assert_eq!(expected, compiler.procedure.body)
    }

    #[test]
    fn can_compile_multiplication() {
        compile_instr!(InstrKind::Bin(
            sub!("x"),
            BinOp::Multiply,
            cnst!(10),
            cnst!(3),
        ));
    }

    #[test]
    fn integer_division_allocates_to_rax() {
        let compiler = compile_instr!(InstrKind::Bin(
            sub!("x"),
            BinOp::IntDiv,
            cnst!(101),
            cnst!(10)
        ));

        assert_allocates!(compiler, [Rax]);
    }

    #[test]
    fn remainder_allocates_to_rdx() {
        let compiler = compile_instr!(InstrKind::Bin(
            sub!("x"),
            BinOp::Remainder,
            cnst!(101),
            cnst!(10)
        ));

        assert_allocates!(compiler, [Rdx]);
    }

    #[test]
    fn function_call_allocates_for_return_value() {
        let compiler = compile_instrs!([
            InstrKind::Param(cnst!(10)),
            InstrKind::Call(sub!("x"), Builtin::Print, 1)
        ]);

        assert_allocates!(compiler, [Rax]);
    }

    #[test]
    fn function_call_moves_if_return_register_already_allocated() {
        let x = sub!("x");
        let c = sub!("c");
        let compiler = compile_instrs!([
            InstrKind::Assign(c.clone(), cnst!(55)),
            InstrKind::Param(cnst!(10)),
            InstrKind::Call(x.clone(), Builtin::Print, 1)
        ]);

        // `x` should now be in RAX
        assert_eq!(Some(Rax), compiler.allocator.lookup(&x));
        // `c` should now be somewhere else
        assert!(compiler.allocator.lookup(&c).is_some());
    }
}

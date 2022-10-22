use std::fmt::Debug;

use crate::{
    ast::typed::{BinOp, CmpOp, IntOp},
    codegen::register_allocation::Allocator,
    il::*,
    listing::Listing,
    prelude::*,
};

use super::{
    assembly::*, calling_convention::CallingConvention, defer::*,
    lifetime_analysis::LifetimeAnalysis, stack_convention::StackConvention, x86::*,
};

use DeferredOperand::*;
use Op::*;

pub fn compile<S: StackConvention>(
    listing: TacListing,
    procedure: Procedure,
    calling_convention: CallingConvention,
) -> Procedure {
    debug!("Compiling procedure '{}'", procedure.name);
    let instrs = DeferringCompiler::compile_deferred(calling_convention, listing);
    trace!("Deferred assembly:\n{}", instrs);

    debug!("Performing lifetime analysis");
    let allocator = LifetimeAnalysis::create_allocator_for(&instrs);

    debug!("Resolving deferred instructions");
    resolve_deferred_instrs::<S>(instrs, procedure, allocator, calling_convention)
}

fn resolve_deferred_instrs<S: StackConvention>(
    instrs: Listing<DeferredLine>,
    mut procedure: Procedure,
    allocator: Allocator<DeferredReg, Register>,
    calling_convention: CallingConvention,
) -> Procedure {
    let mut pushed_regs = vec![];
    let last_line = instrs.len() - 1;
    let mut stack_size = 0;
    let mut aligned_bytes = 0;

    for (position, line) in instrs.into_iter() {
        match line {
            DeferredLine::Comment(cmt) => {
                procedure.body.push_cmt_only(cmt);
            }
            DeferredLine::Label(lbl) => procedure.body.add_label(format!(".{lbl}")),
            DeferredLine::Instr(instr) => {
                let operands: Vec<_> = instr
                    .operands
                    .into_iter()
                    .map(|o| o.resolve(&allocator, position))
                    .collect();

                procedure.body.push(instr.op, operands);
            }
            DeferredLine::CallerPreserve => {
                for reg_to_push in allocator
                    .live_regs_at(position)
                    .into_iter()
                    .map(|a| a.register())
                    .filter(|&r| {
                        calling_convention.is_caller_saved(r)
                            || r == calling_convention.get_return_reg()
                    })
                {
                    procedure.body.push(Push, [Operand::Reg(reg_to_push)]);
                    stack_size += reg_to_push.byte_size();
                    pushed_regs.push(reg_to_push);
                }
                let alignment = calling_convention.stack_alignment();
                let misaligned_bytes = stack_size % alignment;
                if misaligned_bytes != 0 {
                    aligned_bytes = (alignment - misaligned_bytes) as i128;
                    procedure.body.push(
                        Sub,
                        [Operand::Reg(Register::Rsp), Operand::Lit(aligned_bytes)],
                    );
                }
            }
            DeferredLine::CallerRestore => {
                if aligned_bytes != 0 {
                    procedure.body.push(
                        Add,
                        [Operand::Reg(Register::Rsp), Operand::Lit(aligned_bytes)],
                    );
                }
                for reg_to_pop in pushed_regs.drain(..).rev() {
                    procedure.body.push(Pop, [Operand::Reg(reg_to_pop)]);
                    stack_size -= reg_to_pop.byte_size();
                }
            }
            DeferredLine::Return => {
                if position != last_line {
                    S::add_epilogue(&mut procedure.body);
                }
            }
            DeferredLine::LockRequest(_, _) => (),
            DeferredLine::ImplicitRead(_) => (),
            DeferredLine::AlignStack => (),
        }
    }

    procedure
}

pub struct DeferringCompiler {
    asm_listing: Listing<DeferredLine>,
    calling_convention: CallingConvention,
    current_param: usize,
    current_temp: usize,
}
impl DeferringCompiler {
    /// Construct a new procedure compiler for the given procedure.
    fn compile_deferred(
        calling_convention: CallingConvention,
        listing: TacListing,
    ) -> Listing<DeferredLine> {
        let mut compiler = Self {
            asm_listing: Listing::new(),
            calling_convention,
            current_param: 0,
            current_temp: 0,
        };

        for (_, instr) in listing.into_lines() {
            compiler.compile_instr(instr);
        }

        compiler.asm_listing
    }

    /// Compile a single TAC instruction.
    fn compile_instr(&mut self, instr: TacInstr) {
        self.asm_listing
            .push(DeferredLine::Comment(instr.to_string()));

        match instr {
            TacInstr::Assign(tgt, value) => self.compile_assign(tgt, value),
            TacInstr::Bin(tgt, op, left, right) => self.compile_bin(tgt, op, left, right),
            TacInstr::Param(param) => self.compile_param(param),
            TacInstr::Call(tgt, name, params) => self.compile_call(tgt, name, params),
            TacInstr::IfTrue(value, lbl) => self.compile_jump(value, lbl, true),
            TacInstr::IfFalse(value, lbl) => self.compile_jump(value, lbl, false),
            TacInstr::IfCmp(lhs, op, rhs, label) => self.compile_compare_jump(lhs, op, rhs, label),
            TacInstr::Return(val) => self.compile_return(val),
            TacInstr::Label(lbl) => self.asm_listing.push(DeferredLine::Label(lbl)),
            TacInstr::Phi(_, _) => unreachable!("Phi was not optimised away!"),
            TacInstr::Goto(tgt, names) => self.compile_goto(tgt, names),
        }
    }

    fn compile_return(&mut self, value: Option<Value>) {
        let return_reg = self.calling_convention.get_return_reg();
        if let Some(value) = value {
            self.emit_lock_or_write(value, return_reg);
        }
        self.asm_listing.push(DeferredLine::Return);
    }

    /// Compile a conditional jump. A jump will be made if `value` evaluates to
    /// `jump_when`. This allows this function to compile both `if_true x goto
    /// label` and `if_false y goto label`.
    fn compile_jump(&mut self, value: Value, label: Label, jump_when: bool) {
        let jump_type = match jump_when {
            true => Jnz,
            false => Jz,
        };

        let operand = self.value_to_operand(value, Test.op1_semantics());
        self.emit(Test, [operand.clone(), operand]);
        self.emit(jump_type, [DeferredOperand::Lbl(label)]);
    }

    /// Compile a compare & jump. A jump will be made if the comparison evaluates
    /// to true.
    fn compile_compare_jump(&mut self, lhs: Value, op: CmpOp, rhs: Value, label: Label) {
        let jump_type = match op {
            CmpOp::Equal => Je,
            CmpOp::NotEqual => Jne,
            CmpOp::GreaterThan => Jg,
            CmpOp::LessThan => Jl,
            CmpOp::GreaterThanEqual => Jge,
            CmpOp::LessThanEqual => Jle,
        };

        let lhs = self.value_to_operand(lhs, Cmp.op1_semantics());
        let rhs = self.value_to_operand(rhs, Cmp.op2_semantics());
        self.emit(Cmp, [lhs, rhs]);
        self.emit(jump_type, [Lbl(label)]);
    }

    /// Compile an assignment.
    fn compile_assign(&mut self, target: Name, value: Value) {
        let target = self.name_to_operand(target);
        let value = self.value_to_operand(value, Mov.op2_semantics());

        self.emit(Mov, [target, value]);
    }

    /// Compile a binary operation. Dispatches the operation for the correct compile fuction
    /// if the operation represents a comparison or (TODO) boolean operation.
    fn compile_bin(&mut self, tgt: Name, op: BinOp, left: Value, right: Value) {
        // Comparisons are handled separately, since they should produce a boolean.
        if let BinOp::Compare(cmp) = op {
            self.compile_cmp(tgt, cmp, left, right);
            return;
        }

        // Integer division and remainder are handled specially.
        if let BinOp::IntArith(int_op @ (IntOp::Divide | IntOp::Remainder)) = op {
            // The lower half of the dividend goes in RDX.
            // This is also where the remainder is stored.
            todo!("Deal with integer division");
            // self.reserve(Rdx);
            // // The upper half of the dividend goes in RAX.
            // // This is also where the quotient is stored.
            // let dividend = self.prepare_operand(left, OpSemantics::rax_only());
            // let divisor = self.prepare_operand(right, Idiv.op1_semantics());

            // We also need CQO here.

            // self.emit(Xor, [Reg(Rdx), Reg(Rdx)]);

            // self.emit_cmt(Xor, [Reg(Rdx), Reg(Rdx)], "<div> clear upper half")
            //     .emit_cmt(Idiv, [divisor.operand()], format!("<div> {}", comment));

            // match int_op {
            //     IntOp::Divide => {
            //         self.allocator.bind_to(tgt, Rax);
            //         self.allocator.release(Rdx);
            //     }
            //     IntOp::Remainder => {
            //         self.allocator.bind_to(tgt, Rdx);
            //         self.release_prepared(dividend);
            //     }
            //     _ => unreachable!(),
            // }

            // self.release_prepared(divisor);
            // return;
        }

        let op = translate_binop(op);

        match &left {
            // Instructions like `a = a + 1` can be performed in a single operation.
            Value::Name(name) if name == &tgt => {
                let target = self.name_to_operand(tgt);
                let right = self.value_to_operand(right, op.op2_semantics());
                self.emit(op, [target, right]);
            }
            _ => {
                let target = self.name_to_operand(tgt);
                let left = self.value_to_operand(left, op.op2_semantics());
                let right = self.value_to_operand(right, op.op2_semantics());
                // Binary operations of the form `x = y <> z` (where <> is some operation) cannot be
                // compiled in one go, so we translate them to the following sequence:
                // x <- y
                // x <>= z
                self.emit(Mov, [target.clone(), left]);
                self.emit(op, [target, right]);
            }
        }
    }

    /// Compile a comparison between two values.
    fn compile_cmp(&mut self, tgt: Name, cmp: CmpOp, left: Value, right: Value) {
        let left_op = self.value_to_operand(left, Cmp.op1_semantics());
        let right_op = self.value_to_operand(right, Cmp.op2_semantics());

        // TODO: We may not need this operation if `Setg` clears the register for us.
        let target_op = self.value_to_operand(Value::Name(tgt), OpSemantics::any());
        self.emit(Xor, [target_op.clone(), target_op.clone()]);

        self.emit(Cmp, [left_op, right_op]);
        // The x86 `set` operation copies a comparison flag into a register,
        // allowing us to treat the value as a boolean.
        let set_op = match cmp {
            CmpOp::GreaterThan => Setg,
            CmpOp::GreaterThanEqual => Setge,
            CmpOp::LessThan => Setl,
            CmpOp::LessThanEqual => Setle,
            CmpOp::Equal => Sete,
            CmpOp::NotEqual => Setne,
        };
        self.emit(set_op, [target_op]);
    }

    /// Compile a call to a function with `param_count` parameters.
    fn compile_call(&mut self, tgt: Option<Name>, name: String, mut args: Vec<Value>) {
        let max_args = self.calling_convention.get_params().len();
        if args.len() > max_args {
            todo!("Can't deal with more than {} parameters yet.", max_args);
        }

        self.asm_listing.push(DeferredLine::CallerPreserve);
        self.asm_listing.push(DeferredLine::AlignStack);

        let param_regs: Vec<_> = self
            .calling_convention
            .iter_params(args.len())
            // Note: we apply arguments in reverse order so we can pop the arguments
            // one by one.
            .rev()
            .collect();
        for (_, reg) in param_regs {
            let value = args.pop().expect("Parameter count mismatch");
            self.emit_lock_or_write(value, reg);
        }
        if let Some(tgt) = tgt {
            self.emit_lock(tgt, Register::Rax);
        }

        // TODO: How to notify that RAX will be written to?
        // Might not be necessary if caller preservation includes RAX.
        self.emit(Call, [Id(name)]);

        self.asm_listing.push(DeferredLine::CallerRestore);
    }

    fn compile_param(&mut self, param: Name) {
        let param_reg = self.calling_convention.get_params()[self.current_param];
        self.current_param += 1;
        self.emit_lock(param, param_reg);
    }

    fn compile_goto(&mut self, tgt: Label, names: Vec<Name>) {
        self.emit(Jmp, [Lbl(tgt)]);
        for name in names {
            self.asm_listing
                .push(DeferredLine::ImplicitRead(DeferredReg::from_name(name)));
        }
    }

    /// Prepare the given value as an operand.
    fn value_to_operand(&mut self, value: Value, semantics: OpSemantics) -> DeferredOperand {
        match value {
            Value::Const(c) => {
                if semantics.immediate {
                    DeferredOperand::Lit(c)
                } else {
                    let temp_reg = DeferredReg::AsmTemp(self.next_temp());
                    self.emit(Mov, [Reg(temp_reg.clone()), Lit(c)]);
                    Reg(temp_reg)
                }
            }
            Value::Name(n) => self.name_to_operand(n),
        }
    }

    // Prepare the given name as an operand.
    fn name_to_operand(&self, name: Name) -> DeferredOperand {
        Reg(DeferredReg::from_name(name))
    }

    /// Lock or write the given value, depending on what it is.
    /// If the value is a variable, lock it to the target register.
    /// If it is a constant, write it to the target register.
    fn emit_lock_or_write(&mut self, value: Value, register: Register) {
        match value {
            Value::Const(c) => self.emit(Mov, [ConstReg(register), Lit(c)]),
            Value::Name(n) => self.emit_lock(n, register),
        };
    }

    fn emit<V: Into<Vec<DeferredOperand>>>(&mut self, op: Op, operands: V) {
        self.asm_listing.push(DeferredLine::Instr(DeferredInstr {
            op,
            operands: operands.into(),
        }))
    }

    fn emit_lock(&mut self, name: Name, reg: Register) {
        self.asm_listing
            .push(DeferredLine::LockRequest(DeferredReg::from_name(name), reg))
    }

    fn next_temp(&mut self) -> usize {
        let next = self.current_temp;
        self.current_temp += 1;
        next
    }
}

fn translate_binop(op: BinOp) -> Op {
    match op {
        BinOp::IntArith(IntOp::Add) => Add,
        BinOp::IntArith(IntOp::Multiply) => Imul,
        BinOp::IntArith(IntOp::Divide) => Idiv,
        BinOp::IntArith(IntOp::Subtract) => Sub,
        _ => todo!("Don't know how to translate {:?} to assembly", op),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AcceptsReg {
    AnyReg,
}
impl Default for AcceptsReg {
    fn default() -> Self {
        Self::AnyReg
    }
}

/// Describes what types of locations are accepted for an operand.
#[derive(Debug, Default, Clone, Copy)]
pub struct OpSemantics {
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
    fn any() -> Self {
        Self {
            register: AcceptsReg::AnyReg,
            immediate: true,
        }
    }
}

/// TODO: Op semantics are more complicated than this and should be modelled
/// as a list of rules.
trait OpSemanticsFor {
    /// Get the operand semantics for the first operand of this instruction.
    fn op1_semantics(&self) -> OpSemantics;
    /// Get the operand semantics for the second operand of this instruction.
    fn op2_semantics(&self) -> OpSemantics;
}
impl OpSemanticsFor for Op {
    /// Retrieves the operand semantics for the first operand.
    fn op1_semantics(&self) -> OpSemantics {
        match self {
            Test => OpSemantics::any_reg(),
            Cmp => OpSemantics::any_reg_or_mem(),
            Idiv => OpSemantics::any_reg_or_mem(),
            Mov => OpSemantics::any(),
            _ => {
                warn!(
                    "Don't know operand 1 semantics for {}, guessing 'any'",
                    self
                );
                // Not a big problem if this is wrong, because then nasm will just throw an error
                // if we try to assemble the code.
                OpSemantics::any()
            }
        }
    }
    /// Retrieves the operand semantics for the second operand.
    fn op2_semantics(&self) -> OpSemantics {
        match self {
            Cmp => OpSemantics::any_reg(),
            Mov => OpSemantics::any(),
            _ => {
                warn!(
                    "Don't know operand 2 semantics for {}, guessing 'any'",
                    self
                );
                // Not a big problem if this is wrong, because then nasm will just throw an error
                // if we try to assemble the code.
                OpSemantics::any()
            }
        }
    }
}

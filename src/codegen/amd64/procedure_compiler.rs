use crate::{
    ast::typed::{BinOp, CmpOp, IntOp},
    codegen::register_allocation::{Allocator, Move},
    ext::hash_map::ConstHashMap,
    il::*,
    listing::Listing,
    prelude::*,
};

use super::{
    assembly::*, calling_convention::CallingConvention, defer::*,
    lifetime_analysis::LifetimeAnalysis, op_semantics::*, stack_convention::StackConvention,
    x86::*,
};

use itertools::Itertools;
use DeferredOperand::*;
use Op::*;
use Register::*;

pub fn compile<S: StackConvention>(
    listing: TacProcedure,
    procedure: Procedure,
    calling_convention: CallingConvention,
) -> Procedure {
    debug!("Compiling procedure '{}'", procedure.name);
    let instrs = DeferringCompiler::compile_deferred(calling_convention, listing);
    debug!("Deferred assembly:\n{}", instrs.display_line_nos());

    debug!("Allocating registers");
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

    let mut moves: ConstHashMap<_, _> = allocator
        .get_moves()
        .into_iter()
        .group_by(|mv| mv.position())
        .into_iter()
        .map(|(k, v)| (k, v.collect::<Vec<_>>()))
        .collect();

    debug!("After emitting moves:\n{}", instrs.display_line_nos());

    for (position, line) in instrs.into_iter() {
        trace!("Resolve line {position}: {line}");

        for mv in moves.remove(&position).unwrap_or_default() {
            match mv {
                Move::OneWay(one_way) => {
                    procedure.body.push(
                        Mov,
                        vec![Operand::Reg(one_way.to()), Operand::Reg(one_way.from())],
                    );
                }
                Move::Swap(swap) => {
                    procedure.body.push(
                        Xchg,
                        vec![Operand::Reg(swap.first()), Operand::Reg(swap.second())],
                    );
                }
            }
        }

        match line {
            DeferredLine::Comment(cmt) => {
                procedure.body.push_cmt_only(cmt);
            }
            DeferredLine::BlockEntry(name, params) => procedure.body.add_label(format!(".{name}")),
            DeferredLine::Block(block) => {
                let mut aligned_bytes = 0;

                if block.caller_preserve {
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

                for instr in block.instructions {
                    let op = instr.op;
                    let operands: Vec<_> = instr
                        .operands
                        .into_iter()
                        .enumerate()
                        .map(|(n, o)| o.resolve(&allocator, position, op, n))
                        .collect();

                    procedure.body.push(instr.op, operands);
                }

                if block.caller_preserve {
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
                if block.ret && position != last_line {
                    S::add_epilogue(&mut procedure.body);
                }
            }
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
        procedure: TacProcedure,
    ) -> Listing<DeferredLine> {
        let mut compiler = Self {
            asm_listing: Listing::new(),
            calling_convention,
            current_param: 0,
            current_temp: 0,
        };

        let (entry, children) = procedure.into_blocks();

        for param in entry.parameters() {
            compiler.compile_param(param.clone());
        }

        compiler.compile_basic_block(entry, None);

        for (name, block) in children.into_iter() {
            compiler.compile_basic_block(block, Some(name));
        }

        compiler.asm_listing
    }

    fn compile_basic_block(&mut self, block: BasicBlock, name: Option<Label>) {
        let (lines, params) = block.into_lines_params();
        if let Some(name) = name {
            self.asm_listing
                .push(DeferredLine::BlockEntry(name, params));
        }

        for (_, instr) in lines {
            self.compile_instr(instr);
        }
    }

    /// Compile a single TAC instruction.
    fn compile_instr(&mut self, instr: TacInstr) {
        self.asm_listing
            .push(DeferredLine::Comment(instr.to_string()));

        match instr {
            TacInstr::Assign(tgt, value) => self.compile_assign(tgt, value),
            TacInstr::Bin(tgt, op, left, right) => self.compile_bin(tgt, op, left, right),
            TacInstr::Call(tgt, name, args) => self.compile_call(tgt, name, args),
            TacInstr::Goto(tgt, names) => self.compile_goto(tgt, names),
            TacInstr::If(value, true_lbl, false_lbl, args) => {
                self.compile_jump(value, true_lbl, false_lbl, args)
            }
            TacInstr::IfCmp(lhs, op, rhs, true_lbl, false_lbl, args) => {
                self.compile_compare_jump(lhs, op, rhs, true_lbl, false_lbl, args)
            }
            TacInstr::Return(val) => self.compile_return(val),
        }
    }

    fn compile_return(&mut self, value: Option<Value>) {
        let return_reg = self.calling_convention.get_return_reg();

        self.emit_block(|block| {
            if let Some(value) = value {
                block.emit_lock_or_write(value, return_reg);
            }
            block.ret = true;
        })
    }

    /// Compile a goto instruction.
    fn compile_goto(&mut self, tgt: Label, params: Vec<Name>) {
        self.emit_block(|block| {
            block.emit(Jmp, [Lbl(tgt)]);
            for param in params {
                block.implicit_read(DeferredReg::from_name(param));
            }
        })
    }

    /// Compile a conditional jump. A jump will be made if `value` evaluates to
    /// `jump_when`. This allows this function to compile both `if_true x goto
    /// label` and `if_false y goto label`.
    fn compile_jump(&mut self, value: Value, true_lbl: Label, false_lbl: Label, params: Vec<Name>) {
        let operand = self.value_to_operand(value, Test.op1_semantics());

        self.emit_block(|block| {
            for param in params {
                block.implicit_read(DeferredReg::from_name(param));
            }

            block.emit(Test, [operand.clone(), operand]);
            block.emit(Jnz, [Lbl(true_lbl)]);
            block.emit(Jmp, [Lbl(false_lbl)]);
        });
    }

    /// Compile a compare & jump. A jump will be made if the comparison evaluates
    /// to true.
    fn compile_compare_jump(
        &mut self,
        lhs: Value,
        op: CmpOp,
        rhs: Value,
        true_lbl: Label,
        false_lbl: Label,
        params: Vec<Name>,
    ) {
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

        self.emit_block(|block| {
            for param in params {
                block.implicit_read(DeferredReg::from_name(param));
            }
            block.emit(Cmp, [lhs, rhs]);
            block.emit(jump_type, [Lbl(true_lbl)]);
            block.emit(Jmp, [Lbl(false_lbl)]);
        });
    }

    /// Compile an assignment.
    fn compile_assign(&mut self, target: Name, value: Value) {
        let target = DeferredOperand::from_name(target);
        let value = self.value_to_operand(value, Mov.op2_semantics());

        self.emit(Mov, [target, value]);
    }

    /// Compile a binary operation. Dispatches the operation for the correct compile fuction
    /// if the operation represents a comparison or boolean operation.
    fn compile_bin(&mut self, tgt: Name, op: BinOp, left: Value, right: Value) {
        // Comparisons are handled separately, since they should produce a boolean.
        if let BinOp::Compare(cmp) = op {
            self.compile_cmp(tgt, cmp, left, right);
            return;
        }

        // Integer division and remainder are handled specially
        if let BinOp::IntArith(int_op @ (IntOp::Divide | IntOp::Remainder)) = op {
            let divisor_temp = self.generate_temp();

            self.emit_block(|block| {
                // The idiv instruction divides a value spread across rdx and rax
                // by the value in a different register or memory location.
                block.emit_lock_or_write(left, Rax);

                let overwritten_register = match int_op {
                    IntOp::Divide => {
                        // The quotient goes in Rax
                        block.lock_register(tgt, Rax, Direction::Write);
                        Rdx
                    }
                    IntOp::Remainder => {
                        // The remainder goes in Rdx
                        block.lock_register(tgt, Rdx, Direction::Write);
                        Rax
                    }
                    _ => unreachable!(),
                };

                block.implicit_write(overwritten_register);

                // Since we only work with 64-bit values, we know rdx must be zeroed.
                block.emit(Xor, [ConstReg(Rdx), ConstReg(Rdx)]);
                block.emit(Cqo, []);

                let rhs_op = block.value_to_operand_hint(right, Idiv.op1_semantics(), divisor_temp);
                block.emit(Idiv, [rhs_op]);
            });
            return;
        }

        let op = translate_binop(op);

        match &left {
            // Instructions like `a = a + 1` can be performed in a single operation.
            Value::Name(name) if name == &tgt => {
                let target = DeferredOperand::from_name(tgt);
                let right = self.value_to_operand(right, op.op2_semantics());
                self.emit(op, [target, right]);
            }
            _ => {
                let target = DeferredOperand::from_name(tgt);
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
        let bool_tgt = self.value_to_operand(Value::Name(tgt), set_op.op1_semantics());

        // This `xor` is necessary because `setxx` only writes to the lower byte of the register,
        // so we need to zero the rest.
        self.emit(Xor, [bool_tgt.clone(), bool_tgt.clone()]);

        self.emit(Cmp, [left_op, right_op]);

        self.emit(set_op, [bool_tgt]);
    }

    /// Compile a call to a function with `param_count` parameters.
    fn compile_call(&mut self, tgt: Option<Name>, name: String, mut args: Vec<Value>) {
        let max_args = self.calling_convention.get_params().len();
        if args.len() > max_args {
            todo!("Can't deal with more than {} parameters yet.", max_args);
        }

        let param_regs: Vec<_> = self
            .calling_convention
            .iter_params(args.len())
            // Note: we apply arguments in reverse order so we can pop the arguments
            // one by one.
            .rev()
            .collect();

        self.emit_block(|block| {
            block.caller_preserve = true;
            block.align_stack = true;

            for (_, reg) in param_regs {
                let value = args.pop().expect("Parameter count mismatch");
                block.emit_lock_or_write(value, reg);
            }
            if let Some(tgt) = tgt {
                block.lock_register(tgt, Register::Rax, Direction::Write);
            }

            block.emit(Call, [Id(name)]);
        });
    }

    /// Compile a parameter. This does not emit any actual assembly, it only
    /// informs the register allocator in which register the parameter is
    /// located.
    fn compile_param(&mut self, param: Name) {
        let param_reg = self.calling_convention.get_params()[self.current_param];
        self.current_param += 1;

        self.emit_block(|block| {
            block.lock_register(param, param_reg, Direction::Read);
        })
    }

    /// Prepare the given value as an operand. This may result in the
    /// allocation of a register to hold a the value if it is a constant and
    /// the semantics of this operand do not allow immediate values.
    fn value_to_operand(&mut self, value: Value, semantics: OpSemantics) -> DeferredOperand {
        // It is not very likely that `temp_reg` will actually end up holding
        // the value, so in most cases, we will just generate unused
        // temporaries. This is not ideal, but it's the easiest way to allow
        // `value_to_operand_hint` to be used both within and outside a block.
        let temp_reg = self.generate_temp();
        self.emit_block(|block| block.value_to_operand_hint(value, semantics, temp_reg))
    }

    /// Emit a block of deferred assembly. Within a block, variables are
    /// guaranteed not to be moved by the register allocator.
    fn emit_block<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut DeferredBlock) -> R,
    {
        let mut block = DeferredBlock::new();
        let res = f(&mut block);
        self.asm_listing.push(DeferredLine::Block(block));
        res
    }

    /// Create a 'singleton block' and add a single instruction to it.
    pub fn emit<V: Into<Vec<DeferredOperand>>>(&mut self, op: Op, operands: V) {
        self.emit_block(|block| block.emit(op, operands))
    }

    /// Generate a new temporary variable.
    fn generate_temp(&mut self) -> DeferredReg {
        let next = self.current_temp;
        self.current_temp += 1;
        DeferredReg::AsmTemp(next)
    }
}

trait BlockExt {
    fn emit_lock_or_write(&mut self, value: Value, register: Register);
    fn value_to_operand_hint(
        &mut self,
        value: Value,
        semantics: OpSemantics,
        hint: DeferredReg,
    ) -> DeferredOperand;
}
impl BlockExt for DeferredBlock {
    /// Lock or write the given value, depending on what it is.
    /// If the value is a variable, lock it to the target register.
    /// If it is a constant, write it to the target register.
    fn emit_lock_or_write(&mut self, value: Value, register: Register) {
        match value {
            Value::Const(c) => self.emit(Mov, [ConstReg(register), Lit(c)]),
            Value::Name(n) => self.lock_register(n, register, Direction::Read),
        };
    }

    /// Prepare the given value as an operand. If the value is a constant and
    /// the operand semantics do not allow immediate values, the value is
    /// first written to the hinted register.
    fn value_to_operand_hint(
        &mut self,
        value: Value,
        semantics: OpSemantics,
        hint: DeferredReg,
    ) -> DeferredOperand {
        match value {
            Value::Const(c) => {
                if semantics.immediate() {
                    DeferredOperand::Lit(c)
                } else {
                    // If the operand semantics forbid immediate values, we need to first
                    // emit a write to a temporary register.
                    self.emit(Mov, [Reg(hint.clone()), Lit(c)]);
                    Reg(hint)
                }
            }
            Value::Name(n) => DeferredOperand::from_name(n),
        }
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

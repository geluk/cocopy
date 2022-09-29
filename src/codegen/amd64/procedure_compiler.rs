use std::marker::PhantomData;

use crate::{
    ast::typed::{BinOp, CmpOp, IntOp},
    codegen::register_allocation::{Allocator, Destination},
    il::*,
    listing::{Listing, Position},
    prelude::*,
};

use super::{
    assembly::*, calling_convention::CallingConvention, register_allocator::RegisterAllocator,
    stack_convention::StackConvention, x86::*,
};

#[derive(Debug)]
pub enum DeferredLine {
    Comment(String),
    Label(Label),
    Instr(DeferredInstr),
    LockRequest(DeferredReg, Register),
    CallerPreserve,
    CallerRestore,
    AlignStack,
    Return,
}

#[derive(Debug)]
pub struct DeferredInstr {
    pub op: Op,
    pub target: Option<Target>,
    pub operands: Vec<DeferredOperand>,
}

#[derive(Debug, Clone)]
pub enum DeferredOperand {
    /// Some not yet known register.
    /// Also includes the operator semantics so that the register allocator
    /// can make sure to assign the name to a valid register, or to emit a
    /// just-in-time swap to bring it into the right register.
    Reg(DeferredReg, OpSemantics),
    /// An immediate value
    Lit(TargetSize),
    /// A label
    Lbl(Label),
    /// An identifier
    Id(String),
}
impl DeferredOperand {
    /// Given an allocator and a source code position, resolve the deferred
    /// operand back to an assembly operand.
    pub fn resolve(
        self,
        allocator: &Allocator<DeferredReg, Register>,
        position: Position,
    ) -> Operand {
        match self {
            Reg(deferred, _) => deferred.resolve(allocator, position),
            Lit(lit) => Operand::Lit(lit as i128),
            Lbl(lbl) => Operand::Lbl(lbl.to_string()),
            Id(id) => Operand::Id(id),
        }
    }
}

/// An as of yet unknown register. After the register allocation pass, this
/// can be definitively resolved to a register by querying the register
/// allocator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeferredReg {
    Sub(Variable),
    Temp(usize),
    AsmTemp(usize),
}
impl DeferredReg {
    /// Given an allocator and a source code position, resolve the deferred
    /// register back to an assembly operand.
    pub fn resolve(
        self,
        allocator: &Allocator<DeferredReg, Register>,
        position: Position,
    ) -> Operand {
        match allocator.lookup(&self, position) {
            Destination::Reg(alloc) => Operand::Reg(alloc.register()),
            Destination::Stack(_) => todo!("Reference the stack"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Target {
    Deferred(DeferredReg),
    Reg(Register),
}

use DeferredOperand::*;
use Op::*;

pub struct ProcedureCompiler<C: StackConvention> {
    asm_listing: Listing<DeferredLine>,
    calling_convention: CallingConvention,
    arg_stack: Vec<Value>,
    current_param: usize,
    current_temp: usize,
    _phantom: PhantomData<*const C>,
}
impl<C: StackConvention> ProcedureCompiler<C> {
    /// Compile the given source code into the given procedure.
    pub fn compile(
        listing: TacListing,
        mut procedure: Procedure,
        calling_convention: CallingConvention,
    ) -> Procedure {
        debug!("Compiling procedure '{}'", procedure.name);

        debug!("Compiling");
        let instrs = Self::compile_internal(calling_convention, listing);
        for instr in instrs.iter_lines() {
            debug!("{instr:?}")
        }

        debug!("Preprocessing");
        let allocator = RegisterAllocator::preprocess(&instrs);

        for (position, line) in instrs.into_iter() {
            match line {
                DeferredLine::Comment(cmt) => {
                    procedure.body.push_cmt_only(cmt);
                }
                DeferredLine::Label(lbl) => procedure.body.add_label(lbl.to_string()),
                DeferredLine::Instr(instr) => {
                    let mut operands: Vec<_> = instr
                        .operands
                        .into_iter()
                        .map(|o| o.resolve(&allocator, position))
                        .collect();

                    if let Some(target) = instr.target {
                        let tgt_op = match target {
                            Target::Deferred(df) => df.resolve(&allocator, position),
                            Target::Reg(r) => Operand::Reg(r),
                        };
                        operands.insert(0, tgt_op);
                    }

                    procedure.body.push(instr.op, operands);
                }
                DeferredLine::LockRequest(_, _) => (),
                DeferredLine::CallerPreserve => (),
                DeferredLine::CallerRestore => (),
                DeferredLine::AlignStack => (),
                DeferredLine::Return => todo!(),
            }
        }

        procedure
    }

    /// Construct a new procedure compiler for the given procedure.
    fn compile_internal(
        calling_convention: CallingConvention,
        listing: TacListing,
    ) -> Listing<DeferredLine> {
        let mut compiler = Self {
            asm_listing: Listing::empty(),
            calling_convention,
            arg_stack: vec![],
            current_param: 0,
            current_temp: 0,
            _phantom: Default::default(),
        };

        for (_, instr) in listing.into_lines() {
            compiler.compile_instr(instr);
        }

        compiler.asm_listing
    }

    /// Compile a single TAC instruction.
    fn compile_instr(&mut self, instr: Instruction) {
        let instr_string = instr.to_string();
        self.asm_listing.push(DeferredLine::Comment(instr_string));

        if let Some(lbl) = instr.label {
            self.asm_listing.push(DeferredLine::Label(lbl))
        }
        match instr.kind {
            InstrKind::Assign(tgt, value) => self.compile_assign(tgt, value),
            InstrKind::Bin(tgt, op, left, right) => self.compile_bin(tgt, op, left, right),
            InstrKind::Arg(arg) => self.compile_arg(arg),
            InstrKind::Param(param) => self.compile_param(param),
            InstrKind::Call(tgt, name, params) => self.compile_call(tgt, name, params),
            InstrKind::Nop => (),
            InstrKind::IfTrue(value, lbl) => self.compile_jump(value, lbl, true),
            InstrKind::IfFalse(value, lbl) => self.compile_jump(value, lbl, false),
            InstrKind::IfCmp(lhs, op, rhs, label) => self.compile_compare_jump(lhs, op, rhs, label),
            InstrKind::Return(val) => self.compile_return(val),
            // If this happens, the optimiser has failed and we won't be able to generate code
            // for branching statements, so we should bail here.
            InstrKind::Phi(_, _) => unreachable!("Phi was not optimised away!"),
            InstrKind::Goto(tgt) => {
                self.emit(Jmp, [Lbl(tgt)]);
            }
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

        let operand = self.prepare_operand(value, Test.op1_semantics());
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

        let lhs = self.prepare_operand(lhs, Cmp.op1_semantics());
        let rhs = self.prepare_operand(rhs, Cmp.op2_semantics());
        self.emit(Cmp, [lhs, rhs]);
        self.emit(jump_type, [Lbl(label)]);
    }

    /// Compile an assignment.
    fn compile_assign(&mut self, target: Name, value: Value) {
        let target = self.prepare_target(target);
        let value = self.prepare_operand(value, Mov.op2_semantics());

        self.emit_write(Mov, target, [value]);
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

        let target = self.prepare_target(tgt);
        let op = translate_binop(op);
        let left = self.prepare_operand(left, OpSemantics::any());
        let right = self.prepare_operand(right, OpSemantics::any());

        // Binary operations of the form `x = y <> z` (where <> is some operation) cannot be
        // compiled in one go, so we translate them to the following sequence:
        // x <- y
        // x <>= z
        self.emit_write(Mov, target.clone(), [left]);
        self.emit_write(op, target, [right]);
    }

    /// Compile a comparison between two values.
    fn compile_cmp(&mut self, tgt: Name, cmp: CmpOp, left: Value, right: Value) {
        let left_op = self.prepare_operand(left, OpSemantics::any_reg());
        let right_op = self.prepare_operand(right, OpSemantics::any());

        let target = self.prepare_target(tgt.clone());
        let target_op = self.prepare_operand(Value::Name(tgt), OpSemantics::any());
        self.emit_write(Xor, target.clone(), [target_op]);

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
        self.emit_write(set_op, target, []);
    }

    /// Compile a call to a function with `param_count` parameters.
    fn compile_call(&mut self, tgt: Option<Name>, name: String, param_count: usize) {
        let max_params = self.calling_convention.get_params().len();
        if param_count > max_params {
            todo!("Can't deal with more than {} parameters yet.", max_params);
        }

        let param_regs: Vec<_> = self
            .calling_convention
            .iter_params(param_count)
            // Might need some more testing to check if this always behaves nicely,
            // especially with nested function calls.
            .rev()
            .collect();
        for (idx, reg) in param_regs {
            let value = self.arg_stack.pop().expect("Parameter count mismatch!");
            self.emit_lock_or_write(value, reg);
        }

        self.asm_listing.push(DeferredLine::CallerPreserve);
        self.asm_listing.push(DeferredLine::AlignStack);

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

    /// Compile a function argument.
    fn compile_arg(&mut self, arg: Value) {
        self.arg_stack.push(arg);
    }

    /// Create a deferred target for the given name.
    fn prepare_target(&self, target: Name) -> Target {
        Target::Deferred(defer(target))
    }

    fn prepare_operand(&mut self, value: Value, semantics: OpSemantics) -> DeferredOperand {
        match value {
            Value::Const(c) => {
                if semantics.immediate {
                    DeferredOperand::Lit(c)
                } else {
                    let deferred_reg = DeferredReg::AsmTemp(self.next_temp());
                    self.emit_write(Mov, Target::Deferred(deferred_reg.clone()), [Lit(c)]);
                    DeferredOperand::Reg(deferred_reg, semantics)
                }
            }
            Value::Name(Name::Sub(var)) => DeferredOperand::Reg(DeferredReg::Sub(var), semantics),
            Value::Name(Name::Temp(temp)) => {
                DeferredOperand::Reg(DeferredReg::Temp(temp), semantics)
            }
        }
    }

    fn emit_lock_or_write(&mut self, value: Value, register: Register) {
        match value {
            Value::Const(c) => {
                self.emit_write(Mov, Target::Reg(register), [DeferredOperand::Lit(c)])
            }
            Value::Name(n) => self.emit_lock(n, register),
        };
    }

    fn emit_write<V: Into<Vec<DeferredOperand>>>(&mut self, op: Op, target: Target, operands: V) {
        self.asm_listing.push(DeferredLine::Instr(DeferredInstr {
            op,
            target: Some(target),
            operands: operands.into(),
        }))
    }

    fn emit<V: Into<Vec<DeferredOperand>>>(&mut self, op: Op, operands: V) {
        self.asm_listing.push(DeferredLine::Instr(DeferredInstr {
            op,
            target: None,
            operands: operands.into(),
        }))
    }

    fn emit_lock(&mut self, name: Name, reg: Register) {
        self.asm_listing
            .push(DeferredLine::LockRequest(defer(name), reg))
    }

    fn next_temp(&mut self) -> usize {
        let next = self.current_temp;
        self.current_temp += 1;
        next
    }
}

fn defer(target: Name) -> DeferredReg {
    match target {
        Name::Sub(var) => DeferredReg::Sub(var),
        Name::Temp(temp) => DeferredReg::Temp(temp),
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
            _ => todo!("Determine operand 1 semantics for {}", self),
        }
    }
    /// Retrieves the operand semantics for the second operand.
    fn op2_semantics(&self) -> OpSemantics {
        match self {
            Cmp => OpSemantics::any_reg(),
            Mov => OpSemantics::any(),
            _ => todo!("Determine operand 2 semantics for {}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::super::stack_convention::Windows64;
    // use super::*;

    // macro_rules! sub {
    //     ($name:expr) => {
    //         Name::Sub(Variable::new($name.to_string(), 1))
    //     };
    // }

    // macro_rules! cnst {
    //     ($value:expr) => {
    //         Value::Const($value)
    //     };
    // }

    // macro_rules! compile_instrs {
    //     ($instrs:expr) => {{
    //         let proc = Procedure::new("main".to_string(), Block::new(), Block::new());
    //         // We may need to construct a listing from the given instructions here.
    //         let mut compiler = ProcedureCompiler::<Windows64>::new(
    //             proc,
    //             TacListing::new(),
    //             CallingConvention::Microsoft64,
    //             Allocator::new(Register::iter().copied().collect()),
    //         );

    //         for instr in $instrs.into_iter() {
    //             compiler.compile_instr(Instruction::new(instr));
    //         }
    //         println!(
    //             "Listing source:\n===============\n{}===============\n",
    //             compiler.procedure
    //         );
    //         compiler.allocator.debug();
    //         compiler
    //     }};
    // }

    // macro_rules! compile_instr {
    //     ($instr:expr) => {
    //         compile_instrs!([$instr])
    //     };
    // }

    // macro_rules! first_reg {
    //     ($compiler:expr) => {
    //         $compiler
    //             .allocator
    //             .iter_allocations()
    //             .next()
    //             .copied()
    //             .unwrap()
    //     };
    // }

    // macro_rules! assert_allocates {
    //     ($compiler:expr, $expected:expr) => {
    //         let allocations: Vec<_> = $compiler.allocator.iter_allocations().copied().collect();
    //         assert_eq!(
    //             &$expected[..],
    //             &allocations,
    //             "\n\nExpected allocations:\n\t{:?}\nBut found:\n\t{:?}\n\n",
    //             $expected,
    //             &allocations,
    //         )
    //     };
    // }

    // macro_rules! assert_no_allocations {
    //     ($compiler:expr) => {
    //         let allocations: Vec<_> = $compiler.allocator.iter_allocations().copied().collect();
    //         assert!(
    //             allocations.is_empty(),
    //             "Expected no allocations, but found: {:?}",
    //             allocations,
    //         )
    //     };
    // }

    // #[test]
    // fn compile_tac_compiles_instruction_to_assembly() {
    //     let compiler = compile_instr!(InstrKind::Bin(
    //         sub!("x"),
    //         BinOp::IntArith(IntOp::Add),
    //         cnst!(10),
    //         cnst!(99)
    //     ));
    //     let target_reg = first_reg!(compiler);

    //     let mut expected = Block::new();
    //     expected.push(Mov, vec![Reg(target_reg), Lit(10)]);
    //     expected.push(Add, vec![Reg(target_reg), Lit(99)]);

    //     assert_eq!(expected, compiler.procedure.body)
    // }

    // #[test]
    // fn can_compile_multiplication() {
    //     compile_instr!(InstrKind::Bin(
    //         sub!("x"),
    //         BinOp::IntArith(IntOp::Multiply),
    //         cnst!(10),
    //         cnst!(3),
    //     ));
    // }

    // #[test]
    // fn integer_division_allocates_to_rax() {
    //     let compiler = compile_instr!(InstrKind::Bin(
    //         sub!("x"),
    //         BinOp::IntArith(IntOp::Divide),
    //         cnst!(101),
    //         cnst!(10)
    //     ));

    //     assert_allocates!(compiler, [Rax]);
    // }

    // #[test]
    // fn remainder_allocates_to_rdx() {
    //     let compiler = compile_instr!(InstrKind::Bin(
    //         sub!("x"),
    //         BinOp::IntArith(IntOp::Remainder),
    //         cnst!(101),
    //         cnst!(10)
    //     ));

    //     assert_allocates!(compiler, [Rdx]);
    // }

    // #[test]
    // fn function_call_with_return_allocates_for_return_value() {
    //     let compiler = compile_instrs!([
    //         InstrKind::Arg(cnst!(10)),
    //         InstrKind::Call(Some(sub!("x")), "get_x".to_string(), 1)
    //     ]);

    //     assert_allocates!(compiler, [Rax]);
    // }

    // #[test]
    // #[ignore = "Enable this test for the new allocator"]
    // fn function_call_without_return_does_not_allocate() {
    //     let compiler = compile_instrs!([
    //         InstrKind::Arg(cnst!(10)),
    //         InstrKind::Call(None, "print".to_string(), 1)
    //     ]);

    //     assert_no_allocations!(compiler);
    // }

    // #[test]
    // fn function_call_moves_if_return_register_already_allocated() {
    //     let x = sub!("x");
    //     let c = sub!("c");
    //     let compiler = compile_instrs!([
    //         InstrKind::Assign(c.clone(), cnst!(55)),
    //         InstrKind::Arg(cnst!(10)),
    //         InstrKind::Call(Some(x.clone()), "print".to_string(), 1),
    //         // Dummy assignment to ensure `c` is still used after the call.
    //         InstrKind::Assign(c.clone(), Value::Name(c.clone()))
    //     ]);

    //     // `x` should now be in RAX.
    //     assert_eq!(Some(Rax), compiler.allocator.lookup(&x));
    //     // `c` should now be somewhere else.
    //     assert!(compiler.allocator.lookup(&c).is_some());
    // }

    // #[test]
    // fn function_call_does_not_move_if_return_reg_may_be_overwritten() {
    //     let x = sub!("x");
    //     let c = sub!("c");
    //     let compiler = compile_instrs!([
    //         InstrKind::Assign(c.clone(), cnst!(55)),
    //         InstrKind::Arg(cnst!(10)),
    //         InstrKind::Call(Some(x.clone()), "print".to_string(), 1),
    //     ]);

    //     // `x` should now be in RAX.
    //     assert_eq!(Some(Rax), compiler.allocator.lookup(&x));
    //     // `c` should now be gone.
    //     assert!(compiler.allocator.lookup(&c).is_none());
    // }

    // #[test]
    // fn divide_value_already_in_rax_does_not_overwrite() {
    //     let x = sub!("x");
    //     let y = sub!("y");
    //     let compiler = compile_instrs!([
    //         InstrKind::Assign(x.clone(), cnst!(55)),
    //         InstrKind::Bin(
    //             y.clone(),
    //             BinOp::IntArith(IntOp::Divide),
    //             Value::Name(x.clone()),
    //             Value::Const(10)
    //         ),
    //     ]);

    //     assert_ne!(compiler.allocator.lookup(&x), compiler.allocator.lookup(&y));
    // }
}

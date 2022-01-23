//! Native code generation for 64-bit Windows.

use crate::{
    ast::untyped::BinOp,
    codegen::amd64::x86::{Op, Register},
    il::*,
};

use super::{assembly::*, register_allocator::RegisterAllocator};

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(prog: Vec<Instruction>) -> Assembly {
    use Decl::*;
    let mut asm = make_assembly();

    asm.push_decl(Bits(64))
        .push_decl(Default("rel"))
        .push_decl(Global("main"))
        .push_decl(Extern("_CRT_INIT"))
        .push_decl(Extern("ExitProcess"))
        .push_decl(Extern("printf"));

    asm.text
        .main
        .body
        .push(Op::Call, vec![Id("_CRT_INIT")])
        .blank();

    compile_tac(prog, &mut asm);

    asm.text
        .main
        .body
        .blank()
        .ret_zero()
        .push(Call, vec![Id("ExitProcess")]);

    //asm.text.procedures.push(square());

    asm.data
        .db("msg_ch", "'The char is %c', 13, 10, 0")
        .db("msg_i", "'The integer is %i', 13, 10, 0");

    asm
}

fn compile_tac(prog: Vec<Instruction>, asm: &mut Assembly) {
    let mut allocator = RegisterAllocator::new();
    let mut final_tgt = None;
    for instr in prog {
        let comment = instr.to_string();
        match instr {
            Instruction::Assign(tgt, value) => {
                let target = Reg(allocator.lookup(tgt));
                final_tgt.replace(target.clone());
                let value = get_operand(value, &mut allocator);

                asm.text
                    .main
                    .body
                    .push_cmt(Mov, vec![target, value], comment);
            }
            Instruction::Bin(tgt, op, left, right) => {
                let target = Reg(allocator.lookup(tgt));
                final_tgt.replace(target.clone());
                let op = translate_binop(op);
                let left = get_operand(left, &mut allocator);
                let right = get_operand(right, &mut allocator);

                // Store
                asm.text.main.body.push_cmt(
                    Mov,
                    vec![target, left],
                    format!("<store> {}", comment),
                );
                // Apply
                asm.text.main.body.push_cmt(
                    op,
                    vec![target, right],
                    format!("<apply> {}", comment),
                );
            }
        }
    }
    if let Some(tgt) = final_tgt {
        asm.text
            .main
            .body
            .blank()
            .push_cmt(Lea, vec![Reg(RCX), Id("[msg_i]")], "arg0: printf template")
            .push_cmt(Mov, vec![Reg(RDX), tgt], "arg1: calculation outcome")
            .push_cmt(Call, vec![Id("printf")], "call printf");
    }
}

fn get_operand(value: Value, allocator: &mut RegisterAllocator) -> Operand {
    match value {
        Value::Const(lit) => Lit(lit as i128),
        Value::Name(name) => Reg(allocator.lookup(name)),
    }
}

fn translate_binop(op: BinOp) -> Op {
    match op {
        BinOp::Add => Add,
        BinOp::Multiply => Imul,
        BinOp::Subtract => Sub,
        _ => todo!("Unknown operation"),
    }
}

fn square() -> Procedure {
    let mut square = procedure("square");
    square
        .body
        .push(Mov, vec![Reg(RAX), Reg(RCX)])
        .push(Imul, vec![Reg(RAX)]);
    square
}

fn make_assembly() -> Assembly {
    Assembly::new(Procedure::new("main", prologue(), Block::new()))
}

fn procedure(name: Str) -> Procedure {
    Procedure::new(name, prologue(), epilogue())
}

fn prologue() -> Block {
    let mut prologue = Block::new();

    prologue
        .push_cmt(Push, vec![Reg(RBP)], "Store base pointer")
        .push_cmt(Mov, vec![Reg(RBP), Reg(RSP)], "Move base pointer down")
        .push_cmt(Sub, vec![Reg(RSP), Lit(32)], "Create shadow space")
        .blank();

    prologue
}

fn epilogue() -> Block {
    let mut epilogue = Block::new();
    epilogue
        .blank()
        .push_cmt(Mov, vec![Reg(RSP), Reg(RBP)], "Move stack pointer back up")
        .push_cmt(Pop, vec![Reg(RBP)], "Restore previous base pointer")
        .push_cmt(Ret, vec![], "Return to caller");

    epilogue
}

trait BodyExt {
    fn ret_zero(&mut self) -> &mut Self;
}
impl BodyExt for Block {
    fn ret_zero(&mut self) -> &mut Self {
        self.push_cmt(Xor, vec![Reg(RAX), Reg(RAX)], "Return zero");
        self
    }
}

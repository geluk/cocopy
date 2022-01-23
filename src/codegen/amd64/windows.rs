//! Native code generation for 64-bit Windows.

use crate::{
    codegen::amd64::x86::{Op, Register},
    il::*,
};

use super::assembly::*;

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(prog: &Vec<Instruction>) -> Assembly {
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

    asm.text
        .main
        .body
        .push(Lea, vec![Reg(RCX), Id("[msg_ch]")])
        .push(Mov, vec![Reg(RDX), Id("[msg_ch]")])
        .push(Call, vec![Id("printf")])
        .blank();

    asm.text
        .main
        .body
        .push(Mov, vec![Reg(RCX), Lit(12)])
        .push(Call, vec![Id("square")])
        .push(Lea, vec![Reg(RCX), Id("[msg_i]")])
        .push(Mov, vec![Reg(RDX), Reg(RAX)])
        .push(Call, vec![Id("printf")])
        .blank();

    asm.text
        .main
        .body
        .ret_zero()
        .push(Call, vec![Id("ExitProcess")]);

    let mut square = procedure("square");
    square
        .body
        .push(Mov, vec![Reg(RAX), Reg(RCX)])
        .push(Mul, vec![Reg(RAX)]);
    asm.text.procedures.push(square);

    asm.data
        .db("msg_ch", "'The char is %c', 13, 10, 0")
        .db("msg_i", "'The integer is %i', 13, 10, 0");

    asm
}

fn make_assembly() -> Assembly {
    Assembly::new(procedure("main"))
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

//! Native code generation for 64-bit Linux.

use crate::il::Instruction;

use super::assembly::*;
use super::x86::*;

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(_prog: &Vec<Instruction>) -> Assembly {
    default()
}

fn default() -> Assembly {
    use Decl::*;
    let mut asm = make_assembly();

    asm.push_decl(Extern("printf")).push_decl(Global("main"));

    asm.text
        .main
        .body
        .push(Mov, vec![Reg(RDI), Id("fmt_int")])
        .push(Mov, vec![Reg(RSI), Lit(101)])
        .push(Call, vec![Id("printf")])
        .push(Mov, vec![Reg(RAX), Lit(0)]);

    asm.data.db("fmt_int", "'%i', 0");

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
    prologue.push_cmt(Push, vec![Reg(RBP)], "Store base pointer");

    prologue
}

fn epilogue() -> Block {
    let mut epilogue = Block::new();
    epilogue
        .push_cmt(Pop, vec![Reg(RBP)], "Restore previous base pointer")
        .push_cmt(Ret, vec![], "Return to caller");

    epilogue
}

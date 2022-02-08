//! Native code generation for 64-bit Linux.

use crate::codegen::amd64::calling_convention::CallingConvention;
use crate::codegen::amd64::procedure_compiler::ProcedureCompiler;
use crate::il::TacProgram;

use super::assembly::*;
use super::x86::*;

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(prog: TacProgram) -> Assembly {
    use Decl::*;
    let mut asm = make_assembly();

    asm.push_decl(Bits(64))
        .push_decl(Extern("printf"))
        .push_decl(Extern("scanf"))
        .push_decl(Global("main"));

    asm.text.main =
        ProcedureCompiler::compile(prog.top_level, asm.text.main, CallingConvention::SystemV64);

    for (name, listing) in prog.functions {
        let proc =
            ProcedureCompiler::compile(listing, procedure(name), CallingConvention::SystemV64);
        asm.text.procedures.push(proc);
    }

    asm.text
        .main
        .body
        .blank()
        .push_cmt(Mov, [Reg(Rax), Lit(0)], "return 0");

    asm.text.procedures.push(print());
    asm.text.procedures.push(readint());

    asm.data
        .db("msg_i", "'The integer is %i', 10, 0")
        .db("scanf_i", "'%i', 0");

    asm
}

fn print() -> Procedure {
    let mut print = procedure("print");
    print
        .body
        .push(Mov, [Reg(Rsi), Reg(Rdi)])
        .push(Lea, [Reg(Rdi), Id("[msg_i]".to_string())])
        .push(Call, [Id("printf".to_string())]);
    print
}

fn readint() -> Procedure {
    let mut readint = procedure("readint");
    readint
        .body
        .push_cmt(
            Sub,
            [Reg(Rsp), Lit(16)],
            "allocate stack space for return value",
        )
        .push(Lea, [Reg(Rdi), Id("[scanf_i]".to_string())])
        .push_cmt(
            Mov,
            [Reg(Rsi), Reg(Rsp)],
            "let scanf write its result to the stack",
        )
        .push(Call, [Id("scanf".to_string())])
        .push_cmt(
            Mov,
            [Reg(Rax), Id("[rsp]".to_string())],
            "fetch result from the stack",
        )
        .push_cmt(Add, [Reg(Rsp), Lit(16)], "put stack back");

    readint
}

fn make_assembly() -> Assembly {
    Assembly::new(procedure("main"))
}

fn procedure<S: Into<String>>(name: S) -> Procedure {
    Procedure::new(name.into(), prologue(), epilogue())
}

fn prologue() -> Block {
    let mut prologue = Block::new();
    prologue
        .push_cmt(Push, [Reg(Rbp)], "store base pointer")
        .blank();

    prologue
}

fn epilogue() -> Block {
    let mut epilogue = Block::new();
    epilogue
        .blank()
        .push_cmt(Pop, [Reg(Rbp)], "restore previous base pointer")
        .push_cmt(Ret, [], "return to caller");

    epilogue
}

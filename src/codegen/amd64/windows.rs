//! Native code generation for 64-bit Windows.

use crate::il::*;

use super::{
    assembly::*,
    calling_convention::CallingConvention,
    procedure_compiler::ProcedureCompiler,
    stack_convention::{self, StackConvention, Windows64},
    x86::*,
};

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(prog: TacProgram) -> Assembly {
    use Decl::*;
    let mut asm = make_assembly();

    asm.push_decl(Bits(64))
        .push_decl(Default("rel"))
        .push_decl(Global("main"))
        .push_decl(Extern("_CRT_INIT"))
        .push_decl(Extern("ExitProcess"))
        .push_decl(Extern("printf"))
        .push_decl(Extern("scanf"));

    asm.text
        .main
        .body
        .push(Op::Call, [Id("_CRT_INIT".to_string())])
        .blank();

    asm.text.main = ProcedureCompiler::compile(
        prog.top_level,
        asm.text.main,
        CallingConvention::Microsoft64,
    );

    for (name, listing) in prog.functions {
        let proc =
            ProcedureCompiler::compile(listing, procedure(name), CallingConvention::Microsoft64);
        asm.text.procedures.push(proc);
    }

    asm.text
        .main
        .body
        .blank()
        .ret_zero()
        .push(Call, [Id("ExitProcess".to_string())]);

    asm.text.procedures.push(print());
    asm.text.procedures.push(readint());

    asm.data
        .db("msg_ch", "'The char is %c', 13, 10, 0")
        .db("msg_i", "'The integer is %i', 13, 10, 0")
        .db("msg_scanf_i", "'Input: ', 13, 10, 0")
        .db("scanf_i", "'%i', 0");

    asm
}

fn print() -> Procedure {
    let mut print = procedure("print");
    print
        .body
        .push(Mov, [Reg(Rdx), Reg(Rcx)])
        .push(Lea, [Reg(Rcx), Id("[msg_i]".to_string())])
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
        .push(Lea, [Reg(Rcx), Id("[scanf_i]".to_string())])
        .push_cmt(
            Mov,
            [Reg(Rdx), Reg(Rsp)],
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
    Assembly::new(Procedure::new(
        "main".to_string(),
        Windows64::prologue(),
        Block::new(),
    ))
}

fn procedure<S: Into<String>>(name: S) -> Procedure {
    stack_convention::make_procedure::<_, Windows64>(name)
}

trait BodyExt {
    fn ret_zero(&mut self) -> &mut Self;
}
impl BodyExt for Block {
    fn ret_zero(&mut self) -> &mut Self {
        self.push_cmt(Xor, [Reg(Rax), Reg(Rax)], "return zero");
        self
    }
}

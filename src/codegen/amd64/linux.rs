//! Native code generation for 64-bit Linux.

use crate::il::Instruction;

use super::assembly::*;

pub fn compile(_prog: &Vec<Instruction>) -> Assembly {
    default()
}

fn default() -> Assembly {
    use Decl::*;
    let mut asm = Assembly::new();

    asm.push_decl(Extern("printf")).push_decl(Global("main"));

    asm.text
        .label("main")
        .ins_cmt("push", "rbp", "Create stack frame, aligning on 16 bytes")
        .ins("mov", "rdi, fmt_int")
        .ins("mov", "rsi, -10")
        .ins("call", "printf")
        .ins_cmt("pop", "rbp", "Pop stack")
        .ins_cmt("mov", "rax, 0", "Set return value to 0")
        .ins("ret", "");

    asm.data.ins("fmt_int", "db '%i', 0");

    asm
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn default_creates_assembly() {
        Assembly::default();
    }
}

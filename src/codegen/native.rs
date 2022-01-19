//! Native code generation for Linux x64.
use std::fmt::{self, Display, Formatter};

use crate::parser::syntax_tree::Program;

pub fn compile(_prog: &Program) -> Assembly {
    Assembly::default()
}

type Str = &'static str;

enum Decl {
    Extern(Str),
    Global(Str),
}
impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Decl::Extern(e) => write!(f, "extern {}", e),
            Decl::Global(g) => write!(f, "global {}", g),
        }
    }
}

struct Line {
    instr: Instr,
    comment: Option<Str>,
}
impl Display for Line {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.comment {
            None => self.instr.fmt(f),
            Some(cmt) => {
                let line = self.instr.to_string();
                write!(f, "{:24}; {}", line, cmt)
            }
        }
    }
}

enum Instr {
    Instr(Str),
    Label(Str),
}
impl Display for Instr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instr::Instr(i) => write!(f, "    {}", i),
            Instr::Label(l) => write!(f, "{}:", l),
        }
    }
}

#[derive(Default)]
pub struct Assembly {
    declarations: Vec<Decl>,
    text: Section,
    data: Section,
}

impl Assembly {
    pub fn new() -> Self {
        Self {
            text: Section::new(".text"),
            data: Section::new(".data"),
            declarations: vec![],
        }
    }

    pub fn default() -> Self {
        use Decl::*;
        let mut asm = Self::new();

        asm.declarations.push(Extern("printf"));
        asm.declarations.push(Global("main"));

        asm.text
            .push_label("main")
            .push_cmt("push rbp", "Create stack frame, aligning on 16 bytes")
            .push("mov rdi, fmt_int")
            .push("mov rsi, -10")
            .push("call printf")
            .push_cmt("pop rbp", "Pop stack")
            .push_cmt("mov rax, 0", "Set return value to 0")
            .push("ret");

        asm.data.push("fmt_int db '%i', 0");

        asm
    }
}

impl Display for Assembly {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for decl in &self.declarations {
            writeln!(f, "{}", decl)?;
        }
        writeln!(f)?;

        write!(f, "{}", self.text)?;
        write!(f, "{}", self.data)
    }
}

#[derive(Default)]
pub struct Section {
    name: Str,
    lines: Vec<Line>,
}
impl Section {
    pub fn new(name: Str) -> Self {
        Self {
            name,
            lines: vec![],
        }
    }
    pub fn push(&mut self, instr: Str) -> &mut Self {
        self.lines.push(Line {
            instr: Instr::Instr(instr),
            comment: None,
        });
        self
    }

    pub fn push_cmt(&mut self, instr: Str, comment: Str) -> &mut Self {
        self.lines.push(Line {
            instr: Instr::Instr(instr),
            comment: Some(comment),
        });
        self
    }

    pub fn push_label(&mut self, label: Str) -> &mut Self {
        self.lines.push(Line {
            instr: Instr::Label(label),
            comment: None,
        });
        self
    }
}
impl Display for Section {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "section {}", self.name)?;
        for line in &self.lines {
            writeln!(f, "{}", line)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn default_creates_assembly() {
        Assembly::default();
    }
}

//! General assembly definitions. Some of these are specific to NASM.

use std::fmt::{self, Display, Formatter};

use super::x86::{Op, Register};

pub type Str = &'static str;

/// A complete assembly file.
pub struct Assembly {
    declarations: Vec<Decl>,
    pub text: Text,
    pub data: Data,
}
impl Assembly {
    pub fn new(main: Procedure) -> Self {
        Self {
            text: Text::new(main),
            data: Data::new(),
            declarations: vec![],
        }
    }

    pub fn push_decl(&mut self, decl: Decl) -> &mut Self {
        self.declarations.push(decl);
        self
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

/// An assembly section, such as `.text` or `.data`.
pub trait Section {
    fn name(&self) -> Str;
}

/// A `.data` section.
pub struct Data {
    lines: Vec<Line<Directive>>,
}
impl Data {
    pub fn new() -> Self {
        Self { lines: vec![] }
    }

    pub fn db(&mut self, name: Str, content: Str) -> &mut Self {
        self.lines.push(Line::new(Directive::Db(name, content)));
        self
    }
}
impl Section for Data {
    fn name(&self) -> Str {
        ".data"
    }
}
impl Display for Data {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "section {}", self.name())?;
        for line in &self.lines {
            writeln!(f, "{}", line)?;
        }

        Ok(())
    }
}

/// A section of assembly code, such as `.text` or `.data`.
pub struct Text {
    pub main: Procedure,
    pub procedures: Vec<Procedure>,
}
impl Text {
    pub fn new(main: Procedure) -> Self {
        Self {
            main,
            procedures: vec![],
        }
    }
}
impl Section for Text {
    fn name(&self) -> Str {
        ".text"
    }
}
impl Display for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "section {}", self.name())?;
        writeln!(f, "{}", self.main)?;

        for proc in &self.procedures {
            writeln!(f, "{}", proc)?;
        }

        Ok(())
    }
}

pub struct Procedure {
    pub name: Str,
    pub prologue: Block,
    pub body: Block,
    pub epilogue: Block,
}
impl Procedure {
    pub fn new(name: Str, prologue: Block, epilogue: Block) -> Self {
        Self {
            name,
            prologue,
            body: Block::new(),
            epilogue,
        }
    }
}
impl Display for Procedure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}:", self.name)?;
        write!(f, "{}", self.prologue)?;
        write!(f, "{}", self.body)?;
        write!(f, "{}", self.epilogue)
    }
}

pub enum Decl {
    Bits(usize),
    Default(Str),
    Extern(Str),
    Global(Str),
}
impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Decl::Extern(e) => write!(f, "extern {}", e),
            Decl::Global(g) => write!(f, "global {}", g),
            Decl::Bits(b) => write!(f, "bits {}", b),
            Decl::Default(d) => write!(f, "default {}", d),
        }
    }
}

pub struct Block {
    lines: Vec<Line<Instr>>,
}
impl Block {
    pub fn new() -> Self {
        Self { lines: vec![] }
    }

    pub fn push(&mut self, op: Op, operands: Vec<Operand>) -> &mut Self {
        self.lines.push(Line::new(Instr::new(op, operands)));
        self
    }

    pub fn push_cmt<S: Into<String>>(
        &mut self,
        op: Op,
        operands: Vec<Operand>,
        comment: S,
    ) -> &mut Self {
        self.lines
            .push(Line::new_cmt(Instr::new(op, operands), comment.into()));
        self
    }

    pub fn blank(&mut self) -> &mut Self {
        self.lines.push(Line::new_blank());
        self
    }
}
impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for line in &self.lines {
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

/// A line of assembly, with an instruction and optional comment.
pub struct Line<T> {
    dir: Option<T>,
    comment: Option<String>,
}
impl<T> Line<T> {
    /// Construct a new line without comment.
    pub fn new(dir: T) -> Self {
        Self {
            dir: Some(dir),
            comment: None,
        }
    }
    /// Construct a new line with a comment.
    pub fn new_cmt(dir: T, comment: String) -> Self {
        Self {
            dir: Some(dir),
            comment: Some(comment),
        }
    }

    /// Construct an empty line.
    pub fn new_blank() -> Self {
        Self {
            dir: None,
            comment: None,
        }
    }
}
impl<T: Display> Display for Line<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match (&self.dir, self.comment.as_ref()) {
            (None, None) => Ok(()),
            (None, Some(cmt)) => write!(f, "                                {}", cmt),
            (Some(dir), None) => {
                write!(f, "{}", dir.to_string())
            }
            (Some(dir), Some(cmt)) => {
                write!(f, "{:32}; {}", dir.to_string(), cmt)
            }
        }
    }
}

pub enum Directive {
    Db(Str, Str),
}
impl Display for Directive {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Directive::Db(name, value) => write!(f, "    {:7} db {}", name, value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instr {
    operator: Op,
    operands: Vec<Operand>,
}
impl Instr {
    pub fn new(operator: Op, operands: Vec<Operand>) -> Instr {
        Self { operator, operands }
    }
}
impl Display for Instr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let operator = self.operator.to_string();
        write!(f, "    {:7} ", operator)?;
        let operands = self
            .operands
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        f.write_str(&operands)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Reg(Register),
    Lit(usize),
    Id(Str),
}
impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Operand::Reg(reg) => write!(f, "{}", reg),
            Operand::Lit(lit) => write!(f, "{}", lit),
            Operand::Id(str) => f.write_str(str),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use Operand::*;
    use Register::*;

    #[test]
    pub fn instr_serializes_correctly() {
        let instr = Instr::new(Op::Mov, vec![Reg(RAX), Reg(RCX)]);

        assert_eq!("    mov     rax, rcx", instr.to_string());
    }
}

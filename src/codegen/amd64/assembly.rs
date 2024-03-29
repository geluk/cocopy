//! General assembly definitions. Some of these are specific to NASM.

use std::fmt::{self, Display, Formatter};

use super::x86::{Op, Register};

pub type Str = &'static str;

/// A complete assembly file, containing declarations and sections.
#[derive(Debug)]
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

/// A section of assembly code, such as `.text` or `.data`.
pub trait Section {
    fn name(&self) -> Str;
}

/// A `.data` section. For now, only directives are supported.
#[derive(Debug)]
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

/// A `.text` section. Contains a single main procedure,
/// and may contain any number of additional procedures.
#[derive(Debug)]
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

/// An assembly procedure, marked by a label and optionally surrounded by a prologue and epilogue.
#[derive(Debug)]
pub struct Procedure {
    pub name: String,
    pub prologue: Block,
    pub body: Block,
    pub epilogue: Block,
}
impl Procedure {
    pub fn new(name: String, prologue: Block, epilogue: Block) -> Self {
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

/// A declaration, used to provide hints to the assembler.
#[derive(Debug)]
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

/// A block of assembly code.
#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    lines: Vec<Line<Instr>>,
}
impl Block {
    pub fn new() -> Self {
        Self { lines: vec![] }
    }

    pub fn push<V: Into<Vec<Operand>>>(&mut self, op: Op, operands: V) -> &mut Self {
        self.lines.push(Line::new(Instr::new(op, operands.into())));
        self
    }

    pub fn push_cmt<V: Into<Vec<Operand>>, S: Into<String>>(
        &mut self,
        op: Op,
        operands: V,
        comment: S,
    ) -> &mut Self {
        self.lines.push(Line::new_cmt(
            Instr::new(op, operands.into()),
            comment.into(),
        ));
        self
    }

    pub fn push_cmt_only<S: Into<String>>(&mut self, comment: S) -> &mut Self {
        let mut line = Line::new_blank();
        line.comment = Some(comment.into());
        self.lines.push(line);
        self
    }

    /// Add a label to the last line
    pub fn add_label(&mut self, label: String) {
        let mut line = Line::new_blank();
        line.label = Some(label);
        self.lines.push(line);
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

/// A line of assembly, consisting of an optional instruction and optional comment.
/// When the instruction is [`None`], an empty line is emitted.
#[derive(Debug, Eq)]
pub struct Line<T> {
    label: Option<String>,
    line: Option<T>,
    comment: Option<String>,
}
impl<T> Line<T> {
    /// Construct a new line without comment or label.
    pub fn new(dir: T) -> Self {
        Self {
            label: None,
            line: Some(dir),
            comment: None,
        }
    }
    /// Construct a new line with a comment and no label.
    pub fn new_cmt(dir: T, comment: String) -> Self {
        Self {
            label: None,
            line: Some(dir),
            comment: Some(comment),
        }
    }

    /// Construct an empty line.
    pub fn new_blank() -> Self {
        Self {
            label: None,
            line: None,
            comment: None,
        }
    }

    /// Add a label to this line
    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}
impl<T: PartialEq> PartialEq for Line<T> {
    fn eq(&self, other: &Self) -> bool {
        // Ignore comments in equality checks
        self.line == other.line
    }
}
impl<T: Display> Display for Line<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(lbl) = &self.label {
            if lbl.starts_with('.') {
                writeln!(f, "  {}:", lbl)?;
            } else {
                writeln!(f, "{}:", lbl)?;
            }
        }

        match (&self.line, self.comment.as_ref()) {
            (None, None) => Ok(()),
            (None, Some(cmt)) => write!(f, "                                ; {}", cmt),
            (Some(dir), None) => {
                write!(f, "{}", dir)
            }
            (Some(dir), Some(cmt)) => {
                write!(f, "{:32}; {}", dir.to_string(), cmt)
            }
        }
    }
}

/// A directive as used in the `.text` section. Currently, only `db` is supported.
#[derive(Debug)]
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

/// A single instruction, consisting of an operator and zero or more operands.
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
        if self.operands.is_empty() {
            write!(f, "    {}", operator)
        } else {
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
}

/// An operand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// A register
    Reg(Register),
    /// An immediate value
    Lit(i128),
    /// A label
    Lbl(String),
    /// An identifier
    Id(String),
    /// A pointer
    Ptr(PtrSize, Register),
}
impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Operand::Reg(reg) => write!(f, "{}", reg),
            Operand::Lit(lit) => write!(f, "{}", lit),
            Operand::Id(str) => f.write_str(str),
            Operand::Lbl(lbl) => f.write_str(lbl),
            Operand::Ptr(size, inner) => write!(f, "{} [{}]", size, *inner),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PtrSize {
    Byte,
    Word,
    Dword,
    Qword,
}
impl Display for PtrSize {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PtrSize::Byte => f.write_str("byte"),
            PtrSize::Word => f.write_str("word"),
            PtrSize::Dword => f.write_str("dword"),
            PtrSize::Qword => f.write_str("qword"),
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
        let instr = Instr::new(Op::Mov, vec![Reg(Rax), Reg(Rcx)]);

        assert_eq!("    mov     rax, rcx", instr.to_string());
    }
}

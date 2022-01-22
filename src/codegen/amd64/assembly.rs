//! General x86 assembly definitions.

use std::fmt::{self, Display, Formatter};

pub type Str = &'static str;

#[derive(Default)]
pub struct Assembly {
    declarations: Vec<Decl>,
    pub text: Section,
    pub data: Section,
}
impl Assembly {
    pub fn new() -> Self {
        Self {
            text: Section::new(".text"),
            data: Section::new(".data"),
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
    pub fn ins(&mut self, op: Str, args: Str) -> &mut Self {
        self.lines.push(Line::new(Directive::Instr(op, args)));
        self
    }

    pub fn ins_cmt(&mut self, op: Str, args: Str, comment: Str) -> &mut Self {
        self.lines
            .push(Line::new_cmt(Directive::Instr(op, args), comment));
        self
    }

    pub fn label(&mut self, label: Str) -> &mut Self {
        self.lines.push(Line::new(Directive::Label(label)));
        self
    }

    pub fn push_break(&mut self) -> &mut Self {
        self.lines.push(Line::new_break());
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

/// A line of assembly, with an instruction and optional comment.
pub struct Line {
    dir: Option<Directive>,
    comment: Option<Str>,
}
impl Line {
    /// Construct a new line without comment.
    pub fn new(dir: Directive) -> Self {
        Self {
            dir: Some(dir),
            comment: None,
        }
    }
    /// Construct a new line with a comment.
    pub fn new_cmt(dir: Directive, comment: Str) -> Self {
        Self {
            dir: Some(dir),
            comment: Some(comment),
        }
    }

    /// Construct an empty line.
    pub fn new_break() -> Self {
        Self {
            dir: None,
            comment: None,
        }
    }
}
impl Display for Line {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match (&self.dir, self.comment) {
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
    Instr(Str, Str),
    Label(Str),
}
impl Display for Directive {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Directive::Instr(op, args) => write!(f, "    {:8}{}", op, args),
            Directive::Label(l) => write!(f, "{}:", l),
        }
    }
}

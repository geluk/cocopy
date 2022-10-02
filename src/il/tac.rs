//! Three-Address Code

use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{self, Display, Formatter},
    hash::Hash,
};

use crate::{
    ast::typed::{BinOp, CmpOp},
    builtins::Builtin,
    listing::{Listing, Position},
};

pub type TargetSize = isize;

pub type TacListing = Listing<Instruction>;

#[derive(Debug)]
pub struct TacProgram {
    /// User-defined functions.
    pub functions: HashMap<String, TacListing>,
    /// Built-in functions. These functions are not compiled from Python source
    /// code, and are instead directly emitted as assembly code.
    pub builtins: HashMap<String, Builtin>,
    /// The top-level (main) function, containing all code not associated
    /// with another function.
    pub top_level: TacListing,
}
impl TacProgram {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            builtins: HashMap::new(),
            top_level: TacListing::new(),
        }
    }
}
impl Display for TacProgram {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "function main")?;
        for instr in self.top_level.iter_instructions() {
            if instr.label.is_some() {
                writeln!(f, "    {}", instr)?;
            } else {
                writeln!(f, "        {}", instr)?;
            }
        }

        for (name, body) in self.functions.iter() {
            writeln!(f, "function {}", name)?;
            for instr in body.iter_instructions() {
                if instr.label.is_some() {
                    writeln!(f, "    {}", instr)?;
                } else {
                    writeln!(f, "        {}", instr)?;
                }
            }
        }

        Ok(())
    }
}

impl Listing<Instruction> {
    pub fn is_used_after(&self, name: &Name, position: Position) -> bool {
        // We can probably always subtract 1 from `len` here
        if position.0 >= self.len() {
            return false;
        }

        self.iter_lines()
            .skip_while(|(l, _)| l < &position)
            .any(|(_, instr)| instr.reads_from_name(name))
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstrKind,
    pub label: Option<Label>,
}
impl Instruction {
    pub fn new(kind: InstrKind) -> Self {
        Self { kind, label: None }
    }

    pub fn add_label(&mut self, label: Label) {
        self.label = Some(label);
    }

    pub fn reads_from_name(&self, name: &Name) -> bool {
        self.kind.reads_from_name(name)
    }

    pub fn may_replace(&self, name: &Name) -> bool {
        self.kind.may_replace(name)
    }

    pub fn may_delete(&self) -> bool {
        // Instructions with labels on them may be jumped to, and should never be deleted.
        // TODO: separate instructions from labels to prevent this.
        self.label.is_none()
    }

    pub fn replace(&mut self, src: &Name, dest: Cow<Value>) {
        self.kind.replace(src, dest)
    }

    pub fn as_assign(&self) -> Option<(&Name, &Value)> {
        self.kind.as_assign()
    }

    pub fn as_call(&self) -> Option<(Option<&Name>, &String, usize)> {
        self.kind.as_call()
    }

    pub fn as_phi(&self) -> Option<(&Name, &Vec<Name>)> {
        self.kind.as_phi()
    }

    pub fn replace_assign(&mut self, src: &Name, dest: Cow<Name>) {
        self.kind.replace_assign(src, dest)
    }
}
impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.label {
            Some(lbl) => write!(f, "{}: {}", lbl, self.kind),
            None => self.kind.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
    name: String,
    subscript: usize,
}
impl Label {
    pub fn new(name: String, subscript: usize) -> Self {
        Self { name, subscript }
    }
}
impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}_{}", self.name, self.subscript)
    }
}

/// A single TAC instruction.
#[derive(Debug, Clone)]
pub enum InstrKind {
    /// Assign a value to a name.
    Assign(Name, Value),
    /// Perform a binary operation.
    Bin(Name, BinOp, Value, Value),
    /// Jump to a label.
    Goto(Label),
    /// Jump if a value is true.
    IfTrue(Value, Label),
    /// Jump if a value is false.
    IfFalse(Value, Label),
    /// Jump if a comparison evaluates to true.
    IfCmp(Value, CmpOp, Value, Label),
    /// Push an argument to the argument stack.
    Arg(Value),
    /// Pop a parameter from the parameter stack.
    Param(Name),
    /// Call a function, passing `n` parameters.
    Call(Option<Name>, String, usize),
    /// Return a value from a function body.
    Return(Option<Value>),
    /// The ɸ-function.
    Phi(Name, Vec<Name>),
    /// No-op
    Nop,
}
impl InstrKind {
    pub fn reads_from_name(&self, name: &Name) -> bool {
        match self {
            InstrKind::Assign(_, value) => Self::is_usage_of(name, value),
            InstrKind::Bin(_, _, lhs, rhs) => {
                Self::is_usage_of(name, lhs) || Self::is_usage_of(name, rhs)
            }
            InstrKind::Arg(value) => Self::is_usage_of(name, value),
            InstrKind::Param(_) => false,
            InstrKind::Call(_, _, _) => false,
            InstrKind::Nop => false,
            InstrKind::Goto(_) => false,
            InstrKind::IfTrue(value, _) => Self::is_usage_of(name, value),
            InstrKind::IfFalse(value, _) => Self::is_usage_of(name, value),
            InstrKind::IfCmp(lhs, _, rhs, _) => {
                Self::is_usage_of(name, lhs) || Self::is_usage_of(name, rhs)
            }
            InstrKind::Return(None) => false,
            InstrKind::Return(Some(value)) => Self::is_usage_of(name, value),
            InstrKind::Phi(_, names) => names.iter().any(|n| n == name),
        }
    }

    pub fn may_replace(&self, name: &Name) -> bool {
        match self {
            // Names referenced by the ɸ-function may never be replaced with a value
            InstrKind::Phi(_, names) => names.iter().all(|n| n != name),
            _ => true,
        }
    }

    /// Replace all occurrences of a value in this instruction with another value.
    pub fn replace(&mut self, src: &Name, dest: Cow<Value>) {
        fn try_replace(tgt: &mut Value, src: &Name, dest: Cow<Value>) {
            if let Value::Name(name) = tgt {
                if name == src {
                    *tgt = dest.into_owned();
                }
            }
        }
        match self {
            InstrKind::Assign(_, value) => try_replace(value, src, dest),
            InstrKind::Bin(_, _, lhs, rhs) => {
                try_replace(lhs, src, dest.clone());
                try_replace(rhs, src, dest);
            }
            InstrKind::Arg(arg) => try_replace(arg, src, dest),
            InstrKind::Param(_) => (),
            InstrKind::Call(_, _, _) => (),
            InstrKind::Nop => (),
            InstrKind::Goto(_) => (),
            InstrKind::IfTrue(value, _) => try_replace(value, src, dest),
            InstrKind::IfFalse(value, _) => try_replace(value, src, dest),
            InstrKind::IfCmp(lhs, _, rhs, _) => {
                try_replace(lhs, src, dest.clone());
                try_replace(rhs, src, dest);
            }
            InstrKind::Return(None) => (),
            InstrKind::Return(Some(value)) => try_replace(value, src, dest),
            InstrKind::Phi(_, values) => {
                let any_replaced = values.iter().any(|n| n == src);
                assert!(
                    !any_replaced,
                    "Optimiser error! Replacing a name used in the phi function is not allowed ({} -> {})", src, dest
                )
            }
        }
    }

    pub fn as_assign(&self) -> Option<(&Name, &Value)> {
        match self {
            InstrKind::Assign(name, value) => Some((name, value)),
            _ => None,
        }
    }

    fn as_call(&self) -> Option<(Option<&Name>, &String, usize)> {
        match self {
            InstrKind::Call(name, function, params) => Some((name.as_ref(), function, *params)),
            _ => None,
        }
    }

    pub fn as_phi(&self) -> Option<(&Name, &Vec<Name>)> {
        match self {
            InstrKind::Phi(target, params) => Some((target, params)),
            _ => None,
        }
    }

    fn replace_assign(&mut self, src: &Name, dest: Cow<Name>) {
        fn try_replace(tgt: &mut Name, src: &Name, dest: Cow<Name>) {
            if tgt == src {
                *tgt = dest.into_owned();
            }
        }
        match self {
            InstrKind::Assign(tgt, _) => try_replace(tgt, src, dest),
            InstrKind::Bin(tgt, _, _, _) => try_replace(tgt, src, dest),
            InstrKind::Goto(_) => (),
            InstrKind::IfTrue(_, _) => (),
            InstrKind::IfFalse(_, _) => (),
            InstrKind::IfCmp(_, _, _, _) => (),
            InstrKind::Arg(_) => (),
            InstrKind::Param(tgt) => try_replace(tgt, src, dest),
            InstrKind::Call(Some(tgt), _, _) => try_replace(tgt, src, dest),
            InstrKind::Call(_, _, _) => (),
            InstrKind::Return(_) => (),
            InstrKind::Phi(tgt, _) => try_replace(tgt, src, dest),
            InstrKind::Nop => (),
        }
    }

    fn is_usage_of(name: &Name, value: &Value) -> bool {
        match value {
            Value::Name(n) => name == n,
            Value::Const(_) => false,
        }
    }
}
impl Display for InstrKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InstrKind::Assign(target, value) => write!(f, "{} = {}", target, value),
            InstrKind::Bin(target, op, lhs, rhs) => {
                write!(f, "{} = {} {} {}", target, lhs, op, rhs)
            }
            InstrKind::Arg(p) => write!(f, "arg {}", p),
            InstrKind::Param(a) => write!(f, "{} = param", a),
            InstrKind::Call(Some(name), tgt, params) => {
                write!(f, "{} = call {}, {}", name, tgt, params)
            }
            InstrKind::Call(None, tgt, params) => {
                write!(f, "call {}, {}", tgt, params)
            }
            InstrKind::Nop => f.write_str("nop"),
            InstrKind::Goto(label) => write!(f, "goto {}", label),
            InstrKind::IfTrue(value, lbl) => write!(f, "if_true {} goto {}", value, lbl),
            InstrKind::IfFalse(value, lbl) => write!(f, "if_false {} goto {}", value, lbl),
            InstrKind::IfCmp(lhs, op, rhs, lbl) => {
                write!(f, "if {} {} {} goto {}", lhs, op, rhs, lbl)
            }
            InstrKind::Return(None) => f.write_str("return"),
            InstrKind::Return(Some(value)) => write!(f, "return {}", value),
            InstrKind::Phi(name, values) => {
                let args = values
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} = ɸ({})", name, args)
            }
        }
    }
}

/// A TAC name. Names are symbolic addresses and may represent variables in the original source
/// code, or intermediate values of complex computations that have been broken down.
///
/// Each name is only ever assigned to once. If a variable in the source code is assigned to
/// multiple times, the IL generator will subscript each 'assignment lifetime'.
/// In other words, given the following source code:
/// ```
/// x = 10
/// y = 42 + x
/// x = y - 2
/// ```
/// The following TAC is generated:
/// ```
/// x^1 = 10
/// y^1 = 42 + x^1
/// x^2 = y^1 - 2
/// ```
/// This makes it easier to reason about the lifetime of each variable, which in turn allows us
/// to better optimise the code.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Name {
    /// A subscripted variable.
    Sub(Variable),
    /// A generated, temporary name.
    Temp(usize),
}
impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Name::Sub(sub) => sub.fmt(f),
            Name::Temp(temp) => write!(f, "%{}", temp),
        }
    }
}

/// A subscripted variable.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Variable {
    pub name: String,
    pub subscript: usize,
}
impl Variable {
    pub fn new(name: String, subscript: usize) -> Self {
        Self { name, subscript }
    }
}
impl Display for Variable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}^{}", self.name, self.subscript)
    }
}

/// A TAC value. Values can be constants, or references to names that were
/// defined earlier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A constant, specified in the target size of the destination platform.
    Const(TargetSize),
    /// A name, representing either a temporary name or a variable in the source program.
    Name(Name),
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Value::Const(lit) => write!(f, "{}", lit),
            Value::Name(name) => write!(f, "{}", name),
        }
    }
}

pub struct LineNumberIter<I, F> {
    iter: I,
    f: F,
}
impl<A, B, I, F> Iterator for LineNumberIter<I, F>
where
    I: Iterator<Item = (Position, A)>,
    F: FnMut(A) -> Option<B>,
{
    type Item = (Position, B);

    fn next(&mut self) -> Option<(Position, B)> {
        self.iter
            .find_map(|(line, a)| (self.f)(a).map(|x| (line, x)))
    }
}

pub trait MatchInstruction<A, I> {
    fn match_instruction<P, B>(self, pred: P) -> LineNumberIter<I, P>
    where
        P: Fn(A) -> Option<B>;
}
impl<'a, I> MatchInstruction<&'a Instruction, I> for I
where
    I: Iterator<Item = (Position, &'a Instruction)>,
{
    fn match_instruction<P, B>(self, pred: P) -> LineNumberIter<I, P>
    where
        P: Fn(&'a Instruction) -> Option<B>,
    {
        LineNumberIter {
            iter: self,
            f: pred,
        }
    }
}

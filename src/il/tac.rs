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

pub type TacListing = Listing<TacInstr>;

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
            if let TacInstr::Label(_) = instr {
                writeln!(f, "    {}", instr)?;
            } else {
                writeln!(f, "        {}", instr)?;
            }
        }

        for (name, body) in self.functions.iter() {
            writeln!(f, "function {}", name)?;
            for instr in body.iter_instructions() {
                if let TacInstr::Label(_) = instr {
                    writeln!(f, "    {}", instr)?;
                } else {
                    writeln!(f, "        {}", instr)?;
                }
            }
        }

        Ok(())
    }
}

impl Listing<TacInstr> {
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
pub enum TacInstr {
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
    /// A label which can be jumped to.
    Label(Label),
    /// The ɸ-function.
    Phi(Name, Vec<Name>),
}
impl TacInstr {
    pub fn reads_from_name(&self, name: &Name) -> bool {
        match self {
            Self::Assign(_, value) => Self::is_usage_of(name, value),
            Self::Bin(_, _, lhs, rhs) => {
                Self::is_usage_of(name, lhs) || Self::is_usage_of(name, rhs)
            }
            Self::Arg(value) => Self::is_usage_of(name, value),
            Self::Param(_) => false,
            Self::Call(_, _, _) => false,
            Self::Goto(_) => false,
            Self::IfTrue(value, _) => Self::is_usage_of(name, value),
            Self::IfFalse(value, _) => Self::is_usage_of(name, value),
            Self::IfCmp(lhs, _, rhs, _) => {
                Self::is_usage_of(name, lhs) || Self::is_usage_of(name, rhs)
            }
            Self::Return(None) => false,
            Self::Return(Some(value)) => Self::is_usage_of(name, value),
            Self::Label(_) => false,
            Self::Phi(_, names) => names.iter().any(|n| n == name),
        }
    }

    pub fn may_replace(&self, name: &Name) -> bool {
        match self {
            // Names referenced by the ɸ-function may never be replaced with a value
            Self::Phi(_, names) => names.iter().all(|n| n != name),
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
            Self::Assign(_, value) => try_replace(value, src, dest),
            Self::Bin(_, _, lhs, rhs) => {
                try_replace(lhs, src, dest.clone());
                try_replace(rhs, src, dest);
            }
            Self::Arg(arg) => try_replace(arg, src, dest),
            Self::Param(_) => (),
            Self::Call(_, _, _) => (),
            Self::Goto(_) => (),
            Self::IfTrue(value, _) => try_replace(value, src, dest),
            Self::IfFalse(value, _) => try_replace(value, src, dest),
            Self::IfCmp(lhs, _, rhs, _) => {
                try_replace(lhs, src, dest.clone());
                try_replace(rhs, src, dest);
            }
            Self::Return(None) => (),
            Self::Return(Some(value)) => try_replace(value, src, dest),
            Self::Label(_) => (),
            Self::Phi(_, values) => {
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
            Self::Assign(name, value) => Some((name, value)),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<(Option<&Name>, &String, usize)> {
        match self {
            Self::Call(name, function, params) => Some((name.as_ref(), function, *params)),
            _ => None,
        }
    }

    pub fn as_phi(&self) -> Option<(&Name, &Vec<Name>)> {
        match self {
            Self::Phi(target, params) => Some((target, params)),
            _ => None,
        }
    }

    pub fn replace_assign(&mut self, src: &Name, dest: Cow<Name>) {
        fn try_replace(tgt: &mut Name, src: &Name, dest: Cow<Name>) {
            if tgt == src {
                *tgt = dest.into_owned();
            }
        }
        match self {
            Self::Assign(tgt, _) => try_replace(tgt, src, dest),
            Self::Bin(tgt, _, _, _) => try_replace(tgt, src, dest),
            Self::Goto(_) => (),
            Self::IfTrue(_, _) => (),
            Self::IfFalse(_, _) => (),
            Self::IfCmp(_, _, _, _) => (),
            Self::Arg(_) => (),
            Self::Param(tgt) => try_replace(tgt, src, dest),
            Self::Call(Some(tgt), _, _) => try_replace(tgt, src, dest),
            Self::Call(_, _, _) => (),
            Self::Return(_) => (),
            Self::Label(_) => (),
            Self::Phi(tgt, _) => try_replace(tgt, src, dest),
        }
    }

    fn is_usage_of(name: &Name, value: &Value) -> bool {
        match value {
            Value::Name(n) => name == n,
            Value::Const(_) => false,
        }
    }
}
impl Display for TacInstr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Assign(target, value) => write!(f, "{} = {}", target, value),
            Self::Bin(target, op, lhs, rhs) => {
                write!(f, "{} = {} {} {}", target, lhs, op, rhs)
            }
            Self::Arg(p) => write!(f, "arg {}", p),
            Self::Param(a) => write!(f, "{} = param", a),
            Self::Call(Some(name), tgt, params) => {
                write!(f, "{} = call {}, {}", name, tgt, params)
            }
            Self::Call(None, tgt, params) => {
                write!(f, "call {}, {}", tgt, params)
            }
            Self::Goto(label) => write!(f, "goto {}", label),
            Self::IfTrue(value, lbl) => write!(f, "if_true {} goto {}", value, lbl),
            Self::IfFalse(value, lbl) => write!(f, "if_false {} goto {}", value, lbl),
            Self::IfCmp(lhs, op, rhs, lbl) => {
                write!(f, "if {} {} {} goto {}", lhs, op, rhs, lbl)
            }
            Self::Return(None) => f.write_str("return"),
            Self::Return(Some(value)) => write!(f, "return {}", value),
            Self::Label(lbl) => write!(f, "{}:", lbl),
            Self::Phi(name, values) => {
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
impl<'a, I> MatchInstruction<&'a TacInstr, I> for I
where
    I: Iterator<Item = (Position, &'a TacInstr)>,
{
    fn match_instruction<P, B>(self, pred: P) -> LineNumberIter<I, P>
    where
        P: Fn(&'a TacInstr) -> Option<B>,
    {
        LineNumberIter {
            iter: self,
            f: pred,
        }
    }
}

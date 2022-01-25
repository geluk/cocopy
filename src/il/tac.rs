//! Three-Address Code

use std::{
    fmt::{self, Display, Formatter},
    iter::Enumerate,
    slice::{Iter, IterMut},
};

use crate::ast::untyped::BinOp;

pub type TargetSize = isize;

/// A listing of three-address code. This will normally represent a function body
/// or the top-level function.
pub struct TacListing {
    instructions: Vec<Instruction>,
}
impl TacListing {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
        }
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    pub fn iter_lines(&self) -> Enumerate<Iter<Instruction>> {
        self.instructions.iter().enumerate()
    }

    pub fn iter_instructions(&self) -> Iter<Instruction> {
        self.instructions.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<Instruction> {
        self.instructions.iter_mut()
    }

    pub fn remove(&mut self, line: usize) {
        self.instructions.remove(line);
    }

    pub fn into_vec(self) -> Vec<Instruction> {
        self.instructions
    }

    pub fn is_used_after(&self, name: &Name, index: usize) -> bool {
        // We can probably always subtract 1 from `len` here
        if index >= self.instructions.len() {
            return false;
        }

        self.instructions[index..]
            .iter()
            .any(|instr| instr.reads_from_name(name))
    }
}

/// A single TAC instruction.
#[derive(Debug)]
pub enum Instruction {
    Assign(Name, Value),
    Bin(Name, BinOp, Value, Value),
}
impl Instruction {
    pub fn reads_from_name(&self, name: &Name) -> bool {
        match self {
            Instruction::Assign(_, value) => Self::is_usage_of(name, value),
            Instruction::Bin(_, _, lhs, rhs) => {
                Self::is_usage_of(name, lhs) || Self::is_usage_of(name, rhs)
            }
        }
    }

    /// Replace all occurrences a value in this instruction with another value.
    pub fn replace(&mut self, src: &Value, dest: Value) {
        match self {
            Instruction::Assign(_, value) if value == src => *value = dest,
            Instruction::Bin(_, _, l, r) => {
                if l == src {
                    *l = dest.clone();
                }
                if r == src {
                    *r = dest.clone();
                }
            }
            _ => (),
        }
    }

    pub fn as_assign(&self) -> Option<(&Name, &Value)> {
        match self {
            Instruction::Assign(name, value) => Some((name, value)),
            _ => None,
        }
    }

    fn is_usage_of(name: &Name, value: &Value) -> bool {
        match value {
            Value::Name(n) => name == n,
            Value::Const(_) => false,
        }
    }
}
impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instruction::Assign(target, value) => write!(f, "{} = {}", target, value),
            Instruction::Bin(target, op, lhs, rhs) => {
                write!(f, "{} = {} {} {}", target, lhs, op, rhs)
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
    Sub(String, usize),
    Temp(String),
}
impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Name::Sub(name, sub) => write!(f, "{}^{}", name, sub),
            Name::Temp(temp) => write!(f, "%{}", temp),
        }
    }
}

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
    I: Iterator<Item = (usize, A)>,
    F: FnMut(A) -> Option<B>,
{
    type Item = (usize, B);

    fn next(&mut self) -> Option<(usize, B)> {
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
    I: Iterator<Item = (usize, &'a Instruction)>,
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

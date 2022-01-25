//! Three-Address Code

use std::fmt::{self, Display, Formatter};

use crate::ast::untyped::BinOp;

pub type TargetSize = isize;

#[derive(Debug)]
pub enum Instruction {
    Assign(Name, Value),
    Bin(Name, BinOp, Value, Value),
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

#[derive(Debug)]
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

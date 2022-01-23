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
    Name(String),
    Temp(String),
}
impl Name {
    pub fn for_id(id: &str) -> Self {
        Self::Name(id.to_string())
    }
}
impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Name::Name(name) => write!(f, "{}", name),
            Name::Temp(temp) => write!(f, "%{}", temp),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    /// A literal, specified in the target size of the destination platform.
    Lit(TargetSize),
    /// A name, representing either a temporary name or a variable in the source program.
    Name(Name),
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Value::Lit(lit) => write!(f, "{}", lit),
            Value::Name(name) => write!(f, "{}", name),
        }
    }
}

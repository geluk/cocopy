//! ChocoPy type specifications.
use std::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A ChocoPy type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    None,
    Int,
    Bool,
    Str,
    Array(Box<TypeSpec>),
    Function(Vec<TypeSpec>, Box<TypeSpec>),
}
impl Display for TypeSpec {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeSpec::None => f.write_str("<None>"),
            TypeSpec::Int => f.write_str("int"),
            TypeSpec::Bool => f.write_str("bool"),
            TypeSpec::Str => f.write_str("str"),
            TypeSpec::Array(inner) => write!(f, "[{}]", inner),
            TypeSpec::Function(params, ret) => {
                f.write_str("(")?;
                for param in params {
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", ret)
            }
        }
    }
}
impl FromStr for TypeSpec {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => TypeSpec::Int,
            "bool" => TypeSpec::Bool,
            "str" => TypeSpec::Str,
            _ => return Err(()),
        })
    }
}

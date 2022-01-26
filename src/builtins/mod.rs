use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    str::FromStr,
};

use crate::ast::TypeSpec;

pub fn get_type_map() -> HashMap<String, TypeSpec> {
    let mut type_map = HashMap::new();

    type_map.insert(
        "print".to_string(),
        TypeSpec::Function(vec![TypeSpec::Int], Box::new(TypeSpec::None)),
    );

    type_map
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Builtin {
    Print,
}
impl FromStr for Builtin {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "print" => Ok(Builtin::Print),
            _ => Err(()),
        }
    }
}
impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Builtin::Print => f.write_str("print"),
        }
    }
}

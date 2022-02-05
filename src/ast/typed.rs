use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use crate::builtins;

use super::{untyped::*, TypeSpec};

#[derive(Debug)]
pub struct Program {
    pub var_defs: Vec<VarDef>,
    pub func_defs: Vec<FuncDef>,
    pub statements: Vec<Statement>,
    pub global_environment: Environment,
}
impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for var in &self.var_defs {
            writeln!(f, "{}", var)?;
        }
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Environment {
    pub type_map: HashMap<String, TypeSpec>,
}
impl Environment {
    pub fn global() -> Self {
        Self {
            type_map: builtins::get_type_map(),
        }
    }
}

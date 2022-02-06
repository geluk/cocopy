use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use crate::builtins::Builtin;

use super::{untyped::*, TypeSpec};

#[derive(Debug)]
pub struct Program {
    pub var_defs: Vec<VarDef>,
    pub func_defs: Vec<FuncDef>,
    pub statements: Vec<Statement>,
    pub used_builtins: Vec<Builtin>,
    pub global_environment: Environment,
}
impl Program {
    pub fn new() -> Self {
        Self {
            var_defs: vec![],
            func_defs: vec![],
            statements: vec![],
            used_builtins: vec![],
            global_environment: Environment::empty(),
        }
    }
}
impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
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
    pub fn empty() -> Self {
        Self {
            type_map: HashMap::new(),
        }
    }
}

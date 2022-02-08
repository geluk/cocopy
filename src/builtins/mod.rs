use std::collections::HashMap;

use crate::ast::TypeSpec;

pub fn get_type_map() -> HashMap<String, (Builtin, TypeSpec)> {
    HashMap::from([
        (
            "print".to_string(),
            (
                Builtin::Print,
                TypeSpec::Function(vec![TypeSpec::Int], Box::new(TypeSpec::None)),
            ),
        ),
        (
            "readint".to_string(),
            (
                Builtin::Readint,
                TypeSpec::Function(vec![], Box::new(TypeSpec::Int)),
            ),
        ),
    ])
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Builtin {
    Print,
    Readint,
}

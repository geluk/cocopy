use crate::parser::syntax_tree::BinExpr;

#[allow(dead_code)]
mod llvm;
#[allow(dead_code)]
mod native;

#[allow(dead_code)]
pub fn generate_llvm() {}

#[allow(dead_code)]
pub fn generate_native(expression: BinExpr) {
    native::compile(expression);
}

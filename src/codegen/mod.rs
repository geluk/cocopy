use crate::parser::syntax_tree::BinExpr;

mod native;

pub fn generate_llvm() {
    todo!();
}

pub fn generate_native(expression: BinExpr) {
    native::compile(expression);
}

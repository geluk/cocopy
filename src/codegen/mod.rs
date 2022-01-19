//! Everything related to code generation.
use std::path::Path;

use anyhow::Result;

use crate::parser::syntax_tree::Program;

#[allow(dead_code)]
mod llvm;
mod native;

#[allow(dead_code)]
pub fn generate_llvm() {}

pub fn generate_native<P: AsRef<Path>>(prog: &Program, target: P) -> Result<()> {
    let assembly = native::compile(prog);

    std::fs::write(target, assembly.to_string())?;
    Ok(())
}

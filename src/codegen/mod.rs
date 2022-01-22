//! Target code generation.
use std::path::Path;

use anyhow::Result;

use crate::il::Instruction;

#[allow(dead_code)]
mod llvm;
mod native;

#[allow(dead_code)]
pub fn generate_llvm() {}

pub fn generate_native<P: AsRef<Path>>(prog: &Vec<Instruction>, target: P) -> Result<()> {
    let assembly = native::compile(prog);

    std::fs::write(target, assembly.to_string())?;
    Ok(())
}

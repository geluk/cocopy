//! Target code generation.
mod amd64;
#[allow(dead_code)]
mod llvm;

use std::{env, path::Path};

use anyhow::{bail, Result};

use crate::il::Instruction;

#[allow(dead_code)]
pub fn generate_llvm() {}

pub fn generate_native<P: AsRef<Path>>(prog: &Vec<Instruction>, target: P) -> Result<()> {
    let assembly = match env::consts::OS {
        "linux" => amd64::linux::compile(prog),
        "windows" => amd64::windows::compile(prog),
        other => bail!("Target operating system '{}' not supported", other),
    };

    std::fs::write(target, assembly.to_string())?;
    Ok(())
}

//! Target code generation.
mod amd64;
#[allow(dead_code)]
mod llvm;

use std::{
    env,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{bail, Result};

use crate::il::Instruction;

#[allow(dead_code)]
pub fn generate_llvm() {}

pub fn generate_native<P: AsRef<Path>>(prog: &Vec<Instruction>, out_dir: P) -> Result<()> {
    let out_dir = PathBuf::from(out_dir.as_ref());

    clean_artifacts_dir(&out_dir)?;

    let mut asm_path = out_dir.clone();
    asm_path.push("out.asm");

    generate_assembly(prog, &asm_path)?;
    assemble(&asm_path)?;

    Ok(())
}

fn generate_assembly<P: AsRef<Path>>(prog: &Vec<Instruction>, asm_path: P) -> Result<()> {
    let assembly = match env::consts::OS {
        "linux" => amd64::linux::compile(prog),
        "windows" => amd64::windows::compile(prog),
        other => bail!("Target operating system '{}' not supported", other),
    };
    std::fs::write(&asm_path, assembly.to_string())?;
    Ok(())
}

fn assemble<P: AsRef<Path>>(assembly: P) -> Result<()> {
    let output = Command::new("./lib/nasm-2.15.05/nasm.exe")
        .args(["-f", "win64", assembly.as_ref().to_str().unwrap()])
        .output()?;

    println!("{}", String::from_utf8(output.stdout)?);

    Ok(())
}

fn clean_artifacts_dir<P: AsRef<Path>>(dir: P) -> Result<()> {
    std::fs::remove_dir_all(&dir).ok();
    std::fs::create_dir(dir)?;
    Ok(())
}

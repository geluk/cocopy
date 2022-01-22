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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Os {
    Windows,
    Linux,
}

#[allow(dead_code)]
pub fn generate_llvm() {}

pub fn generate_native<P: AsRef<Path>>(prog: &Vec<Instruction>, out_dir: P) -> Result<()> {
    let out_dir = PathBuf::from(out_dir.as_ref());

    let os = determine_os()?;

    clean_artifacts_dir(&out_dir)?;

    let mut asm_path = out_dir.clone();
    asm_path.push("out.asm");

    generate_assembly(prog, &asm_path, os)?;
    assemble(&asm_path, os)?;

    Ok(())
}

fn determine_os() -> Result<Os> {
    Ok(match env::consts::OS {
        "linux" => Os::Linux,
        "windows" => Os::Windows,
        other => bail!("Target operating system '{}' not supported", other),
    })
}

fn generate_assembly<P: AsRef<Path>>(prog: &Vec<Instruction>, asm_path: P, os: Os) -> Result<()> {
    let assembly = match os {
        Os::Linux => amd64::linux::compile(prog),
        Os::Windows => amd64::windows::compile(prog),
    };
    std::fs::write(&asm_path, assembly.to_string())?;
    Ok(())
}

fn assemble<P: AsRef<Path>>(assembly: P, os: Os) -> Result<()> {
    let asm_name = assembly.as_ref().to_str().unwrap();

    let format = match os {
        Os::Windows => "win64",
        Os::Linux => "elf64",
    };
    let output = Command::new("./lib/nasm-2.15.05/nasm.exe")
        .args(["-f", format, asm_name])
        .output()?;

    println!("{}", String::from_utf8(output.stdout)?);
    println!("{}", String::from_utf8(output.stderr)?);

    Ok(())
}

fn clean_artifacts_dir<P: AsRef<Path>>(dir: P) -> Result<()> {
    std::fs::remove_dir_all(&dir).ok();
    std::fs::create_dir(dir)?;
    Ok(())
}

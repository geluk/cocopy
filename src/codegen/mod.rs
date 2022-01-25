//! Target code generation.
mod amd64;
mod linker;
#[allow(dead_code)]
mod llvm;

use std::{
    env,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{bail, Result};

use crate::{
    ext::{DiscardOk, TryDecode, VerifySuccess},
    il::TacListing,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Os {
    Windows,
    Linux,
}

#[allow(dead_code)]
pub fn generate_llvm() {}

pub fn generate_native<P: AsRef<Path>>(prog: TacListing, out_dir: P) -> Result<()> {
    let out_dir = PathBuf::from(out_dir.as_ref());

    let os = determine_os()?;

    clean_artifacts_dir(&out_dir)?;

    let mut asm_path = out_dir.clone();
    asm_path.push("out.asm");
    let mut obj_path = out_dir.clone();
    obj_path.push("out.obj");
    let mut exe_path = out_dir;
    exe_path.push("out.exe");

    generate_assembly(prog, &asm_path, os)?;
    assemble(&asm_path, &obj_path, os)?;

    link(&obj_path, &exe_path, os)?;

    Ok(())
}

fn determine_os() -> Result<Os> {
    Ok(match env::consts::OS {
        "linux" => Os::Linux,
        "windows" => Os::Windows,
        other => bail!("Target operating system '{}' not supported", other),
    })
}

fn clean_artifacts_dir<P: AsRef<Path>>(dir: P) -> Result<()> {
    std::fs::remove_dir_all(&dir).ok();
    std::fs::create_dir(dir)?;
    Ok(())
}

fn generate_assembly<P: AsRef<Path>>(prog: TacListing, asm_path: P, os: Os) -> Result<()> {
    let assembly = match os {
        Os::Linux => amd64::linux::compile(prog),
        Os::Windows => amd64::windows::compile(prog),
    };
    std::fs::write(&asm_path, assembly.to_string())?;
    Ok(())
}

fn assemble<P: AsRef<Path>>(asm_path: P, obj_path: P, os: Os) -> Result<()> {
    let asm_path = asm_path.try_decode()?;
    let obj_path = obj_path.try_decode()?;

    let format = match os {
        Os::Windows => "win64",
        Os::Linux => "elf64",
    };
    let output = Command::new("./lib/nasm-2.15.05/nasm.exe")
        .args(["-f", format, "-o", obj_path, asm_path])
        .output()?;

    output
        .verify_success("Failed to assemble the program")
        .discard_ok()
}

fn link<P: AsRef<Path>>(assembly: P, executable: P, os: Os) -> Result<()> {
    linker::link_object(os, assembly, executable)
}

//! Target code generation.
mod amd64;
mod linker;
mod register_allocation;

use std::{
    env,
    path::{Path, PathBuf},
    process::Command,
};

use crate::{
    ext::{TryDecode, VerifySuccess},
    il::TacProgram,
    prelude::*,
};

/// A target operating system for compilation.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Os {
    Windows,
    Linux,
}

#[allow(dead_code)]
pub fn generate_llvm() {}

/// Generate native code for the current platform. Assembly code is written to `out_dir`,
/// which is then assembled and linked into an executable file.
pub fn generate_native<P: AsRef<Path>>(prog: TacProgram, out_dir: P) -> Result<PathBuf> {
    let out_dir = PathBuf::from(out_dir.as_ref());

    let os = current_os()?;

    clean_artifacts_dir(&out_dir).context("Could not clean artifacts directory")?;

    let mut asm_path = out_dir.clone();
    asm_path.push("out.asm");

    generate_assembly(prog, &asm_path, os).context("Failed to generate assembly")?;

    debug!("Assembling program");
    let obj_path =
        assemble_internal(&asm_path, &out_dir, os).context("Failed to assemble program")?;

    debug!("Linking program");
    link(obj_path, &out_dir, os)
}

/// Reassemble and link an already generated `.asm` file.
pub fn reassemble<P: AsRef<Path>>(out_dir: P) -> Result<PathBuf> {
    let out_dir = PathBuf::from(out_dir.as_ref());

    let os = current_os()?;

    let mut asm_path = out_dir.clone();
    asm_path.push("out.asm");

    let obj_path =
        assemble_internal(&asm_path, &out_dir, os).context("Failed to assemble program")?;

    link(obj_path, &out_dir, os)
}

/// Selects the current operating system as target operating system.
fn current_os() -> Result<Os> {
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

fn generate_assembly<P: AsRef<Path>>(prog: TacProgram, asm_path: P, os: Os) -> Result<()> {
    let assembly = match os {
        Os::Linux => amd64::linux::compile(prog),
        Os::Windows => amd64::windows::compile(prog),
    };
    std::fs::write(&asm_path, assembly.to_string())?;
    Ok(())
}

fn assemble_internal<P: AsRef<Path>>(asm_path: P, out_dir: P, os: Os) -> Result<PathBuf> {
    let asm_path = asm_path.try_decode()?;

    let (format, nasm_path, obj_name) = match os {
        Os::Windows => ("win64", "./lib/nasm-2.15.05/nasm.exe", "out.obj"),
        Os::Linux => ("elf64", "nasm", "out.o"),
    };

    let mut obj_path = out_dir.as_ref().to_owned();
    obj_path.push(obj_name);

    let output = Command::new(nasm_path)
        .args(["-f", format, "-o", obj_path.try_decode()?, asm_path])
        .output()
        .context("Could not execute nasm")?;

    output.verify_success()?;

    Ok(obj_path)
}

fn link<O1: AsRef<Path>, O2: AsRef<Path>>(object_path: O1, out_dir: O2, os: Os) -> Result<PathBuf> {
    linker::link_object(os, object_path, out_dir)
}

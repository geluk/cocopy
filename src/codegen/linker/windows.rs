use std::{
    collections::HashMap,
    fs::{self},
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{anyhow, Context, Result};

use crate::ext::{DiscardOk, TryDecode, VerifySuccess};

use super::Linker;

const VISUAL_STUDIO_DIR: &str = r#"C:\Program files\Microsoft Visual Studio\"#;

pub struct WindowsLinker {}
impl Linker for WindowsLinker {
    /// Link an assembled object into an executable file.
    fn link_object<O: AsRef<Path>, E: AsRef<Path>>(
        object_path: O,
        executable_path: E,
    ) -> Result<()> {
        let vc = find_visual_studio_dir().context("Could not find Visual Studio directory")?;
        let env =
            prepare_linker_environment(&vc).context("Could not prepare linker environment")?;

        let link_exe = find_link_exe(&vc)?;

        let exe = format!(r#"/out:{}"#, executable_path.try_decode()?);
        let mut command = Command::new(link_exe);
        command
            .args([
                object_path.try_decode()?,
                "/subsystem:console",
                // This only results in the generation of debug symbols in a separate PDB, so we can
                // always leave this on.
                "/debug",
                &exe,
                "legacy_stdio_definitions.lib",
                "msvcrt.lib",
            ])
            .envs(&env);

        command
            .output()
            .map_err(Into::into)
            .and_then(VerifySuccess::verify_success)
            .discard_ok()
            .context("Running the linker failed")
    }
}

/// Find the Visual Studio installation directory.
fn find_visual_studio_dir() -> Result<PathBuf> {
    let newest = find_newest_subdir(VISUAL_STUDIO_DIR).context(format!(
        "Could not find Visual Studio version (looked in '{}')",
        VISUAL_STUDIO_DIR
    ))?;

    let mut edition = fs::read_dir(&newest)?
        .next()
        .ok_or_else(|| {
            anyhow!(
                "No Visual Studio edition directory found at '{}'",
                newest.display()
            )
        })??
        .path();

    edition.push("VC");

    Ok(edition)
}

/// Sets up the required environment variables in order for `link.exe`
/// to work correctly.
fn prepare_linker_environment(visual_studio_vc: &Path) -> Result<HashMap<String, String>> {
    let build_dir = visual_studio_vc.join(PathBuf::from(r#"Auxiliary\Build"#));
    let output = Command::new("cmd")
        .args(["/c", "vcvars64.bat&set"])
        .current_dir(build_dir)
        .env("__VSCMD_ARG_NO_LOGO", "1")
        .output()?;

    let (stdout, stderr) = output
        .verify_success()
        .context("Failed to prepare linker environment")?;

    if !stderr.is_empty() {
        return Err(anyhow!("{}", stderr).context("Executing vcvars64 failed"));
    }

    let mut env = HashMap::new();

    for line in stdout.lines().filter(|l| l.contains('=')) {
        let (key, value) = line.split_once('=').unwrap();
        env.insert(key.to_string(), value.to_string());
    }
    Ok(env)
}

/// Find the location of `link.exe` in a Visual Studio installation directory.
fn find_link_exe(vs_vc: &Path) -> Result<PathBuf> {
    let msvc = vs_vc.join(Path::new(r#"Tools\MSVC"#));
    // Look for the latest installed Visual C++ version.
    let mut version = find_newest_subdir(&msvc).context(format!(
        "Could not find MSVC version (looked in '{}')",
        msvc.display()
    ))?;
    version.extend(["bin", "HostX64", "x64", "link.exe"].iter());
    Ok(version)
}

fn find_newest_subdir<P: AsRef<Path>>(path: P) -> Result<PathBuf> {
    let vs_dir = fs::read_dir(path.as_ref()).context(format!(
        "Could not find target directory '{}'",
        path.as_ref().display()
    ))?;
    let mut entries = vs_dir
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
        .context(format!(
            "Could not discover subdirectories of '{}'",
            path.as_ref().display()
        ))?;
    entries.sort_by_key(|ent| ent.file_name());

    entries
        .into_iter()
        .last()
        .map(|ent| ent.path())
        .ok_or_else(|| anyhow!("Path '{}' has no subdirectories", path.as_ref().display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test() {
        let dir = find_visual_studio_dir().unwrap();

        println!("{:#?}", dir);
    }
}

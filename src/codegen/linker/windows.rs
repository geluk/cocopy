use std::{
    path::Path,
    process::{Command, Output},
};

use anyhow::{anyhow, bail, Result};

use crate::ext::{DiscardOk, TryDecode};

use super::Linker;

const VISUAL_STUDIO_VC: &'static str =
    r#"C:\Program Files\Microsoft Visual Studio\2022\Community\VC"#;

pub struct WindowsLinker {}
impl WindowsLinker {
    fn prepare_environment() -> Result<String> {
        let output = Command::new("cmd")
            .args(["/c", r#"\Auxiliary\Build\vcvars64.bat&set"#])
            .current_dir(VISUAL_STUDIO_VC)
            .env("__VSCMD_ARG_NO_LOGO", "1")
            .output()?;

        let (stdout, _) = output.verify_success("Failed to prepare linker environment")?;

        for line in stdout.lines().filter(|l| l.contains("=")) {
            let (key, value) = line.split_once("=").unwrap();
            if key == "PATH" {
                return Ok(value.to_string());
            }
        }
        bail!("PATH not set when preparing linker environment");
    }
}

impl Linker for WindowsLinker {
    fn link_object<P: AsRef<Path>>(object_path: P, executable_path: P) -> Result<()> {
        let path = Self::prepare_environment()?;

        let exe = format!(r#"/out:{}"#, executable_path.try_decode()?);
        let output = Command::new("link.exe")
            .args([
                object_path.try_decode()?,
                "/subsystem:console",
                &exe,
                "legacy_stdio_definitions.lib",
                "msvcrt.lib",
            ])
            .env("PATH", path)
            .output()?;

        output
            .verify_success("Failed to execute linker")
            .discard_ok()
    }
}

trait VerifySuccess {
    fn verify_success(self, error_msg: &'static str) -> Result<(String, String)>;
}
impl VerifySuccess for Output {
    fn verify_success(self, error_msg: &'static str) -> Result<(String, String)> {
        let stdout = String::from_utf8(self.stdout)?;
        let stderr = String::from_utf8(self.stderr)?;

        if self.status.success() {
            Ok((stdout, stderr))
        } else {
            eprintln!("STDOUT:\n======\n{}", stdout);
            eprintln!("STDERR:\n======\n{}", stderr);
            Err(anyhow!("Command exited with a non-zeor status code").context(error_msg))
        }
    }
}

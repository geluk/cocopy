use std::{path::Path, process::Command};

use crate::{
    ext::{DiscardOk, TryDecode, VerifySuccess},
    prelude::*,
};

use super::Linker;

pub struct LinuxLinker {}
impl Linker for LinuxLinker {
    /// Link an assembled object into an executable file.
    fn link_object<O: AsRef<Path>, E: AsRef<Path>>(
        object_path: O,
        executable_path: E,
    ) -> Result<()> {
        Command::new("gcc")
            .args([
                "-static",
                object_path.try_decode()?,
                "-o",
                executable_path.try_decode()?,
            ])
            .output()
            .map_err(Into::into)
            .and_then(VerifySuccess::verify_success)
            .discard_ok()
            .context("Running the linker failed")
    }
}

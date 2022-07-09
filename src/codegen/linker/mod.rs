//! Functionality for interacting with linkers.
mod linux;
mod windows;

use std::path::Path;

use crate::prelude::*;

use self::{linux::LinuxLinker, windows::WindowsLinker};

use super::Os;

trait Linker {
    /// Link an assembled object into an executable file.
    fn link_object<O: AsRef<Path>, E: AsRef<Path>>(
        object_path: O,
        executable_path: E,
    ) -> Result<()>;
}

pub fn link_object<O1: AsRef<Path>, O2: AsRef<Path>>(
    os: Os,
    object_path: O1,
    out_dir: O2,
) -> Result<()> {
    let mut executable_path = out_dir.as_ref().to_owned();

    match os {
        Os::Windows => {
            executable_path.push("out.exe");
            WindowsLinker::link_object(object_path, &executable_path)
        }
        Os::Linux => {
            executable_path.push("out");
            LinuxLinker::link_object(object_path, executable_path)
        }
    }
}

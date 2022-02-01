//! Functionality for interacting with linkers.
mod windows;

use std::path::Path;

use anyhow::Result;

use self::windows::WindowsLinker;

use super::Os;

trait Linker {
    fn link_object<P: AsRef<Path>>(object_path: P, executable_path: P) -> Result<()>;
}

pub fn link_object<P: AsRef<Path>>(os: Os, object_path: P, executable_path: P) -> Result<()> {
    match os {
        Os::Windows => WindowsLinker::link_object(object_path, executable_path),
        Os::Linux => todo!(),
    }
}

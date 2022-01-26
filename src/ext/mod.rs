use std::{path::Path, process::Output};

use anyhow::{anyhow, Result};

pub trait TryDecode {
    fn try_decode(&self) -> Result<&str>;
}
impl<T: AsRef<Path>> TryDecode for T {
    fn try_decode(&self) -> Result<&str> {
        self.as_ref()
            .to_str()
            .ok_or_else(|| anyhow!("Path contains invalid unicode"))
    }
}

pub trait DiscardOk {
    type Res;
    fn discard_ok(self) -> Self::Res;
}
impl<T, E> DiscardOk for std::result::Result<T, E> {
    type Res = std::result::Result<(), E>;

    fn discard_ok(self) -> Self::Res {
        self.map(|_| ())
    }
}
pub trait VerifySuccess {
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
            Err(anyhow!("Command exited with a non-zero status code").context(error_msg))
        }
    }
}

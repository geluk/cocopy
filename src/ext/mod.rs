use std::{path::Path, process::Output};

use crate::prelude::*;

pub mod ordered_hash_map;

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
    fn verify_success(self) -> Result<(String, String)>;
}
impl VerifySuccess for Output {
    fn verify_success(self) -> Result<(String, String)> {
        let stdout = String::from_utf8(self.stdout)?;
        let stderr = String::from_utf8(self.stderr)?;

        if self.status.success() {
            Ok((stdout, stderr))
        } else {
            eprintln!("STDOUT:\n======\n{}", stdout);
            eprintln!("STDERR:\n======\n{}", stderr);
            Err(anyhow!("Command exited with a non-zero status code"))
        }
    }
}

pub trait Sequence {
    type Other;
    fn sequence(self) -> Self::Other;
}

impl<S, E> Sequence for Option<Result<S, E>> {
    type Other = Result<Option<S>, E>;
    fn sequence(self) -> Self::Other {
        match self {
            Some(res) => res.map(|s| Some(s)),
            None => Ok(None),
        }
    }
}

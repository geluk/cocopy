use std::path::Path;

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

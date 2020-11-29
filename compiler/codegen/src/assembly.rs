use std::path::Path;
use tempfile::NamedTempFile;

#[derive(Debug)]
pub struct Assembly {
    file: NamedTempFile,
}

impl PartialEq for Assembly {
    fn eq(&self, other: &Self) -> bool {
        self.path().eq(other.path())
    }
}

impl Eq for Assembly {}

impl Assembly {
    pub const EXTENSION: &'static str = "shadelib";

    pub fn path(&self) -> &Path {
        self.file.path()
    }

    pub fn copy_to<P: AsRef<Path>>(&self, dest: P) -> std::io::Result<()> {
        std::fs::copy(self.path(), dest).map(|_| ())
    }
}

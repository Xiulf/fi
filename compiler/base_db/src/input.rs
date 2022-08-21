use std::path::PathBuf;

use relative_path::{RelativePath, RelativePathBuf};
use rustc_hash::FxHashMap;
use syntax::TextSize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceRootId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRoot {
    pub is_lib: bool,
    pub dir: Option<PathBuf>,
    files: FxHashMap<FileId, RelativePathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    newlines: Vec<TextSize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LineCol {
    pub line: u32,
    pub col: u32,
}

impl SourceRoot {
    pub fn new_local(dir: Option<PathBuf>) -> Self {
        SourceRoot {
            is_lib: false,
            dir,
            files: FxHashMap::default(),
        }
    }

    pub fn new_library(dir: Option<PathBuf>) -> Self {
        SourceRoot {
            is_lib: true,
            dir,
            files: FxHashMap::default(),
        }
    }

    pub fn insert_file(&mut self, file_id: FileId, path: impl AsRef<RelativePath>) {
        self.files.insert(file_id, path.as_ref().to_relative_path_buf());
    }

    pub fn remove_file(&mut self, file_id: FileId) -> bool {
        self.files.remove(&file_id).is_some()
    }

    pub fn relative_path(&self, file_id: FileId) -> &RelativePath {
        &self.files[&file_id]
    }

    pub fn files(&self) -> impl Iterator<Item = FileId> + '_ {
        self.files.keys().copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &RelativePath)> + '_ {
        self.files.iter().map(|(k, v)| (*k, v.as_relative_path()))
    }
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut newlines = vec![TextSize::from(0)];
        let mut curr_row = TextSize::from(0);

        for c in text.chars() {
            curr_row += TextSize::of(c);

            if c == '\n' {
                newlines.push(curr_row);
            }
        }

        LineIndex { newlines }
    }

    pub fn line_col(&self, offset: TextSize) -> LineCol {
        let line = self
            .newlines
            .iter()
            .position(|l| l > &offset)
            .unwrap_or(self.newlines.len())
            - 1;

        let line_start_offset = self.newlines[line];
        let col = offset - line_start_offset;

        LineCol {
            line: line as u32,
            col: col.into(),
        }
    }

    pub fn offset(&self, line_col: LineCol) -> TextSize {
        // @TODO: modify this to account for characters larger than 1 byte
        self.newlines[line_col.line as usize] + TextSize::from(line_col.col)
    }

    pub fn line_offset(&self, line: u32) -> usize {
        self.newlines[line as usize].into()
    }
}

impl std::fmt::Display for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "file{}", self.0)
    }
}

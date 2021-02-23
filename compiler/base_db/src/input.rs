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
    pub fn new_local() -> Self {
        SourceRoot {
            is_lib: false,
            files: FxHashMap::default(),
        }
    }

    pub fn new_library() -> Self {
        SourceRoot {
            is_lib: true,
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
        let line = self.newlines.iter().position(|l| l > &offset).unwrap() - 1;
        let line_start_offset = self.newlines[line];
        let col = offset - line_start_offset;

        LineCol {
            line: line as u32,
            col: col.into(),
        }
    }

    pub fn line_offset(&self, line: u32) -> usize {
        self.newlines[line as usize].into()
    }
}

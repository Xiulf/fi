use syntax::TextSize;
use vfs::file_set::FileSet;
pub use vfs::FileId;
use vfs::VfsPath;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceRootId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRoot {
    file_set: FileSet,
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
    pub fn new(file_set: FileSet) -> Self {
        Self { file_set }
    }

    pub fn path_for_file(&self, file: FileId) -> Option<&VfsPath> {
        self.file_set.path_for_file(file)
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    pub fn iter(&self) -> impl Iterator<Item = FileId> + '_ {
        self.file_set.iter()
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

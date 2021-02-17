use base_db::input::FileId;
use base_db::SourceDatabase;
use syntax::syntax_node::SyntaxNode;

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub const fn new(file_id: FileId, value: T) -> Self {
        InFile { file_id, value }
    }

    pub fn with_value<U>(&self, value: U) -> InFile<U> {
        InFile::new(self.file_id, value)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> InFile<U> {
        InFile::new(self.file_id, f(self.value))
    }

    pub fn as_ref(&self) -> InFile<&T> {
        self.with_value(&self.value)
    }

    pub fn file_syntax(&self, db: &dyn SourceDatabase) -> SyntaxNode {
        db.parse(self.file_id).syntax_node()
    }
}

impl<T: Clone> InFile<&T> {
    pub fn cloned(&self) -> InFile<T> {
        self.with_value(self.value.clone())
    }
}

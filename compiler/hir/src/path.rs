use syntax::ast;

use crate::name::{AsName, Name};
use crate::Db;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    segments: Vec<Name>,
}

impl Path {
    pub fn from_ast(db: &dyn Db, ast: ast::Path) -> Self {
        ast.segments().map(|s| s.as_name(db)).collect()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = Name> + '_ {
        self.segments.iter().copied()
    }

    pub fn as_name(&self) -> Option<Name> {
        if self.segments.len() == 1 {
            Some(self.segments[0])
        } else {
            None
        }
    }

    pub fn push(&mut self, name: Name) {
        self.segments.push(name);
    }
}

impl FromIterator<Name> for Path {
    fn from_iter<T: IntoIterator<Item = Name>>(iter: T) -> Self {
        Self {
            segments: Vec::from_iter(iter),
        }
    }
}

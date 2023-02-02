use syntax::ast;

use crate::name::{AsName, Name};
use crate::Db;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    segments: Vec<Name>,
}

pub struct Display<'a>(&'a dyn Db, &'a Path);

impl Path {
    pub fn from_ast(db: &dyn Db, ast: ast::Path) -> Self {
        ast.segments()
            .filter_map(|s| s.name_ref())
            .map(|n| n.as_name(db))
            .collect()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn display<'a>(&'a self, db: &'a dyn Db) -> Display<'a> {
        Display(db, self)
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

    pub fn last(&self) -> Name {
        *self.segments.last().unwrap()
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

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, name) in self.1.segments.iter().enumerate() {
            if i != 0 {
                '.'.fmt(f)?;
            }

            name.display(self.0).fmt(f)?;
        }

        Ok(())
    }
}

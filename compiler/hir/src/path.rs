use crate::in_file::InFile;
use crate::name::{AsName, Name};
use std::fmt;
use std::iter::FromIterator;
use syntax::ast;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModPath {
    segments: Vec<Name>,
}

impl ModPath {
    pub fn from_segments(segments: impl IntoIterator<Item = Name>) -> Self {
        ModPath {
            segments: Vec::from_iter(segments),
        }
    }

    pub fn segments(&self) -> &[Name] {
        &self.segments
    }

    pub fn push_segment(&mut self, segment: Name) {
        self.segments.push(segment);
    }

    pub fn pop_segment(&mut self) -> Option<Name> {
        self.segments.pop()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn is_ident(&self) -> bool {
        self.segments.len() == 1
    }

    pub fn as_ident(&self) -> Option<&Name> {
        if let [name] = &*self.segments {
            Some(name)
        } else {
            None
        }
    }

    pub(crate) fn expand_import(
        import: InFile<ast::ItemImport>,
        mut cb: impl FnMut(ModPath, Option<ast::NameRef>, bool, Option<Name>, Option<Name>),
    ) {
        if let Some(path) = convert_path(import.value.path()) {
            if let Some(items) = import.value.items() {
                for item in items {
                    let mut path = path.clone();

                    path.push_segment(item.as_name());

                    cb(
                        path,
                        Some(item),
                        false,
                        None,
                        import.value.qualify().map(|n| n.as_name()),
                    );
                }
            } else {
                cb(path, None, true, None, import.value.qualify().map(|n| n.as_name()));
            }
        }
    }
}

pub(crate) fn convert_path(path: Option<ast::Path>) -> Option<ModPath> {
    let path = path?;
    let segments = path.segments().filter_map(|s| Some(s.name_ref()?.as_name())).collect();

    Some(ModPath { segments })
}

impl fmt::Display for ModPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, segment) in self.segments.iter().enumerate() {
            if i != 0 {
                write!(f, ".")?;
            }

            segment.fmt(f)?;
        }

        Ok(())
    }
}

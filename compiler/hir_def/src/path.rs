use crate::in_file::InFile;
use crate::name::{AsName, Name};
use std::fmt;
use std::iter::FromIterator;
use syntax::ast;

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path {
    segments: Vec<Name>,
}

impl Path {
    pub fn from_segments(segments: impl IntoIterator<Item = Name>) -> Self {
        Path {
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

    pub fn to_instance(&mut self, name: Name) {
        self.segments.insert(self.segments.len() - 1, name);
    }

    pub(crate) fn expand_import(
        import: InFile<ast::ItemImport>,
        mut cb: impl FnMut(Path, Option<ast::NameRef>, bool, Option<Name>, Option<Name>),
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

    pub fn lower(path: ast::Path) -> Self {
        let segments = path.segments().filter_map(|s| Some(s.name_ref()?.as_name())).collect();

        Path { segments }
    }
}

pub(crate) fn convert_path(path: Option<ast::Path>) -> Option<Path> {
    path.map(Path::lower)
}

impl From<Name> for Path {
    fn from(name: Name) -> Self {
        Path { segments: vec![name] }
    }
}

impl std::iter::FromIterator<Name> for Path {
    fn from_iter<T: IntoIterator<Item = Name>>(iter: T) -> Self {
        Self::from_segments(iter)
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, segment) in self.segments.iter().enumerate() {
            if i != 0 {
                write!(f, "/")?;
            }

            segment.fmt(f)?;
        }

        Ok(())
    }
}

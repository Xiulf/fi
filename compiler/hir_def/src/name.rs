use smol_str::SmolStr;
use std::fmt;
use syntax::ast;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Repr);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Repr {
    Text(SmolStr),
    TupleField(usize),
}

impl Default for Name {
    fn default() -> Self {
        Name(Repr::Text(SmolStr::default()))
    }
}

impl Name {
    const fn new_text(text: SmolStr) -> Self {
        Name(Repr::Text(text))
    }

    pub fn new_tuple_field(idx: usize) -> Self {
        Name(Repr::TupleField(idx))
    }

    const fn new_inline(text: &str) -> Self {
        Name(Repr::Text(SmolStr::new_inline(text)))
    }

    pub fn missing() -> Self {
        Self::new_text("[missing name]".into())
    }

    pub fn as_tuple_index(&self) -> Option<usize> {
        match self.0 {
            | Repr::TupleField(idx) => Some(idx),
            | _ => None,
        }
    }
}

pub trait AsName {
    fn as_name(&self) -> Name;
}

impl AsName for str {
    fn as_name(&self) -> Name {
        Name::new_text(self.into())
    }
}

impl AsName for ast::Name {
    fn as_name(&self) -> Name {
        Name::new_text(self.text().into())
    }
}

impl AsName for ast::NameRef {
    fn as_name(&self) -> Name {
        match self.as_tuple_field() {
            | Some(idx) => Name::new_tuple_field(idx),
            | None => Name::new_text(self.text().into()),
        }
    }
}

impl AsName for ast::Operator {
    fn as_name(&self) -> Name {
        Name::new_text(format!("({})", self.text()).into())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            | Repr::Text(text) => text.fmt(f),
            | Repr::TupleField(idx) => idx.fmt(f),
        }
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        match self.0 {
            | Repr::Text(ref t) => t.eq(other),
            | Repr::TupleField(_) => false,
        }
    }
}

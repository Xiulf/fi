use smol_str::SmolStr;

use crate::Db;

pub trait AsName {
    fn as_name(&self, db: &dyn Db) -> Name;
}

#[salsa::interned]
pub struct Name {
    #[return_ref]
    repr: Repr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Repr {
    Text(SmolStr),
}

impl Name {
    pub fn new_text(db: &dyn Db, text: SmolStr) -> Self {
        Self::new(db, Repr::Text(text))
    }

    pub fn missing(db: &dyn Db) -> Self {
        Self::new_text(db, SmolStr::new_inline("[missing]"))
    }

    pub fn is_lowercase(self, db: &dyn Db) -> bool {
        match self.repr(db) {
            | Repr::Text(text) => text.chars().all(char::is_lowercase),
        }
    }
}

impl AsName for str {
    fn as_name(&self, db: &dyn Db) -> Name {
        Name::new_text(db, SmolStr::new(self))
    }
}

impl AsName for syntax::ast::PathSegment {
    fn as_name(&self, db: &dyn Db) -> Name {
        let resolver = db.syntax_interner().read().unwrap();
        Name::new_text(db, SmolStr::new(self.text(&*resolver)))
    }
}

impl AsName for syntax::ast::Name {
    fn as_name(&self, db: &dyn Db) -> Name {
        let resolver = db.syntax_interner().read().unwrap();
        Name::new_text(db, SmolStr::new(self.text(&*resolver)))
    }
}

impl AsName for syntax::ast::NameRef {
    fn as_name(&self, db: &dyn Db) -> Name {
        let resolver = db.syntax_interner().read().unwrap();
        Name::new_text(db, SmolStr::new(self.text(&*resolver)))
    }
}

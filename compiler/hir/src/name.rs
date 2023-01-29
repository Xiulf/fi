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
    Symbol(SmolStr),
}

pub struct Display<'db>(&'db dyn Db, Name);

impl Name {
    pub fn new_text(db: &dyn Db, text: SmolStr) -> Self {
        Self::new(db, Repr::Text(text))
    }

    pub fn new_symbol(db: &dyn Db, text: SmolStr) -> Self {
        Self::new(db, Repr::Symbol(text))
    }

    pub fn missing(db: &dyn Db) -> Self {
        Self::new_text(db, SmolStr::new_inline("[missing]"))
    }

    pub fn is_lowercase(self, db: &dyn Db) -> bool {
        match self.repr(db) {
            | Repr::Text(text) => text.chars().all(char::is_lowercase),
            | Repr::Symbol(_) => false,
        }
    }

    pub fn display(self, db: &dyn Db) -> Display {
        Display(db, self)
    }
}

impl AsName for str {
    fn as_name(&self, db: &dyn Db) -> Name {
        Name::new_text(db, SmolStr::new(self))
    }
}

impl AsName for syntax::ast::Name {
    fn as_name(&self, db: &dyn Db) -> Name {
        let resolver = db.syntax_interner().read().unwrap();

        if let Some(ident) = self.ident_token() {
            return Name::new_text(db, SmolStr::new(ident.resolve_text(&*resolver)));
        }

        if let Some(symbol) = self.symbol_token() {
            return Name::new_symbol(db, SmolStr::new(symbol.resolve_text(&*resolver)));
        }

        Name::missing(db)
    }
}

impl AsName for syntax::ast::NameRef {
    fn as_name(&self, db: &dyn Db) -> Name {
        let resolver = db.syntax_interner().read().unwrap();

        if let Some(ident) = self.ident_token() {
            return Name::new_text(db, SmolStr::new(ident.resolve_text(&*resolver)));
        }

        if let Some(symbol) = self.symbol_token() {
            return Name::new_symbol(db, SmolStr::new(symbol.resolve_text(&*resolver)));
        }

        Name::missing(db)
    }
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1.repr(self.0) {
            | Repr::Text(t) => t.fmt(f),
            | Repr::Symbol(t) => write!(f, "({t})"),
        }
    }
}

impl ra_ap_stdx::hash::NoHashHashable for Name {
}

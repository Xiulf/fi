use std::sync::RwLock;

#[salsa::db(vfs::Jar, ::diagnostics::Jar, base_db::Jar, hir::Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,
    syntax_interner: RwLock<syntax::Interner>,
    libs: base_db::libs::LibSet,
}

impl Default for Database {
    fn default() -> Self {
        Self {
            storage: Default::default(),
            syntax_interner: RwLock::new(syntax::new_interner()),
            libs: Default::default(),
        }
    }
}

impl salsa::Database for Database {
}

impl base_db::Db for Database {
    fn syntax_interner(&self) -> &std::sync::RwLock<syntax::Interner> {
        &self.syntax_interner
    }

    fn libs(&self) -> &base_db::libs::LibSet {
        &self.libs
    }
}

#![feature(trait_upcasting)]

pub mod input;
pub mod libs;

use std::sync::RwLock;

use libs::LibSet;
use vfs::File;

pub trait Db: vfs::Db + salsa::DbWithJar<Jar> {
    fn syntax_interner(&self) -> &RwLock<syntax::Interner>;
    fn libs(&self) -> &LibSet;
}

#[salsa::jar(db = Db)]
pub struct Jar(input::SourceRoot, libs::LibId);

pub fn parse(db: &dyn Db, file: File) -> syntax::Parsed<syntax::ast::SourceFile> {
    let text = file.text(db).as_deref().unwrap_or_default();
    let mut interner = db.syntax_interner().write().unwrap();

    syntax::ast::SourceFile::parse(text, &mut interner)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[salsa::db(vfs::Jar, crate::Jar)]
    struct Database {
        storage: salsa::Storage<Self>,
        syntax_interner: RwLock<syntax::Interner>,
        libs: LibSet,
    }

    impl Database {
        fn new() -> Self {
            Self {
                storage: salsa::Storage::default(),
                syntax_interner: RwLock::new(syntax::new_interner()),
                libs: LibSet::default(),
            }
        }
    }

    impl salsa::Database for Database {
    }

    impl crate::Db for Database {
        fn syntax_interner(&self) -> &RwLock<syntax::Interner> {
            &self.syntax_interner
        }

        fn libs(&self) -> &LibSet {
            &self.libs
        }
    }

    #[test]
    fn test_parse() {
        let input = r#"
            module Core.Cmp =

            main = 0

            type X =
                | Y
                | Z

            trait Iterator self it =
                next :: self -> Option it

            impl Iterator Iter Item =
                next self = _
        "#;
        let input = unindent::unindent(input.trim());
        let db = Database::new();
        let file = File::new(&db, vfs::VfsPath::new_virtual("/test.fi".into()), Some(input.into()));
        let parsed = parse(&db, file);
        let interner = db.syntax_interner().read().unwrap();
        let node = parsed.syntax_node();
        let node = node.as_serialize_with_resolver(&*interner);
        let errors = parsed.errors();

        insta::assert_ron_snapshot!((node, errors), {
            "[1].$expected" => insta::sorted_redaction(),
        });
    }
}

#![feature(trait_upcasting)]

pub mod input;
pub mod libs;

use libs::LibSet;
use parking_lot::RwLock;
use vfs::File;

pub trait Db: vfs::Db + diagnostics::Db + salsa::DbWithJar<Jar> {
    fn syntax_interner(&self) -> &RwLock<syntax::Interner>;
    fn libs(&self) -> &LibSet;
}

#[salsa::jar(db = Db)]
pub struct Jar(input::SourceRoot, libs::LibId, parse);

#[salsa::tracked]
pub fn parse(db: &dyn Db, file: File) -> syntax::ast::SourceFile {
    let mut interner = db.syntax_interner().write();

    syntax::ast::SourceFile::parse(db, file, &mut interner)
}

#[derive(Debug)]
pub struct ICE(pub std::borrow::Cow<'static, str>);

impl ICE {
    pub fn throw(msg: impl Into<std::borrow::Cow<'static, str>>) -> ! {
        std::panic::panic_any(Self(msg.into()))
    }
}

impl std::fmt::Display for ICE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "internal compiler error: '{}'", self.0)
    }
}

impl std::error::Error for ICE {
}

#[derive(Debug)]
pub struct Error(pub std::borrow::Cow<'static, str>);

impl Error {
    pub fn throw(msg: impl Into<std::borrow::Cow<'static, str>>) -> ! {
        std::panic::panic_any(Self(msg.into()))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error: '{}'", self.0)
    }
}

impl std::error::Error for Error {
}

pub fn setup_panic_hook(
    on_ice: impl Fn(&ICE) -> bool + Send + Sync + 'static,
    on_error: impl Fn(&Error) -> bool + Send + Sync + 'static,
) {
    std::panic::set_hook(Box::new(move |info| {
        let loc = info.location().unwrap();

        if let Some(ice) = info.payload().downcast_ref::<ICE>() {
            if on_ice(ice) {
                eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", ice.0, loc);
            }
            return;
        }

        if let Some(err) = info.payload().downcast_ref::<Error>() {
            if on_error(err) {
                eprintln!("\x1B[31mError:\x1B[0m '{}'", err.0);
            }
            return;
        }

        let msg = if let Some(s) = info.payload().downcast_ref::<salsa::Cycle>() {
            format!("{:?}", s).into()
        } else if let Some(s) = info.payload().downcast_ref::<&'static str>() {
            (*s).into()
        } else if let Some(s) = info.payload().downcast_ref::<String>() {
            s.clone().into()
        } else {
            "...".into()
        };

        let ice = ICE(msg);
        if on_ice(&ice) {
            eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", ice.0, loc);
        }
    }));
}

#[cfg(test)]
mod tests {
    use syntax::ast::AstNode;

    use super::*;

    #[salsa::db(vfs::Jar, diagnostics::Jar, crate::Jar)]
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
        let interner = db.syntax_interner().read();
        let node = parsed.syntax();
        let node = node.as_serialize_with_resolver(&*interner);

        insta::assert_ron_snapshot!(node, {
            "[1].$expected" => insta::sorted_redaction(),
        });
    }
}

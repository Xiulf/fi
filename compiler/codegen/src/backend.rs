use std::io;
use std::sync::Arc;

use hir::db::HirDatabase;
use libloading::{Library, Symbol};

use crate::db::CodegenDatabase;
use crate::CompilerTarget;

#[derive(Debug, Clone)]
pub struct Backend(Arc<BackendInner>);

#[derive(Debug)]
struct BackendInner {
    #[allow(dead_code)]
    lib: Library,
    codegen: Symbol<'static, CodegenFn>,
}

impl PartialEq for Backend {
    fn eq(&self, other: &Backend) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Backend {
}

// type InitFn = fn(&'static dyn log::Log, log::LevelFilter);
type CodegenFn = fn(&dyn HirDatabase, hir::Module, &mut dyn io::Write);

pub fn backend(db: &dyn CodegenDatabase) -> Backend {
    let lib = match db.target() {
        | CompilerTarget::Javascript if cfg!(target_os = "windows") => "shade_backend_js.dll",
        | CompilerTarget::Javascript if cfg!(target_os = "macos") => "libshade_backend_js.dylib",
        | CompilerTarget::Javascript => "libshade_backend_js.so",
    };

    let lib = std::env::current_exe().unwrap().parent().unwrap().join(lib);
    let lib = unsafe { Library::new(lib).unwrap() };
    // let init = unsafe { lib.get::<InitFn>(b"init").unwrap() };
    let codegen = unsafe { std::mem::transmute(lib.get::<CodegenFn>(b"codegen").unwrap()) };

    // init(log::logger(), log::LevelFilter::Debug);

    Backend(Arc::new(BackendInner { lib, codegen }))
}

impl Backend {
    pub fn invoke(&self, db: &dyn CodegenDatabase, module: hir::Module, file: &mut dyn io::Write) {
        (self.0.codegen)(db.upcast(), module, file);
    }
}

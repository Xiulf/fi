use std::io;
use std::sync::Arc;

use libloading::{Library, Symbol};
use tracing_subscriber::EnvFilter;

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

type InitLoggingFn = fn(EnvFilter);
type CodegenFn = fn(&dyn CodegenDatabase, hir::Module, &mut dyn io::Write);

pub fn backend(db: &dyn CodegenDatabase) -> Backend {
    let lib = match db.target() {
        | CompilerTarget::Javascript if cfg!(target_os = "windows") => "shade_backend_js.dll",
        | CompilerTarget::Javascript if cfg!(target_os = "macos") => "libshade_backend_js.dylib",
        | CompilerTarget::Javascript => "libshade_backend_js.so",
        | CompilerTarget::Native(_) if cfg!(target_os = "windows") => "shade_backend_llvm.dll",
        | CompilerTarget::Native(_) if cfg!(target_os = "macos") => "libshade_backend_llvm.dylib",
        | CompilerTarget::Native(_) => "libshade_backend_llvm.so",
    };

    let lib = std::env::current_exe().unwrap().parent().unwrap().join(lib);
    let lib = unsafe { Library::new(lib).unwrap() };
    let init_logging = unsafe { lib.get::<InitLoggingFn>(b"init_logging").unwrap() };
    let codegen = unsafe { std::mem::transmute(lib.get::<CodegenFn>(b"codegen").unwrap()) };

    init_logging(EnvFilter::default().add_directive(tracing::Level::DEBUG.into()));

    Backend(Arc::new(BackendInner { lib, codegen }))
}

impl Backend {
    pub fn invoke(&self, db: &dyn CodegenDatabase, module: hir::Module, file: &mut dyn io::Write) {
        (self.0.codegen)(db, module, file);
    }
}

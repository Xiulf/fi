#![feature(arc_unwrap_or_clone)]

mod body;
mod ctx;
mod layout;
mod place;
mod ty;
mod value;

use std::io::Write;

use codegen::db::CodegenDatabase;
use tracing_subscriber::EnvFilter;

#[no_mangle]
pub fn init_logging(filter: EnvFilter) {
    tracing_subscriber::fmt().without_time().with_env_filter(filter).init();

    std::panic::set_hook(Box::new(|info| {
        let loc = info.location().unwrap();

        // if let Some(ice) = info.payload().downcast_ref::<ICE>() {
        //     eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", ice.0, loc);
        //     return;
        // }

        // if let Some(err) = info.payload().downcast_ref::<Error>() {
        //     eprintln!("\x1B[31mError:\x1B[0m '{}'", err.0);
        //     return;
        // }

        let msg = match info.payload().downcast_ref::<&'static str>() {
            | Some(s) => *s,
            | None => match info.payload().downcast_ref::<String>() {
                | Some(s) => &s[..],
                | None => "...",
            },
        };

        eprintln!("\x1B[31mInternal Compiler Error:\x1B[0m '{}' at {}", msg, loc);
    }));
}

#[no_mangle]
pub fn codegen(db: &dyn CodegenDatabase, module: hir::Module, file: &mut dyn Write) {
    ctx::with_codegen_ctx(db, module, |mut ctx| {
        ctx.codegen();
        ctx.write(file);
    })
}

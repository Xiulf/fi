#![feature(arc_unwrap_or_clone)]

mod abi;
mod body;
mod ctx;
mod local;
mod operand;
mod place;
mod ssa;
mod ty;

use std::io::Write;

use mir::db::MirDatabase;

pub fn codegen(db: &dyn MirDatabase, module: hir::Module, file: &mut dyn Write) {
    ctx::with_codegen_ctx(db, module, |mut ctx| {
        ctx.codegen(module);
        ctx.write(file);
    })
}

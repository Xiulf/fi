use crate::FunctionCtx;
use cranelift_module::Backend;

#[derive(Debug, PartialEq)]
pub enum SsaKind {
    Ssa,
    NotSsa,
}

pub fn analyze(fx: &FunctionCtx<impl Backend>, id: mir::LocalId) -> SsaKind {
    if fx
        .clif_type(fx.tcx.layout(fx.body.locals[&id].ty))
        .is_some()
    {
        SsaKind::Ssa
    } else {
        SsaKind::NotSsa
    }
}

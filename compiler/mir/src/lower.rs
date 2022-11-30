mod expr;
mod intrinsic;
mod pat;

use std::sync::Arc;

use arena::ArenaMap;
use hir::ty::{Constraint, Ty};
use hir::PatId;

use crate::builder::Builder;
use crate::db::MirDatabase;
use crate::syntax::*;

pub(crate) fn lower_body(db: &dyn MirDatabase, def: hir::id::DefWithBodyId) -> Body {
    let body = db.body(def);
    let infer = db.infer(def);
    let origin = BodyOrigin { def, expr: None };
    let mut bcx = BodyLowerCtx {
        db,
        body: body.clone(),
        infer: infer.clone(),
        builder: Builder::new(origin),
        locals: ArenaMap::default(),
    };

    if let hir::id::DefWithBodyId::FuncId(f) = def {
        let f = hir::Func::from(f);
        tracing::debug!("lower_body({})", f.name(db.upcast()));
    }

    let entry = bcx.builder.create_block();

    for &param in body.params() {
        let ty = infer.type_of_pat[param];
        let local = bcx.builder.add_local(LocalKind::Arg, ty);

        bcx.builder.add_block_param(entry, local);
        bcx.define_pat(param, Place::new(local));
    }

    bcx.builder.switch_block(entry);

    let res = bcx.lower_expr(body.body_expr());

    bcx.builder.return_(res);
    db.intern_body(Arc::new(bcx.builder.build()))
}

struct BodyLowerCtx<'db> {
    db: &'db dyn MirDatabase,
    builder: Builder,
    body: Arc<hir::Body>,
    infer: Arc<hir::InferenceResult<Ty, Constraint>>,
    locals: ArenaMap<PatId, Place>,
}

mod expr;
mod intrinsic;
mod pat;

use std::sync::Arc;

use arena::ArenaMap;
use hir::ty::{Constraint, Ty};
use hir::{AsName, PatId};

use crate::builder::Builder;
use crate::db::MirDatabase;
use crate::repr::Repr;
use crate::syntax::*;

struct LowerCtx<'db> {
    db: &'db dyn MirDatabase,
    builder: Builder,
    infer: Arc<hir::InferenceResult<Ty, Constraint>>,
}

struct BodyLowerCtx<'db> {
    cx: LowerCtx<'db>,
    body: Arc<hir::Body>,
    locals: ArenaMap<PatId, Place>,
}

impl<'db> std::ops::Deref for BodyLowerCtx<'db> {
    type Target = LowerCtx<'db>;

    fn deref(&self) -> &Self::Target {
        &self.cx
    }
}

impl<'db> std::ops::DerefMut for BodyLowerCtx<'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cx
    }
}

pub(crate) fn lower_body(db: &dyn MirDatabase, def: hir::id::DefWithBodyId) -> Body {
    let body = db.body(def);
    let infer = db.infer(def);
    let origin = BodyOrigin { def, expr: None };
    let mut bcx = BodyLowerCtx {
        cx: LowerCtx {
            db,
            infer: infer.clone(),
            builder: Builder::new(origin),
        },
        body: body.clone(),
        locals: ArenaMap::default(),
    };

    if let hir::id::DefWithBodyId::FuncId(f) = def {
        let f = hir::Func::from(f);
        tracing::debug!("lower_body({})", f.name(db.upcast()));
    }

    let entry = bcx.builder.create_block();

    for &param in body.params() {
        let ty = infer.type_of_pat[param];
        let repr = db.repr_of(ty);
        let local = bcx.builder.add_local(LocalKind::Arg, repr);

        bcx.builder.add_block_param(entry, local);
        bcx.define_pat(param, Place::new(local));
    }

    bcx.builder.switch_block(entry);

    let res = bcx.lower_expr(body.body_expr(), &mut None);

    bcx.builder.return_(res);
    db.intern_body(Arc::new(bcx.cx.builder.build()))
}

pub(crate) fn mir_main_shim(db: &dyn MirDatabase, main_fn: hir::Func) -> Body {
    let id = hir::id::FuncId::from(main_fn);
    let infer = db.infer(id.into());
    let origin = BodyOrigin {
        def: id.into(),
        expr: None,
    };

    let mut cx = LowerCtx {
        db,
        infer: infer.clone(),
        builder: Builder::new(origin),
    };

    let entry = cx.builder.create_block();
    cx.builder.switch_block(entry);

    let body = db.body(id.into());
    let mut methods = infer.methods[&(body.body_expr(), 0)].iter().copied();
    let member = match methods.next().unwrap() {
        | hir::MethodSource::Member(id) => id,
        | hir::MethodSource::Record(..) => unreachable!(),
    };

    let report_fn = match db.member_data(member).item(&"report".as_name()).unwrap() {
        | hir::id::AssocItemId::FuncId(f) => hir::Func::from(f),
        | _ => unreachable!(),
    };

    let report_sig = db.func_signature(report_fn);
    let report_ret = report_sig.ret.clone();
    let report = cx
        .builder
        .add_local(LocalKind::Tmp, Repr::Func(Box::new(report_sig), false));

    let main_sig = db.func_signature(main_fn);
    let main_ret = main_sig.ret.clone();
    let main_repr = Repr::Func(Box::new(main_sig), false);
    let main = cx.builder.add_local(LocalKind::Tmp, main_repr);
    let ret = cx.builder.add_local(LocalKind::Tmp, main_ret);
    let res = cx.builder.add_local(LocalKind::Tmp, report_ret);

    cx.builder.def_ref(Place::new(report), report_fn.into());
    cx.builder.def_ref(Place::new(main), main_fn.into());
    cx.builder.call(Place::new(ret), Operand::Move(Place::new(main)), []);
    cx.builder
        .call(Place::new(res), Operand::Move(Place::new(report)), [Operand::Move(
            Place::new(ret),
        )]);

    cx.builder.return_(Operand::Move(Place::new(res).field(0)));
    db.intern_body(Arc::new(cx.builder.build()))
}

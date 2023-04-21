mod expr;
mod intrinsics;

use arena::ArenaMap;
use hir::{Ctor, Value};
use hir_def::id::{CtorId, ValueDefId, ValueId};
use triomphe::Arc;

use crate::builder::Builder;
use crate::ir::*;
use crate::repr::{repr_of, Repr};
use crate::Db;

#[salsa::tracked]
pub fn value_mir(db: &dyn Db, id: ValueId) -> ValueDef {
    let value = Value::from(id);
    let name = value.link_name(db);
    tracing::debug!(?name);
    let body = if !value.has_body(db) {
        None
    } else {
        let body = hir_def::body::query(db, id).0;
        if body.params().is_empty() {
            if let hir_def::expr::Expr::Path { def: Some(def), .. } = body[body.body_expr()] {
                if let Some(def) = value_def_mir(db, def) {
                    return def;
                }
            }
        }

        let repr = repr_of(db, value.ty(db).ty());
        Some(Ctx::new(db, value).lower(repr))
    };

    ValueDef::new(db, Linkage::Export, name, body)
}

#[salsa::tracked]
pub fn ctor_mir(db: &dyn Db, id: CtorId) -> ValueDef {
    let ctor = Ctor::from(id);
    let type_ctor = ctor.type_ctor(db);
    let mut builder = Builder::default();
    let entry = builder.create_block();

    builder.switch_block(entry);

    let params = ctor
        .types(db)
        .iter()
        .map(|&ty| {
            let repr = repr_of(db, ty);
            let local = builder.add_local(LocalKind::Arg, repr);
            builder.add_block_param(entry, local);
            local
        })
        .collect::<Vec<_>>();

    let ret_repr = repr_of(db, ctor.ret(db));

    if params.is_empty() {
        builder.ret((Const::Ctor(id), ret_repr));
    } else {
        let ret = builder.add_local(LocalKind::Var, ret_repr.clone());
        builder.init(ret);
        let single_variant = type_ctor.ctors(db).len() == 1;
        let downcast = if single_variant {
            Place::new(ret)
        } else {
            Place::new(ret).downcast(id)
        };

        for (i, param) in params.into_iter().enumerate() {
            builder.assign(downcast.clone().field(i), Place::new(param));
        }

        if !single_variant {
            builder.set_discriminant(Place::new(ret), id);
        }

        builder.ret(Place::new(ret));
    }

    let repr = repr_of(db, ctor.ty(db).ty());
    let body = builder.build(db, MirValueId::CtorId(id), repr);
    let name = ctor.link_name(db);

    ValueDef::new(db, Linkage::Export, name, Some(body))
}

fn value_def_mir(db: &dyn Db, def: ValueDefId) -> Option<ValueDef> {
    match def {
        | ValueDefId::ValueId(id) => Some(value_mir(db, id)),
        | ValueDefId::CtorId(id) => Some(ctor_mir(db, id)),
        | ValueDefId::FixityId(id) => {
            let data = hir_def::data::fixity_data(db, id);

            value_def_mir(db, data.def(db)?.left()?)
        },
        | _ => None,
    }
}

pub struct Ctx<'db> {
    db: &'db dyn Db,
    id: MirValueId,
    body: Arc<hir_def::body::Body>,
    infer: Arc<hir_ty::ctx::InferResult>,
    builder: Builder,
    locals: ArenaMap<hir_def::pat::PatId, Place>,
    lambdas: Vec<(hir_def::expr::ExprId, Body)>,
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn Db, value: Value) -> Self {
        let body = hir_def::body::query(db, value.id()).0;
        let infer = hir_ty::infer(db, value.id());

        Self {
            db,
            body,
            infer,
            id: MirValueId::ValueId(value.id()),
            builder: Builder::default(),
            locals: ArenaMap::default(),
            lambdas: Vec::new(),
        }
    }

    pub fn for_lambda(&self, expr: hir_def::expr::ExprId) -> Self {
        let id = match self.id {
            | MirValueId::ValueId(value) => value,
            | MirValueId::Lambda(value, _) => value,
            | _ => unreachable!(),
        };

        Self {
            db: self.db,
            body: self.body.clone(),
            infer: self.infer.clone(),
            builder: Builder::default(),
            locals: ArenaMap::default(),
            id: MirValueId::Lambda(id, expr),
            lambdas: Vec::new(),
        }
    }

    pub fn lower(mut self, repr: Arc<Repr>) -> Body {
        for c in self.infer.constraints.iter() {
            self.builder.add_constraint(c.clone());
        }

        let body = self.body.clone();
        let entry = self.builder.create_block();
        self.builder.switch_block(entry);

        for &param in body.params() {
            let repr = repr_of(self.db, self.infer.type_of_pat[param]);
            let local = self.builder.add_local(LocalKind::Arg, repr);
            self.bind_pat(param, Place::new(local));
            self.builder.add_block_param(entry, local);
        }

        let res = self.lower_expr(body.body_expr(), &mut None);

        for (_, body) in self.lambdas {
            use hir_def::display::HirDisplay;
            tracing::debug!("\n{}", body.display(self.db));
        }

        self.builder.ret(res);
        self.builder.build(self.db, self.id, repr)
    }
}

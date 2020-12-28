mod expr;
mod pat;

use crate::constraint::{Constrain, Constraint};
use crate::error::TypeError;
use crate::{ty::*, TypeDatabase};
use hir::ir;
use std::collections::HashMap;

pub struct Ctx<'db> {
    pub(crate) db: &'db dyn TypeDatabase,
    pub(crate) file: source::FileId,
    pub(crate) var_kinds: HashMap<TypeVar, Ty>,
    pub(crate) tys: HashMap<ir::HirId, (Ty, ir::Span)>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) errors: Vec<TypeError>,
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn TypeDatabase, file: source::FileId) -> Self {
        Ctx {
            db,
            file,
            var_kinds: HashMap::new(),
            tys: HashMap::new(),
            constraints: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> HashMap<ir::HirId, Ty> {
        self.tys.into_iter().map(|(id, (ty, _))| (id, ty)).collect()
    }

    pub fn insert_var_kind(&mut self, var: TypeVar, kind: Ty) {
        self.var_kinds.insert(var, kind);
    }

    pub fn constrain(&mut self) -> Constrain<'_, 'db> {
        Constrain::new(self)
    }

    pub fn infer_body(&mut self, body: &ir::Body, expected: Ty, expected_span: ir::Span) {
        let param_tys = body
            .params
            .iter()
            .map(|p| {
                let ty = Ty::infer(self.db.new_infer_var());

                self.tys.insert(p.id, (ty.clone(), p.span));
                ty
            })
            .collect::<List<_>>();

        let ret = self.infer_expr(&body.value);
        let ty = Ty::func(param_tys, ret);
        // let ty = ty.generalize(body.id.0.owner);

        self.constrain()
            .equal(ty, body.value.span, expected, expected_span);
    }

    pub fn hir_ty(&mut self, ty: &ir::Type) -> Ty {
        match &ty.kind {
            ir::TypeKind::Error => Ty::error(),
            ir::TypeKind::Infer => Ty::infer(self.db.new_infer_var()),
            ir::TypeKind::Hole { name: _ } => Ty::infer(self.db.new_infer_var()),
            ir::TypeKind::Int { .. } => panic!("cannot use int as type directly"),
            ir::TypeKind::Ident { res } => match res {
                ir::Res::Error => Ty::error(),
                ir::Res::Def(_, id) => self.db.typecheck(*id).ty.clone(),
                ir::Res::Local(id) => Ty::var(TypeVar(*id)),
            },
            ir::TypeKind::Func { params, ret } => {
                let params = params.iter().map(|t| self.hir_ty(t)).collect();
                let ret = self.hir_ty(ret);

                Ty::func(params, ret)
            }
            ir::TypeKind::Tuple { tys } => {
                let tys = tys.iter().map(|t| self.hir_ty(t)).collect();

                Ty::tuple(tys)
            }
            ir::TypeKind::Record { row } => {
                let fields = row
                    .fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.symbol,
                        span: f.span,
                        ty: self.hir_ty(&f.ty),
                    })
                    .collect();

                let tail = row.tail.as_deref().map(|t| self.hir_ty(t));

                Ty::record(fields, tail)
            }
            ir::TypeKind::App { base, args } => {
                let base = self.hir_ty(base);
                let args = args.iter().map(|a| self.hir_ty(a)).collect();
                let ty = base.instantiate(&args);

                Ty::app(ty, base, args)
            }
            ir::TypeKind::Forall { vars, ty } => {
                let vars = vars
                    .iter()
                    .map(|v| {
                        let kind = self.hir_ty(&v.kind);
                        let var = TypeVar(v.id);

                        self.insert_var_kind(var, kind);
                        var
                    })
                    .collect();

                let ty = self.hir_ty(ty);

                Ty::for_all(vars, ty)
            }
            ir::TypeKind::Cons { cs, ty } => {
                let ctnt = TraitCtnt {
                    trait_: ir::HirId {
                        owner: cs.trait_,
                        local_id: ir::LocalId(0),
                    },
                    tys: cs.tys.iter().map(|t| self.hir_ty(t)).collect(),
                };

                let ty = self.hir_ty(ty);

                Ty::ctnt(ctnt, ty)
            }
            ir::TypeKind::Kinded { ty, kind } => {
                let ty_ = self.hir_ty(ty);
                let kind_ = self.hir_ty(kind);
                let ty_kind = self.infer_kind(&ty_);

                self.constrain().equal(ty_kind, ty.span, kind_, kind.span);

                ty_
            }
        }
    }

    pub fn infer_kind(&mut self, ty: &Ty) -> Ty {
        let kind_type = self.db.lang_items().kind_type();
        let kind_type = Ty::data(kind_type.owner);

        match &**ty {
            Type::Error => Ty::error(),
            Type::Int(_) => Ty::data(self.db.lang_items().kind_figure().owner),
            Type::TypeOf(def) => self.infer_kind(&self.db.typecheck(*def).ty),
            Type::Infer(_) => Ty::infer(self.db.new_infer_var()),
            Type::Var(var) => self.var_kinds[var].clone(),
            Type::Func(..) => kind_type,
            Type::Data(..) => kind_type,
            Type::Tuple(_) => kind_type,
            Type::Record(..) => kind_type,
            Type::ForAll(vars, ty) => {
                let ret = self.infer_kind(ty);

                Ty::func(
                    vars.iter().map(|var| self.var_kinds[var].clone()).collect(),
                    ret,
                )
            }
            Type::App(ty, _, _) => self.infer_kind(ty),
            Type::Ctnt(_, ty) => self.infer_kind(ty),
        }
    }

    pub fn verify(&self) {
        for (_, (ty, span)) in &self.tys {
            if let Type::Infer(_) = &**ty {
                self.db
                    .to_diag_db()
                    .error("could not infer type")
                    .with_label(diagnostics::Label::primary(self.file, *span))
                    .finish();
            }
        }
    }
}

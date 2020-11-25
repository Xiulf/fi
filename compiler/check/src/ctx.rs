mod expr;
mod pat;

use crate::constraint::Constraint;
use crate::{ty::*, TypeDatabase};
use hir::ir;
use std::collections::HashMap;

pub struct Ctx<'db> {
    pub(crate) db: &'db dyn TypeDatabase,
    pub(crate) file: source::FileId,
    pub(crate) var_kinds: HashMap<TypeVar, Ty>,
    pub(crate) tys: HashMap<ir::HirId, Ty>,
    pub(crate) cs: Vec<Constraint>,
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn TypeDatabase, file: source::FileId) -> Self {
        Ctx {
            db,
            file,
            var_kinds: HashMap::new(),
            tys: HashMap::new(),
            cs: Vec::new(),
        }
    }

    pub fn finish(self) -> HashMap<ir::HirId, Ty> {
        self.tys
    }

    pub fn insert_var_kind(&mut self, var: TypeVar, kind: Ty) {
        self.var_kinds.insert(var, kind);
    }

    pub fn constrain(&mut self, c: Constraint) {
        self.cs.push(c);
    }

    pub fn infer_body(&mut self, body: &ir::Body, expected: Ty, expected_span: ir::Span) {
        let param_tys = body
            .params
            .iter()
            .map(|p| {
                let ty = Ty::infer(self.db.new_infer_var());

                self.tys.insert(p.id, ty.clone());
                ty
            })
            .collect();

        let ret = self.infer_expr(&body.value);

        // self.unify();

        let ty = Ty::func(param_tys, ret);
        // let ty = ty.generalize(body.id.0.owner);

        self.constrain(Constraint::Equal(
            ty,
            body.value.span,
            expected,
            expected_span,
        ));
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

                Ty::app(ty, args)
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
            ir::TypeKind::Cons { cs: _, ty: _ } => {
                unimplemented!();
            }
            ir::TypeKind::Kinded { ty, kind } => {
                let ty_ = self.hir_ty(ty);
                let kind_ = self.hir_ty(kind);
                let ty_kind = self.infer_kind(&ty_);

                self.constrain(Constraint::Equal(ty_kind, ty.span, kind_, kind.span));

                ty_
            }
        }
    }

    pub fn infer_kind(&mut self, ty: &Ty) -> Ty {
        let kind_type = self.db.lang_items().kind_type();
        let kind_type = Ty::data(kind_type.owner);

        match &**ty {
            Type::Error => Ty::error(),
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
            Type::App(ty, _) => self.infer_kind(ty),
        }
    }
}

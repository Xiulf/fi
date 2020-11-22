use crate::{ty::*, TypeDatabase};
use hir::ir;
use std::collections::HashMap;

pub struct Ctx<'db> {
    db: &'db dyn TypeDatabase,
    module: ir::ModuleId,
    var_kinds: HashMap<TypeVar, Ty>,
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn TypeDatabase, module: ir::ModuleId) -> Self {
        Ctx {
            db,
            module,
            var_kinds: HashMap::new(),
        }
    }

    pub fn hir_ty(&mut self, ty: &ir::Type) -> Ty {
        match &ty.kind {
            ir::TypeKind::Error => Ty::Error,
            ir::TypeKind::Infer => Ty::Infer(self.db.new_infer_var()),
            ir::TypeKind::Hole { name: _ } => Ty::Infer(self.db.new_infer_var()),
            ir::TypeKind::Int { .. } => panic!("cannot use int as type directly"),
            ir::TypeKind::Ident { res } => match res {
                ir::Res::Error => Ty::Error,
                ir::Res::Def(_, id) => self.db.type_of(self.module, *id),
                ir::Res::Local(id) => Ty::Var(TypeVar(*id)),
            },
            ir::TypeKind::Func { params, ret } => {
                let params = params.iter().map(|t| self.hir_ty(t)).collect();
                let ret = self.hir_ty(ret);

                Ty::Func(params, Box::new(ret))
            }
            ir::TypeKind::Tuple { tys } => {
                let tys = tys.iter().map(|t| self.hir_ty(t)).collect();

                Ty::Tuple(tys)
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

                let tail = row.tail.as_deref().map(|t| Box::new(self.hir_ty(t)));

                Ty::Record(fields, tail)
            }
            ir::TypeKind::App { base, args } => {
                let base = self.hir_ty(base);
                let args = args.iter().map(|a| self.hir_ty(a)).collect();

                base.instantiate(args)
            }
            ir::TypeKind::Forall { vars, ty } => {
                let vars = vars
                    .iter()
                    .map(|v| {
                        let kind = self.hir_ty(&v.kind);
                        let var = TypeVar(v.id);

                        self.var_kinds.insert(var, kind);
                        var
                    })
                    .collect::<Vec<_>>();

                let ty = self.hir_ty(ty);

                Ty::ForAll(vars, Box::new(ty))
            }
            ir::TypeKind::Cons { cs, ty } => {
                unimplemented!();
            }
            ir::TypeKind::Kinded { ty, kind } => {
                let ty = self.hir_ty(ty);
                let kind = self.hir_ty(kind);
                let ty_kind = self.infer_kind(&ty);

                ty
            }
        }
    }

    fn infer_kind(&mut self, ty: &Ty) -> Ty {
        let kind_type = self.db.lang_items().kind_type();
        let kind_type = Ty::Data(kind_type.owner, Vec::new());

        match ty {
            Ty::Error => Ty::Error,
            Ty::Infer(_) => Ty::Infer(self.db.new_infer_var()),
            Ty::Var(var) => self.var_kinds[var].clone(),
            Ty::Func(..) => kind_type,
            Ty::Data(..) => kind_type,
            Ty::Tuple(_) => kind_type,
            Ty::Record(..) => kind_type,
            Ty::ForAll(vars, ty) => {
                let ret = self.infer_kind(ty);

                Ty::Func(
                    vars.iter().map(|var| self.var_kinds[var].clone()).collect(),
                    Box::new(ret),
                )
            }
        }
    }
}

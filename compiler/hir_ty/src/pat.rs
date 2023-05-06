use hir_def::expr::Literal;
use hir_def::pat::{Pat, PatId};

use crate::ctx::{BodyCtx, Expectation};
use crate::lower::LowerCtx;
use crate::ty::{FuncType, Ty, TyKind};

impl BodyCtx<'_, '_> {
    pub fn infer_pat(&mut self, id: PatId, expected: Expectation) -> Ty {
        let ty = self.infer_pat_inner(id, expected);

        if let Expectation::HasType(expected) = expected {
            self.unify_types(ty, expected, id.into());

            if matches!(self.resolve_type_shallow(ty).kind(self.db), TyKind::Error) {
                return expected;
            }
        }

        ty
    }

    fn infer_pat_inner(&mut self, id: PatId, expected: Expectation) -> Ty {
        if let Some(&ty) = self.result.type_of_pat.get(id) {
            return ty;
        }

        let body = self.body.clone();
        let ty = match &body[id] {
            | Pat::Missing => self.error(),
            | Pat::Typed { pat, ty } => {
                let (type_map, _, _) = self.owner.type_map(self.db);
                let mut lcx = LowerCtx::new(self, type_map);
                let ty = lcx.lower_type_ref(*ty, false);

                self.infer_pat(*pat, Expectation::HasType(ty));
                ty
            },
            | Pat::Wildcard => self.ctx.fresh_type(self.level, false),
            | Pat::Bind { subpat: None, .. } => self.ctx.fresh_type(self.level, false),
            | Pat::Bind {
                subpat: Some(subpat), ..
            } => self.infer_pat(*subpat, expected),
            | Pat::Ctor { ctor: None, args, .. } => {
                for &arg in args.iter() {
                    self.infer_pat_inner(arg, Expectation::None);
                }

                self.error()
            },
            | Pat::Ctor {
                ctor: Some(def), args, ..
            } => {
                let ty = crate::ctor_ty(self.db, *def);
                let (ty, _) = self.instantiate(ty, Vec::new(), None, false);

                if args.is_empty() {
                    ty
                } else {
                    let params = args
                        .iter()
                        .map(|a| self.infer_pat_inner(*a, Expectation::None))
                        .collect();
                    let ret = self.ctx.fresh_type(self.level, false);
                    let new_func = Ty::new(
                        self.db,
                        TyKind::Func(FuncType {
                            params,
                            ret,
                            env: self.unit_type(),
                            is_varargs: false,
                        }),
                    );

                    self.unify_types(ty, new_func, id.into());
                    ret
                }
            },
            | Pat::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let kind = self.int_tag_kind();
                    let var = self.ctx.fresh_type_with_kind(self.level, kind, false);
                    let int = self.int_type();

                    Ty::new(self.db, TyKind::App(int, Box::new([var])))
                },
                | Literal::Float(_) => {
                    let kind = self.float_tag_kind();
                    let var = self.ctx.fresh_type_with_kind(self.level, kind, false);
                    let float = self.float_type();

                    Ty::new(self.db, TyKind::App(float, Box::new([var])))
                },
                | l => todo!("{l:?}"),
            },
        };

        self.result.type_of_pat.insert(id, ty);
        ty
    }
}

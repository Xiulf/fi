use hir_def::expr::Literal;
use hir_def::pat::{Pat, PatId};

use crate::ctx::{BodyCtx, Expectation};
use crate::lower::LowerCtx;
use crate::ty::{Constraint, ConstraintOrigin, FuncType, Ty, TyKind};

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
        let body = self.body.clone();
        let ty = match &body[id] {
            | Pat::Missing => self.error(),
            | Pat::Wildcard => self.ctx.fresh_type(self.level, false),
            | Pat::Bind { subpat: None, .. } => self.ctx.fresh_type(self.level, false),
            | Pat::Bind {
                subpat: Some(subpat), ..
            } => self.infer_pat(*subpat, expected),
            | Pat::Ctor { ctor: None, .. } => self.error(),
            | Pat::Ctor {
                ctor: Some(def), args, ..
            } => {
                let ty = crate::ctor_ty(self.db, *def);
                let ty = self.instantiate(ty, false);

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
                            variadic: false,
                        }),
                    );

                    self.unify_types(ty, new_func, id.into());
                    ret
                }
            },
            | Pat::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let var = self.ctx.fresh_type(self.level, false);

                    if let Some(any_int) = self.any_int_trait() {
                        self.constrain(
                            Constraint {
                                trait_id: any_int,
                                args: Box::new([var]),
                            },
                            ConstraintOrigin::PatId(id),
                        );
                    }

                    var
                },
                | Literal::Float(_) => {
                    let var = self.ctx.fresh_type(self.level, false);

                    if let Some(any_float) = self.any_float_trait() {
                        self.constrain(
                            Constraint {
                                trait_id: any_float,
                                args: Box::new([var]),
                            },
                            ConstraintOrigin::PatId(id),
                        );
                    }

                    var
                },
                | l => todo!("{l:?}"),
            },
            | Pat::Typed { pat, ty } => {
                let (type_map, _, _) = self.owner.type_map(self.db);
                let mut lcx = LowerCtx::new(self, type_map);
                let ty = lcx.lower_type_ref(*ty, false);

                self.infer_pat(*pat, Expectation::HasType(ty))
            },
        };

        self.result.type_of_pat.insert(id, ty);
        ty
    }
}

use hir_def::id::TypeVarId;

use crate::ctx::Ctx;
use crate::ty::{Constraint, ConstraintOrigin, GeneralizedType, Ty, TyKind, Unknown};

impl Ctx<'_> {
    pub fn solve_constraints(&mut self) {
        let constraints = self.sort_constraints();

        for (c, o) in constraints {
            tracing::debug!("{o:?}: {c:?}");
        }
    }

    fn sort_constraints(&mut self) -> Vec<(Constraint, ConstraintOrigin)> {
        let constraints = std::mem::take(&mut self.constraints);
        let constraints = constraints
            .into_iter()
            .map(|(c, o)| (self.resolve_constraint_fully(c), o))
            .collect::<Vec<_>>();
        let mut res = Vec::with_capacity(constraints.len());
        let mut type_vars = Box::new([]) as Box<[_]>;
        let mut unknowns = Vec::new();
        let ty = match self.result.ty {
            | GeneralizedType::Mono(ty) => ty,
            | GeneralizedType::Poly(ref vars, ty) => {
                type_vars = vars.clone();
                ty
            },
        };

        let ty = self.resolve_type_fully(ty);

        ty.traverse(self.db, &mut |t| match t.kind(self.db) {
            | TyKind::Unknown(u, false) => unknowns.push(*u),
            | _ => {},
        });

        for (constraint, origin) in constraints {
            if self.should_propagate(&constraint, &type_vars, &unknowns) {
                self.result.constraints.push(constraint);
            } else {
                res.push((constraint, origin));
            }
        }

        res
    }

    fn should_propagate(&self, constraint: &Constraint, type_vars: &[TypeVarId], unknowns: &[Unknown]) -> bool {
        let db = self.db;
        let check = move |t: Ty| {
            let mut res = false;
            t.traverse(db, &mut |t| match t.kind(db) {
                | TyKind::Var(v) => res |= type_vars.contains(v),
                | TyKind::Unknown(u, false) => res |= unknowns.contains(u),
                | _ => {},
            });
            res
        };

        constraint.args.iter().any(|&t| check(t))
    }
}

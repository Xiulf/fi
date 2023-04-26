use base_db::libs::LibId;
use diagnostics::Diagnostics;
use hir_def::display::HirDisplay;
use hir_def::id::{ContainerId, HasModule, ImplId, TraitId, TypeVarId, TypedItemId};
use ra_ap_stdx::hash::NoHashHashMap;

use crate::ctx::Ctx;
use crate::diagnostics::UnsolvedConstraint;
use crate::ty::{Constraint, ConstraintOrigin, GeneralizedType, InstanceImpl, Ty, TyKind, Unknown};
use crate::unify::{UnifyBindings, UnifyResult};
use crate::Db;

const RECURSION_LIMIT: u32 = 32;

impl<'db> Ctx<'db> {
    pub fn solve_constraints(&mut self, allow_propagation: bool) {
        let constraints = if allow_propagation {
            self.sort_constraints()
        } else {
            std::mem::replace(&mut self.constraints, Vec::new())
                .into_iter()
                .map(|(c, o)| (self.resolve_constraint_fully(c), o))
                .collect()
        };

        let mut failing = self.try_solve_constraints(constraints.iter(), false);
        let mut prev_len = failing.len();

        loop {
            failing = self.try_solve_constraints(failing, false);

            if failing.is_empty() || failing.len() == prev_len {
                break;
            }

            prev_len = failing.len();
        }

        failing = self.try_solve_constraints(failing, true);

        for (c, o) in failing {
            Diagnostics::emit(self.db, UnsolvedConstraint {
                constraint: c,
                origin: *o,
                owner: self.owner,
            });
        }
    }

    fn try_solve_constraints<'a>(
        &mut self,
        constraints: impl IntoIterator<Item = &'a (Constraint, ConstraintOrigin)>,
        allow_defaults: bool,
    ) -> Vec<&'a (Constraint, ConstraintOrigin)> {
        constraints
            .into_iter()
            .filter_map(|constraint| {
                if allow_defaults {
                    for &arg in constraint.0.args.iter() {
                        self.default_literals(arg);
                    }
                }

                self.try_solve_constraint(constraint)
            })
            .collect()
    }

    pub(crate) fn default_literals(&mut self, ty: Ty) {
        let int_type = self.int_type();
        let float_type = self.float_type();
        let default_int_type = self.db.type_cache().default_int_ty(self.db);
        let default_float_type = self.db.type_cache().default_float_ty(self.db);
        let env = (int_type, default_int_type, float_type, default_float_type);
        type Env = (Ty, Ty, Ty, Ty);

        fn rec(db: &dyn Db, bindings: &mut UnifyBindings, env: Env, ty: Ty) {
            match ty.kind(db) {
                | TyKind::Unknown(u, _) => match bindings.0.get(u) {
                    | Some(t) => rec(db, bindings, env, *t),
                    | None => {},
                },
                | TyKind::App(base, args) => {
                    let base = bindings.resolve_type_shallow(db, *base);
                    let arg = bindings.resolve_type_shallow(db, args[0]);

                    if base == env.0 {
                        if let TyKind::Unknown(u, false) = arg.kind(db) {
                            bindings.0.insert(*u, env.1);
                        }
                    } else if base == env.2 {
                        if let TyKind::Unknown(u, false) = arg.kind(db) {
                            bindings.0.insert(*u, env.3);
                        }
                    }
                },
                | _ => {},
            }
        }

        ty.traverse(self.db, &mut |t| rec(self.db, &mut self.subst.solved, env, t))
    }

    fn try_solve_constraint<'a>(
        &mut self,
        c @ (constraint, origin): &'a (Constraint, ConstraintOrigin),
    ) -> Option<&'a (Constraint, ConstraintOrigin)> {
        let mut impls = self.find_impls(constraint, *origin, RECURSION_LIMIT);

        if impls.len() != 1 {
            return Some(c);
        }

        let (impls, bindings) = impls.remove(0);
        self.subst.solved.0.extend(bindings.0);

        for (impl_id, _, o) in impls {
            if let ConstraintOrigin::ExprId(expr, name) = o {
                if let Some(name) = name {
                    let data = hir_def::data::impl_data(self.db, impl_id);

                    if let Some(&method) = data.items(self.db).get(&name) {
                        self.result.methods.insert(expr, method);
                    }
                }

                self.result.instances[expr].impls.push(InstanceImpl::ImplId(impl_id));
            }
            tracing::debug!("{impl_id:?}, {o:?}");
        }

        None
    }

    #[tracing::instrument(skip_all)]
    fn find_impls(
        &mut self,
        constraint: &Constraint,
        origin: ConstraintOrigin,
        n: u32,
    ) -> Vec<(Vec<(ImplId, Constraint, ConstraintOrigin)>, UnifyBindings)> {
        tracing::debug!("find_impls({})", constraint.display(self.db));
        if n == 0 {
            tracing::warn!(
                "Recursion limit reached when searching for impls for {}",
                constraint.display(self.db)
            );
            return Vec::new();
        }

        self.all_impls(constraint.trait_id)
            .filter_map(|&impl_id| {
                let mut bindings = UnifyBindings::default();
                let mut replacements = NoHashHashMap::default();
                let type_vars = TypedItemId::ImplId(impl_id).type_map(self.db).2;
                let (types, constraints) = crate::impl_types(self.db, impl_id);

                for &var in type_vars.iter() {
                    let replacement = self.fresh_type(self.level, false);
                    replacements.insert(var, replacement);
                }

                let types = types
                    .iter()
                    .map(|t| t.replace_vars(self.db, &replacements))
                    .collect::<Box<[_]>>();

                if self.unify_all(types.iter(), constraint.args.iter(), &mut bindings) != UnifyResult::Ok {
                    return None;
                }

                let constraints = constraints
                    .iter()
                    .map(|c| c.replace_vars(self.db, &replacements))
                    .map(|c| bindings.resolve_constraint_fully(self.db, c))
                    .collect();

                self.check_impl_constraints(constraint, origin, impl_id, constraints, bindings, n)
            })
            .collect()
    }

    fn check_impl_constraints(
        &mut self,
        constraint: &Constraint,
        origin: ConstraintOrigin,
        impl_id: ImplId,
        constraints: Box<[Constraint]>,
        mut bindings: UnifyBindings,
        n: u32,
    ) -> Option<(Vec<(ImplId, Constraint, ConstraintOrigin)>, UnifyBindings)> {
        let mut impls = vec![(impl_id, constraint.clone(), origin)];

        for (_i, constraint) in constraints.iter().enumerate() {
            // let origin = ConstraintOrigin::Impl(impl_id, i);
            let mut matching = self.find_impls(constraint, origin, n - 1);

            if matching.len() != 1 {
                return None;
            }

            let (mut impls2, bindings2) = matching.remove(0);
            bindings.0.extend(bindings2.0);
            impls.append(&mut impls2);
        }

        Some((impls, bindings))
    }

    fn all_impls(&self, trait_id: TraitId) -> impl Iterator<Item = &'db ImplId> {
        let module = self.owner.module(self.db);
        let lib = module.lib(self.db);

        trait_impls(self.db, lib, trait_id).iter()
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
            | TyKind::Unknown(u, _) => unknowns.push(*u),
            | _ => {},
        });

        for (constraint, origin) in constraints {
            if self.should_propagate(&constraint, &type_vars, &unknowns) {
                if let Some(i) = self.result.constraints.iter().position(|c| c == &constraint) {
                    if let ConstraintOrigin::ExprId(expr, _) = origin {
                        self.result.instances[expr].impls.push(InstanceImpl::Param(i));
                    }
                } else {
                    let i = self.result.constraints.len();
                    if let ConstraintOrigin::ExprId(expr, _) = origin {
                        self.result.instances[expr].impls.push(InstanceImpl::Param(i));
                    }
                    self.result.constraints.push(constraint);
                }
            } else {
                res.push((constraint, origin));
            }
        }

        res
    }

    fn should_propagate(&self, constraint: &Constraint, type_vars: &[TypeVarId], unknowns: &[Unknown]) -> bool {
        if let TypedItemId::ValueId(id) = self.owner {
            if let ContainerId::ImplId(id) = id.container(self.db) {
                let (impl_types, _) = crate::impl_types(self.db, id);
                if constraint.args == *impl_types {
                    return false;
                }
            }
        }

        let db = self.db;
        let check = move |t: Ty| match t.kind(db) {
            | TyKind::Var(v) => type_vars.contains(v),
            | TyKind::Unknown(u, _) => unknowns.contains(u),
            | _ => false,
        };

        constraint.args.iter().any(|&t| check(t))
    }
}

#[salsa::tracked(return_ref)]
pub fn trait_impls(db: &dyn Db, lib: LibId, trait_id: TraitId) -> Vec<ImplId> {
    lib_impls(db, lib)
        .iter()
        .copied()
        .filter(move |&i| hir_def::data::impl_data(db, i).trait_id(db) == Some(trait_id))
        .collect()
}

#[salsa::tracked(return_ref)]
pub(crate) fn lib_impls(db: &dyn Db, lib: LibId) -> Vec<ImplId> {
    hir_def::def_map::query(db, lib)
        .modules()
        .flat_map(|(_, data)| data.scope(db).impls())
        .chain(lib.deps(db).iter().flat_map(|&dep| lib_impls(db, dep).iter().copied()))
        .collect()
}

use rustc_hash::FxHashMap;

use crate::class::{Class, Member};
use crate::db::HirDatabase;
use crate::infer::diagnostics::InferenceDiagnostic;
use crate::infer::InferenceResult;
use crate::info::{CtntInfo, FieldInfo, FromInfo, ToInfo, TyId, TyInfo, TySource, TypeVars, Types};
use crate::lower::{ClassLowerResult, LowerResult, MemberLowerResult};
use crate::ty::{Constraint, Field, List, Reason, Ty, TyAndSrc, TyKind, TypeVar, WhereClause};

impl ToInfo for TyAndSrc<Ty> {
    type Output = TyAndSrc<TyId>;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, type_vars: &mut TypeVars, src: TySource) -> Self::Output {
        TyAndSrc {
            ty: self.ty.to_info(db, types, type_vars, src),
            src,
        }
    }
}

impl FromInfo for TyAndSrc<Ty> {
    type Input = TyAndSrc<TyId>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        TyAndSrc {
            ty: Ty::from_info(db, types, input.ty),
            src: input.src,
        }
    }
}

impl ToInfo for Ty {
    type Output = TyId;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, type_vars: &mut TypeVars, src: TySource) -> Self::Output {
        let ty = rec(self, db, types, type_vars, src);
        let mut scopes = FxHashMap::default();

        ty.everything(types, &mut |t| match types[t] {
            | TyInfo::ForAll(ref kinds, _, scope) => {
                let new_scope = type_vars.alloc_scope(kinds.clone());

                scopes.insert(scope, new_scope);
            },
            | _ => {},
        });

        return ty.everywhere(true, types, &mut |types, t| match types[t].clone() {
            | TyInfo::TypeVar(tv) if scopes.contains_key(&tv.scope()) => {
                types.update(t, TyInfo::TypeVar(TypeVar::new(tv.idx(), scopes[&tv.scope()])), true)
            },
            | TyInfo::ForAll(kinds, inner, scope) => {
                let scope = scopes[&scope];

                types.update(t, TyInfo::ForAll(kinds, inner, scope), true)
            },
            | _ => t,
        });

        fn rec(this: Ty, db: &dyn HirDatabase, types: &mut Types, type_vars: &mut TypeVars, src: TySource) -> TyId {
            let info = match this.lookup(db) {
                | TyKind::Error(_) => TyInfo::Error,
                | TyKind::Figure(f) => TyInfo::Figure(f),
                | TyKind::Symbol(s) => TyInfo::Symbol(s),
                | TyKind::Row(fields, tail) => {
                    let fields = fields
                        .iter()
                        .map(|f| FieldInfo {
                            name: f.name.clone(),
                            ty: rec(f.ty, db, types, type_vars, src),
                        })
                        .collect();

                    let tail = tail.map(|t| rec(t, db, types, type_vars, src));

                    TyInfo::Row(fields, tail)
                },
                | TyKind::Ctor(id) => TyInfo::Ctor(id),
                | TyKind::App(base, args) => {
                    let base = rec(base, db, types, type_vars, src);
                    let args = args.iter().map(|&a| rec(a, db, types, type_vars, src)).collect();

                    TyInfo::App(base, args)
                },
                | TyKind::Where(where_, inner) => {
                    let where_ = where_.to_info(db, types, type_vars, src);
                    let inner = rec(inner, db, types, type_vars, src);

                    TyInfo::Where(where_, inner)
                },
                | TyKind::ForAll(vars, ret, scope) => {
                    let vars = vars
                        .iter()
                        .map(|&v| rec(v, db, types, type_vars, src))
                        .collect::<List<_>>();

                    let ret = rec(ret, db, types, type_vars, src);
                    // let new_scope = type_vars.alloc_scope(vars.clone());
                    // let ret = ret.rescope(types, scope, new_scope);

                    TyInfo::ForAll(vars, ret, scope)
                },
                | TyKind::TypeVar(tv) => TyInfo::TypeVar(tv),
                | _ => unimplemented!(),
            };

            types.insert(info, src)
        }
    }
}

impl FromInfo for Ty {
    type Input = TyId;

    fn from_info(db: &dyn HirDatabase, types: &Types, id: TyId) -> Self {
        let kind = match types[id] {
            | TyInfo::Error | TyInfo::Skolem(_, _) => TyKind::Error(Reason::Error),
            | TyInfo::Unknown(_) => TyKind::Error(Reason::Unknown),
            | TyInfo::TypeVar(tv) => TyKind::TypeVar(tv),
            | TyInfo::Figure(f) => TyKind::Figure(f),
            | TyInfo::Symbol(ref s) => TyKind::Symbol(s.clone()),
            | TyInfo::Row(ref fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: Self::from_info(db, types, f.ty),
                    })
                    .collect();

                let tail = tail.map(|t| Self::from_info(db, types, t));

                TyKind::Row(fields, tail)
            },
            | TyInfo::Ctor(id) => TyKind::Ctor(id),
            | TyInfo::App(base, ref args) => {
                let base = Self::from_info(db, types, base);
                let args = args.iter().map(|&a| Self::from_info(db, types, a)).collect();

                TyKind::App(base, args)
            },
            | TyInfo::Where(ref where_, inner) => {
                let where_ = WhereClause::from_info(db, types, where_.clone());
                let inner = Self::from_info(db, types, inner);

                TyKind::Where(where_, inner)
            },
            | TyInfo::ForAll(ref vars, inner, scope) => {
                let vars = vars.iter().map(|&v| Self::from_info(db, types, v)).collect();
                let inner = Self::from_info(db, types, inner);

                TyKind::ForAll(vars, inner, scope)
            },
        };

        kind.intern(db)
    }
}

impl ToInfo for WhereClause<Constraint> {
    type Output = WhereClause<CtntInfo>;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, type_vars: &mut TypeVars, src: TySource) -> Self::Output {
        WhereClause {
            constraints: self
                .constraints
                .iter()
                .map(|c| c.clone().to_info(db, types, type_vars, src))
                .collect(),
        }
    }
}

impl FromInfo for WhereClause<Constraint> {
    type Input = WhereClause<CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            constraints: input
                .constraints
                .iter()
                .map(|c| Constraint::from_info(db, types, c.clone()))
                .collect(),
        }
    }
}

impl ToInfo for Constraint {
    type Output = CtntInfo;

    fn to_info(
        self,
        db: &dyn HirDatabase,
        types: &mut Types,
        type_vars: &mut TypeVars,
        span: TySource,
    ) -> Self::Output {
        CtntInfo {
            class: self.class,
            types: self
                .types
                .iter()
                .map(|t| t.to_info(db, types, type_vars, span))
                .collect(),
        }
    }
}

impl FromInfo for Constraint {
    type Input = CtntInfo;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Constraint {
            class: input.class,
            types: input.types.iter().map(|&t| Ty::from_info(db, types, t)).collect(),
        }
    }
}

impl FromInfo for Class<Ty, Constraint> {
    type Input = Class<TyId, CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            id: input.id,
            fundeps: input.fundeps,
            vars: input.vars.iter().map(|&v| Ty::from_info(db, types, v)).collect(),
            where_clause: WhereClause::from_info(db, types, input.where_clause),
        }
    }
}

impl FromInfo for Member<Ty, Constraint> {
    type Input = Member<TyId, CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            id: input.id,
            class: input.class,
            vars: input.vars.iter().map(|&v| Ty::from_info(db, types, v)).collect(),
            types: input.types.iter().map(|&t| Ty::from_info(db, types, t)).collect(),
            where_clause: WhereClause::from_info(db, types, input.where_clause),
        }
    }
}

impl FromInfo for InferenceResult<Ty, Constraint> {
    type Input = InferenceResult<TyId, CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            self_type: TyAndSrc::from_info(db, types, input.self_type),
            type_of_expr: input
                .type_of_expr
                .iter()
                .map(|(id, &t)| (id, Ty::from_info(db, types, t)))
                .collect(),
            type_of_pat: input
                .type_of_pat
                .iter()
                .map(|(id, &t)| (id, Ty::from_info(db, types, t)))
                .collect(),
            kind_of_ty: input
                .kind_of_ty
                .iter()
                .map(|(id, &t)| (id, Ty::from_info(db, types, t)))
                .collect(),
            instances: input
                .instances
                .into_iter()
                .map(|(id, tys)| (id, tys.into_iter().map(|t| Ty::from_info(db, types, t)).collect()))
                .collect(),
            methods: input.methods,
            diagnostics: input
                .diagnostics
                .into_iter()
                .map(|d| InferenceDiagnostic::from_info(db, types, d))
                .collect(),
        }
    }
}

impl FromInfo for LowerResult<Ty> {
    type Input = LowerResult<TyId>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            ty: TyAndSrc::from_info(db, types, input.ty),
            types: input
                .types
                .iter()
                .map(|(id, &t)| (id, Ty::from_info(db, types, t)))
                .collect(),
            diagnostics: input.diagnostics,
        }
    }
}

impl FromInfo for ClassLowerResult<Ty, Constraint> {
    type Input = ClassLowerResult<TyId, CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            class: Class::from_info(db, types, input.class),
            diagnostics: input.diagnostics,
        }
    }
}

impl FromInfo for MemberLowerResult<Ty, Constraint> {
    type Input = MemberLowerResult<TyId, CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            member: Member::from_info(db, types, input.member),
            diagnostics: input.diagnostics,
        }
    }
}

impl FromInfo for InferenceDiagnostic<Ty, Constraint> {
    type Input = InferenceDiagnostic<TyId, CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        match input {
            | InferenceDiagnostic::UnresolvedValue { id } => Self::UnresolvedValue { id },
            | InferenceDiagnostic::UnresolvedType { owner, id } => Self::UnresolvedType { owner, id },
            | InferenceDiagnostic::UnresolvedClass { src } => Self::UnresolvedClass { src },
            | InferenceDiagnostic::UnresolvedOperator { src, idx } => Self::UnresolvedOperator { src, idx },
            | InferenceDiagnostic::PrivateValue { id } => Self::PrivateValue { id },
            | InferenceDiagnostic::PrivateType { owner, id } => Self::PrivateType { owner, id },
            | InferenceDiagnostic::PrivateClass { src } => Self::PrivateClass { src },
            | InferenceDiagnostic::PrivateOperator { src, idx } => Self::PrivateOperator { src, idx },
            | InferenceDiagnostic::MismatchedKind {
                expected,
                found,
                expected_src,
                found_src,
            } => Self::MismatchedKind {
                expected: Ty::from_info(db, types, expected),
                found: Ty::from_info(db, types, found),
                expected_src,
                found_src,
            },
            | InferenceDiagnostic::MismatchedType {
                id,
                expected,
                found,
                expected_src,
                found_src,
            } => Self::MismatchedType {
                id,
                expected: Ty::from_info(db, types, expected),
                found: Ty::from_info(db, types, found),
                expected_src,
                found_src,
            },
            | InferenceDiagnostic::UnsolvedConstraint { expected, found, ctnt } => Self::UnsolvedConstraint {
                expected,
                found,
                ctnt: Constraint::from_info(db, types, ctnt),
            },
            | InferenceDiagnostic::BreakOutsideLoop { id } => Self::BreakOutsideLoop { id },
            | InferenceDiagnostic::CannotBreakWithValue { id } => Self::CannotBreakWithValue { id },
            | InferenceDiagnostic::NextOutsideLoop { id } => Self::NextOutsideLoop { id },
            | InferenceDiagnostic::CannotNextWithValue { id } => Self::CannotNextWithValue { id },
        }
    }
}

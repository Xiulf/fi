use crate::class::{Class, Member};
use crate::db::HirDatabase;
use crate::infer::diagnostics::InferenceDiagnostic;
use crate::infer::InferenceResult;
use crate::info::{CtntInfo, FieldInfo, FromInfo, ToInfo, TyId, TyInfo, TySource, Types};
use crate::lower::{ClassLowerResult, LowerResult, MemberLowerResult};
use crate::ty::{Constraint, Field, Reason, Ty, TyKind};

impl ToInfo for Ty {
    type Output = TyId;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, span: TySource) -> Self::Output {
        let info = match self.lookup(db) {
            | TyKind::Error(_) => TyInfo::Error,
            | TyKind::Figure(f) => TyInfo::Figure(f),
            | TyKind::Symbol(s) => TyInfo::Symbol(s),
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: f.ty.to_info(db, types, span),
                    })
                    .collect();

                let tail = tail.map(|t| t.to_info(db, types, span));

                TyInfo::Row(fields, tail)
            },
            | TyKind::Ctor(id) => TyInfo::Ctor(id),
            | TyKind::App(base, args) => {
                let base = base.to_info(db, types, span);
                let args = args.iter().map(|a| a.to_info(db, types, span)).collect();

                TyInfo::App(base, args)
            },
            | TyKind::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.to_info(db, types, span)).collect();

                TyInfo::Tuple(tys)
            },
            | TyKind::Func(args, ret) => {
                let args = args.iter().map(|a| a.to_info(db, types, span)).collect();
                let ret = ret.to_info(db, types, span);

                TyInfo::Func(args, ret)
            },
            | TyKind::Ctnt(ctnt, inner) => {
                let ctnt = CtntInfo {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|t| t.to_info(db, types, span)).collect(),
                };

                let inner = inner.to_info(db, types, span);

                TyInfo::Ctnt(ctnt, inner)
            },
            | TyKind::ForAll(vars, ret, scope) => {
                let vars = vars.iter().map(|v| v.to_info(db, types, span)).collect();
                let ret = ret.to_info(db, types, span);

                TyInfo::ForAll(vars, ret, scope)
            },
            | TyKind::TypeVar(tv) => TyInfo::TypeVar(tv),
        };

        types.insert(info, span)
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
            | TyInfo::Tuple(ref tys) => {
                let tys = tys.iter().map(|&t| Self::from_info(db, types, t)).collect();

                TyKind::Tuple(tys)
            },
            | TyInfo::Func(ref args, ret) => {
                let args = args.iter().map(|&a| Self::from_info(db, types, a)).collect();
                let ret = Self::from_info(db, types, ret);

                TyKind::Func(args, ret)
            },
            | TyInfo::Ctnt(ref ctnt, inner) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|&t| Self::from_info(db, types, t)).collect(),
                };

                let inner = Self::from_info(db, types, inner);

                TyKind::Ctnt(ctnt, inner)
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

impl ToInfo for Constraint {
    type Output = CtntInfo;

    fn to_info(self, db: &dyn HirDatabase, types: &mut Types, span: TySource) -> Self::Output {
        CtntInfo {
            class: self.class,
            types: self.types.iter().map(|t| t.to_info(db, types, span)).collect(),
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

impl FromInfo for Class<Ty> {
    type Input = Class<TyId>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            id: input.id,
            fundeps: input.fundeps,
            vars: input.vars.iter().map(|&v| Ty::from_info(db, types, v)).collect(),
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
            constraints: input
                .constraints
                .iter()
                .cloned()
                .map(|c| Constraint::from_info(db, types, c))
                .collect(),
        }
    }
}

impl FromInfo for InferenceResult<Ty, Constraint> {
    type Input = InferenceResult<TyId, CtntInfo>;

    fn from_info(db: &dyn HirDatabase, types: &Types, input: Self::Input) -> Self {
        Self {
            self_type: Ty::from_info(db, types, input.self_type),
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
            ty: Ty::from_info(db, types, input.ty),
            types: input
                .types
                .iter()
                .map(|(id, &t)| (id, Ty::from_info(db, types, t)))
                .collect(),
            diagnostics: input.diagnostics,
        }
    }
}

impl FromInfo for ClassLowerResult<Ty> {
    type Input = ClassLowerResult<TyId>;

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
            | InferenceDiagnostic::UnresolvedType { id } => Self::UnresolvedType { id },
            | InferenceDiagnostic::UnresolvedClass { src } => Self::UnresolvedClass { src },
            | InferenceDiagnostic::UnresolvedOperator { id } => Self::UnresolvedOperator { id },
            | InferenceDiagnostic::PrivateValue { id } => Self::PrivateValue { id },
            | InferenceDiagnostic::PrivateType { id } => Self::PrivateType { id },
            | InferenceDiagnostic::PrivateClass { src } => Self::PrivateClass { src },
            | InferenceDiagnostic::PrivateOperator { id } => Self::PrivateOperator { id },
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
            | InferenceDiagnostic::UnsolvedConstraint { id, ctnt } => Self::UnsolvedConstraint {
                id,
                ctnt: Constraint::from_info(db, types, ctnt),
            },
            | InferenceDiagnostic::BreakOutsideLoop { id } => Self::BreakOutsideLoop { id },
            | InferenceDiagnostic::CannotBreakWithValue { id } => Self::CannotBreakWithValue { id },
            | InferenceDiagnostic::NextOutsideLoop { id } => Self::NextOutsideLoop { id },
            | InferenceDiagnostic::CannotNextWithValue { id } => Self::CannotNextWithValue { id },
        }
    }
}

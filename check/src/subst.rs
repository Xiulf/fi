use crate::constraint::*;
use crate::ty::*;
use std::collections::BTreeMap;

pub struct Subst<'tcx>(BTreeMap<TypeVar, Ty<'tcx>>);

impl<'tcx> Subst<'tcx> {
    pub fn empty() -> Self {
        Subst(BTreeMap::new())
    }

    pub fn compose(self, other: Self) -> Self {
        let mut self_subst = self
            .0
            .into_iter()
            .map(|(k, s)| (k, other.apply_ty(&s)))
            .collect::<BTreeMap<TypeVar, Ty<'tcx>>>();

        self_subst.extend(other.0);

        Subst(self_subst)
    }

    pub fn apply_cs(&self, cs: &Constraints<'tcx>) -> Constraints<'tcx> {
        cs.iter()
            .map(|c| match c {
                Constraint::Equal(a, a_span, b, b_span) => {
                    Constraint::Equal(self.apply_ty(a), *a_span, self.apply_ty(b), *b_span)
                }
                Constraint::PtrArith(a, a_span, b, b_span) => {
                    Constraint::PtrArith(self.apply_ty(a), *a_span, self.apply_ty(b), *b_span)
                }
                Constraint::IsNum(ty, span) => Constraint::IsNum(self.apply_ty(ty), *span),
                Constraint::IsInt(ty, span) => Constraint::IsInt(self.apply_ty(ty), *span),
                Constraint::Call(func, f_span, params, ret, r_span) => {
                    let params = params.clone();

                    for param in &params {
                        self.apply_ty(param.ty);
                    }

                    Constraint::Call(
                        self.apply_ty(func),
                        *f_span,
                        params,
                        self.apply_ty(ret),
                        *r_span,
                    )
                }
                Constraint::Field(obj, o_span, field, ty, t_span) => Constraint::Field(
                    self.apply_ty(obj),
                    *o_span,
                    *field,
                    self.apply_ty(ty),
                    *t_span,
                ),
                Constraint::Index(list, l_span, ty, t_span) => {
                    Constraint::Index(self.apply_ty(list), *l_span, self.apply_ty(ty), *t_span)
                }
            })
            .collect()
    }

    pub fn apply_ty(&self, ty: Ty<'tcx>) -> Ty<'tcx> {
        for (tvar, solution) in &self.0 {
            subst_var(ty, tvar, solution);
        }

        ty
    }
}

fn subst_var(ty: Ty, tvar: &TypeVar, repl: Ty) {
    match ty {
        Type::Var(tvar2) | Type::VInt(tvar2) | Type::VUInt(tvar2) | Type::VFloat(tvar2)
            if tvar == tvar2 =>
        {
            let ptr = ty as *const Type;
            let ptr_mut = ptr as *mut Type;

            unsafe {
                *ptr_mut = (*repl).clone();
            }
        }
        Type::Var(_)
        | Type::VInt(_)
        | Type::VUInt(_)
        | Type::VFloat(_)
        | Type::Error
        | Type::Never
        | Type::Bool
        | Type::Str
        | Type::TypeId
        | Type::Int(_)
        | Type::UInt(_)
        | Type::Float(_) => {}
        Type::Ref(_, to) => subst_var(to, tvar, repl),
        Type::Array(of, _) => subst_var(of, tvar, repl),
        Type::Slice(of) => subst_var(of, tvar, repl),
        Type::Tuple(tys) => {
            for ty in *tys {
                subst_var(ty, tvar, repl);
            }
        }
        Type::Func(params, ret) => {
            for param in *params {
                subst_var(param.ty, tvar, repl);
            }

            subst_var(ret, tvar, repl);
        }
    }
}

impl<'tcx> std::iter::FromIterator<(TypeVar, Ty<'tcx>)> for Subst<'tcx> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (TypeVar, Ty<'tcx>)>>(iter: T) -> Self {
        Subst(<_>::from_iter(iter))
    }
}

use crate::constraint::*;
use crate::ty::*;
use std::collections::BTreeMap;

pub struct Subst<'tcx>(BTreeMap<TypeVar, Ty<'tcx>>);

impl<'tcx> Subst<'tcx> {
    pub fn empty() -> Self {
        Subst(BTreeMap::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn compose(&mut self, other: Self) {
        // self.0.iter_mut().for_each(|(_, s)| {
        //     other.apply_ty(s);
        // });

        self.0.extend(other.0);
    }

    pub fn apply_cs(&self, cs: &mut Constraints<'tcx>) {
        cs.iter_mut().for_each(|c| match c {
            Constraint::Equal(a, _, b, _) => {
                *a = self.apply_ty(a);
                *b = self.apply_ty(b);
            }
            Constraint::PtrArith(a, _, b, _) => {
                *a = self.apply_ty(a);
                *b = self.apply_ty(b);
            }
            Constraint::IsNum(ty, _) => {
                *ty = self.apply_ty(ty);
            }
            Constraint::IsInt(ty, _) => {
                *ty = self.apply_ty(ty);
            }
            Constraint::Call(func, _, params, ret, _) => {
                *func = self.apply_ty(func);
                *ret = self.apply_ty(ret);

                for param in params {
                    param.ty = self.apply_ty(param.ty);
                }
            }
            Constraint::Field(obj, _, _, ty, _) => {
                *obj = self.apply_ty(obj);
                *ty = self.apply_ty(ty);
            }
            Constraint::Index(list, _, ty, _) => {
                *list = self.apply_ty(list);
                *ty = self.apply_ty(ty);
            }
        })
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
        Type::Struct(_, fields) => {
            for field in *fields {
                subst_var(field.ty, tvar, repl);
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

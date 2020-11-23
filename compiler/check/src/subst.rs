use crate::constraint::*;
use crate::ty::*;
use std::collections::HashMap;

pub struct Subst(HashMap<InferVar, Ty>);

impl Subst {
    pub fn empty() -> Self {
        Subst(HashMap::new())
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

    pub fn apply_cs(&self, cs: &mut Vec<Constraint>) {
        cs.iter_mut().for_each(|c| match c {
            Constraint::Equal(a, _, b, _) => {
                *a = self.apply_ty(a);
                *b = self.apply_ty(b);
            }
        })
    }

    pub fn apply_ty(&self, ty: &Ty) -> Ty {
        for (ivar, solution) in &self.0 {
            subst_var(ty, ivar, solution.clone());
        }

        ty.clone()
    }
}

fn subst_var(ty: &Ty, ivar: &InferVar, repl: Ty) {
    match &**ty {
        Type::Infer(ivar2) if ivar == ivar2 => {
            let ptr = &**ty as *const Type;
            let ptr_mut = ptr as *mut Type;

            unsafe {
                *ptr_mut = (*repl).clone();
            }
        }
        Type::Infer(_) | Type::Var(_) | Type::Error | Type::TypeOf(_) => {}
        Type::Tuple(tys) => {
            for ty in &**tys {
                subst_var(ty, ivar, repl.clone());
            }
        }
        Type::Record(fields, tail) => {
            for field in &**fields {
                subst_var(&field.ty, ivar, repl.clone());
            }

            if let Some(tail) = tail {
                subst_var(tail, ivar, repl);
            }
        }
        Type::Data(_, variants) => {
            for variant in &**variants {
                for ty in &*variant.tys {
                    subst_var(ty, ivar, repl.clone());
                }
            }
        }
        Type::Func(params, ret) => {
            for param in &**params {
                subst_var(param, ivar, repl.clone());
            }

            subst_var(ret, ivar, repl);
        }
        Type::App(base, args) => {
            for arg in &**args {
                subst_var(arg, ivar, repl.clone());
            }

            subst_var(base, ivar, repl);
        }
        Type::ForAll(_, ty) => subst_var(ty, ivar, repl),
    }
}

impl std::iter::FromIterator<(InferVar, Ty)> for Subst {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (InferVar, Ty)>>(iter: T) -> Self {
        Subst(<_>::from_iter(iter))
    }
}

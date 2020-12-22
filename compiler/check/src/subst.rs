use crate::constraint::*;
use crate::ty::*;
use std::collections::BTreeMap;
use std::fmt;

pub struct Subst(BTreeMap<InferVar, Ty>);
pub struct SubstDisplay<'a>(&'a Subst, &'a dyn crate::TypeDatabase);

impl Subst {
    pub fn empty() -> Self {
        Subst(BTreeMap::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn display<'a>(&'a self, db: &'a dyn crate::TypeDatabase) -> SubstDisplay<'a> {
        SubstDisplay(self, db)
    }

    pub fn compose(&mut self, other: Self) {
        self.0.iter_mut().for_each(|(_, s)| {
            other.apply_ty(s);
        });

        self.0.extend(other.0);
    }

    pub fn apply_cs(&self, cs: &mut Vec<Constraint>) {
        cs.iter_mut().for_each(|c| match c {
            Constraint::Equal(a, b) => {
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
        Type::Int(_)
        | Type::Infer(_)
        | Type::Var(_)
        | Type::Error
        | Type::TypeOf(_)
        | Type::Data(_) => {}
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
        Type::Func(params, ret) => {
            for param in &**params {
                subst_var(param, ivar, repl.clone());
            }

            subst_var(ret, ivar, repl);
        }
        Type::App(base, orig, args) => {
            for arg in &**args {
                subst_var(arg, ivar, repl.clone());
            }

            subst_var(base, ivar, repl.clone());
            subst_var(orig, ivar, repl);
        }
        Type::ForAll(_, ty) => subst_var(ty, ivar, repl),
        Type::Ctnt(ctnt, ty) => {
            for ty in &*ctnt.tys {
                subst_var(ty, ivar, repl.clone());
            }

            subst_var(ty, ivar, repl);
        }
    }
}

impl std::iter::FromIterator<(InferVar, Ty)> for Subst {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (InferVar, Ty)>>(iter: T) -> Self {
        Subst(<_>::from_iter(iter))
    }
}

impl fmt::Display for SubstDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;

        for (var, ty) in &(self.0).0 {
            if first {
                first = false;
            } else {
                writeln!(f)?;
            }

            write!(f, "{} <- {}", var, ty.display(self.1))?;
        }

        Ok(())
    }
}

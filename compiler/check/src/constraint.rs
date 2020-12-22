use crate::ctx::Ctx;
use crate::error::{TypeError, UnifyError};
use crate::ty::*;
use hir::ir::Span;
use std::fmt;

pub struct Constrain<'ctx, 'db> {
    pub(crate) ctx: &'ctx mut Ctx<'db>,
}

pub enum Constraint {
    Equal(Ty, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitCtnt {
    pub trait_: hir::ir::HirId,
    pub tys: List<Ty>,
}

impl<'ctx, 'db> Constrain<'ctx, 'db> {
    pub fn new(ctx: &'ctx mut Ctx<'db>) -> Self {
        Constrain { ctx }
    }

    pub fn equal(&mut self, a: Ty, a_span: Span, b: Ty, b_span: Span) {
        let result = self.unify_one(Constraint::Equal(a.clone(), b.clone()));

        match result {
            Ok(subst) => {
                subst.apply_ty(&a);
                subst.apply_ty(&b);

                for (_, (ty, _)) in &mut self.tys {
                    subst.apply_ty(ty);
                }
            }
            Err(UnifyError::Mismatch) => {
                self.errors.push(TypeError::Mismatched {
                    a_ty: a,
                    a_span,
                    b_ty: b,
                    b_span,
                });
            }
        }
    }
}

impl<'ctx, 'db> std::ops::Deref for Constrain<'ctx, 'db> {
    type Target = Ctx<'db>;

    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

impl<'ctx, 'db> std::ops::DerefMut for Constrain<'ctx, 'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ctx
    }
}

pub struct CsDisplay<'a>(&'a Constraint, &'a dyn crate::TypeDatabase);

impl Constraint {
    pub fn display<'a>(&'a self, db: &'a dyn crate::TypeDatabase) -> CsDisplay<'a> {
        CsDisplay(self, db)
    }
}

impl fmt::Display for CsDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Constraint::Equal(a, b) => write!(f, "{} == {}", a.display(self.1), b.display(self.1)),
        }
    }
}

pub struct CtntDisplay<'a>(&'a TraitCtnt, &'a dyn crate::TypeDatabase);

impl TraitCtnt {
    pub fn display<'a>(&'a self, db: &'a dyn crate::TypeDatabase) -> CtntDisplay<'a> {
        CtntDisplay(self, db)
    }
}

impl fmt::Display for CtntDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CtntDisplay(this, db) = self;
        let file = db
            .module_tree(this.trait_.owner.lib)
            .file(this.trait_.owner.module);
        let hir = db.module_hir(file);
        let item = &hir.items[&this.trait_];

        item.name.fmt(f)?;

        for ty in &this.tys {
            write!(f, " {}", ty.display(*db))?;
        }

        Ok(())
    }
}

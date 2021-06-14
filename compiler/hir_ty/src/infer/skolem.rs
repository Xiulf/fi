use super::InferenceContext;
use crate::ty::*;

impl InferenceContext<'_> {
    pub fn skolemize(&mut self, kind: Ty, inner: Ty) -> Ty {
        self.skolemize_impl(kind, inner, DebruijnIndex::INNER)
    }

    pub fn unskolemize(&mut self, ty: Ty) -> Ty {
        ty.everywhere(self.db, &mut |ty| match ty.lookup(self.db) {
            | TyKind::Skolem(sk, _) => sk.to_ty(self.db),
            | _ => ty,
        })
    }

    fn skolemize_impl(&mut self, kind: Ty, inner: Ty, debruijn: DebruijnIndex) -> Ty {
        match inner.lookup(self.db) {
            | TyKind::TypeVar(var) if var.debruijn() == debruijn => TyKind::Skolem(var, kind).intern(self.db),
            | TyKind::Skolem(sk, k) => {
                let k = self.skolemize_impl(kind, k, debruijn);

                TyKind::Skolem(sk, k).intern(self.db)
            },
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: self.skolemize_impl(kind, f.ty, debruijn),
                    })
                    .collect();

                let tail = tail.map(|t| self.skolemize_impl(kind, t, debruijn));

                TyKind::Row(fields, tail).intern(self.db)
            },
            | TyKind::Tuple(tys) => {
                let tys = tys.iter().map(|&t| self.skolemize_impl(kind, t, debruijn)).collect();

                TyKind::Tuple(tys).intern(self.db)
            },
            | TyKind::App(a, b) => {
                let a = self.skolemize_impl(kind, a, debruijn);
                let b = self.skolemize_impl(kind, b, debruijn);

                TyKind::App(a, b).intern(self.db)
            },
            | TyKind::Ctnt(ctnt, ty) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    types: ctnt
                        .types
                        .iter()
                        .map(|&t| self.skolemize_impl(kind, t, debruijn))
                        .collect(),
                };

                let ty = self.skolemize_impl(kind, ty, debruijn);

                TyKind::Ctnt(ctnt, ty).intern(self.db)
            },
            | TyKind::ForAll(k, inner) => {
                let k = self.skolemize_impl(kind, k, debruijn);
                let inner = self.skolemize_impl(kind, inner, debruijn.shifted_in());

                TyKind::ForAll(k, inner).intern(self.db)
            },
            | _ => inner,
        }
    }
}

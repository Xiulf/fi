use super::InferenceContext;
use crate::ty::*;

impl InferenceContext<'_> {
    pub fn enter_universe(&mut self) -> Skolem {
        self.universes = self.universes.next();

        Skolem::new(self.universes)
    }

    pub fn exit_universe(&mut self) {
        self.universes = self.universes.prev();
    }

    pub fn skolemize(&mut self, skolem: Skolem, kind: Ty, inner: Ty) -> Ty {
        self.skolemize_impl(skolem, kind, inner, DebruijnIndex::INNER)
    }

    fn skolemize_impl(&mut self, skolem: Skolem, kind: Ty, inner: Ty, debruijn: DebruijnIndex) -> Ty {
        match inner.lookup(self.db) {
            | TyKind::TypeVar(var) if var.debruijn() == debruijn => skolem.to_ty(self.db, kind),
            | TyKind::Skolem(sk, k) => {
                let k = self.skolemize_impl(skolem, kind, k, debruijn);

                sk.to_ty(self.db, k)
            },
            | TyKind::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: self.skolemize_impl(skolem, kind, f.ty, debruijn),
                    })
                    .collect();

                let tail = tail.map(|t| self.skolemize_impl(skolem, kind, t, debruijn));

                TyKind::Row(fields, tail).intern(self.db)
            },
            | TyKind::Tuple(tys) => {
                let tys = tys
                    .iter()
                    .map(|&t| self.skolemize_impl(skolem, kind, t, debruijn))
                    .collect();

                TyKind::Tuple(tys).intern(self.db)
            },
            | TyKind::App(a, b) => {
                let a = self.skolemize_impl(skolem, kind, a, debruijn);
                let b = self.skolemize_impl(skolem, kind, b, debruijn);

                TyKind::App(a, b).intern(self.db)
            },
            | TyKind::Ctnt(ctnt, ty) => {
                let ctnt = Constraint {
                    class: ctnt.class,
                    tys: ctnt
                        .tys
                        .iter()
                        .map(|&t| self.skolemize_impl(skolem, kind, t, debruijn))
                        .collect(),
                };

                let ty = self.skolemize_impl(skolem, kind, ty, debruijn);

                TyKind::Ctnt(ctnt, ty).intern(self.db)
            },
            | TyKind::ForAll(k, inner) => {
                let k = self.skolemize_impl(skolem, kind, k, debruijn);
                let inner = self.skolemize_impl(skolem, kind, inner, debruijn.shifted_in());

                TyKind::ForAll(k, inner).intern(self.db)
            },
            | _ => inner,
        }
    }
}

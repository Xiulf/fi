use super::InferenceContext;
use crate::{
    info::{CtntInfo, FieldInfo, TyId, TyInfo},
    ty::DebruijnIndex,
};

impl InferenceContext<'_> {
    pub fn skolemize(&mut self, kinds: &[TyId], inner: TyId) -> TyId {
        self.skolemize_impl(kinds, inner, DebruijnIndex::INNER)
    }

    pub fn unskolemize(&mut self, ty: TyId) -> TyId {
        ty.everywhere(&mut self.types, &mut |types, ty| match types[ty] {
            | TyInfo::Skolem(sk, _) => types.update(ty, TyInfo::TypeVar(sk)),
            | _ => ty,
        })
    }

    fn skolemize_impl(&mut self, kinds: &[TyId], inner: TyId, debruijn: DebruijnIndex) -> TyId {
        match self.types[inner].clone() {
            | TyInfo::TypeVar(var) if var.debruijn() == debruijn => {
                self.types.update(inner, TyInfo::Skolem(var, kinds[var.idx() as usize]))
            },
            | TyInfo::Skolem(sk, k) => {
                let k = self.skolemize_impl(kinds, k, debruijn);

                self.types.update(inner, TyInfo::Skolem(sk, k))
            },
            | TyInfo::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: self.skolemize_impl(kinds, f.ty, debruijn),
                    })
                    .collect();

                let tail = tail.map(|t| self.skolemize_impl(kinds, t, debruijn));

                self.types.update(inner, TyInfo::Row(fields, tail))
            },
            | TyInfo::Tuple(tys) => {
                let tys = tys.iter().map(|&t| self.skolemize_impl(kinds, t, debruijn)).collect();

                self.types.update(inner, TyInfo::Tuple(tys))
            },
            | TyInfo::App(a, b) => {
                let a = self.skolemize_impl(kinds, a, debruijn);
                let b = b.iter().map(|&b| self.skolemize_impl(kinds, b, debruijn)).collect();

                self.types.update(inner, TyInfo::App(a, b))
            },
            | TyInfo::Func(a, b) => {
                let a = a.iter().map(|&a| self.skolemize_impl(kinds, a, debruijn)).collect();
                let b = self.skolemize_impl(kinds, b, debruijn);

                self.types.update(inner, TyInfo::Func(a, b))
            },
            | TyInfo::Ctnt(ctnt, ty) => {
                let ctnt = CtntInfo {
                    class: ctnt.class,
                    types: ctnt
                        .types
                        .iter()
                        .map(|&t| self.skolemize_impl(kinds, t, debruijn))
                        .collect(),
                };

                let ty = self.skolemize_impl(kinds, ty, debruijn);

                self.types.update(inner, TyInfo::Ctnt(ctnt, ty))
            },
            | TyInfo::ForAll(k, ty) => {
                let k = k.iter().map(|&k| self.skolemize_impl(kinds, k, debruijn)).collect();
                let ty = self.skolemize_impl(kinds, ty, debruijn.shifted_in());

                self.types.update(inner, TyInfo::ForAll(k, ty))
            },
            | _ => inner,
        }
    }
}

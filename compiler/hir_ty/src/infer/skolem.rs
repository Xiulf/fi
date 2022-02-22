use super::InferenceContext;
use crate::info::{CtntInfo, FieldInfo, TyId, TyInfo, TypeVarScopeId};

impl InferenceContext<'_> {
    pub fn unskolemize(&mut self, ty: TyId) -> TyId {
        ty.everywhere(&mut self.types, &mut |types, ty| match types[ty] {
            | TyInfo::Skolem(sk, _) => types.update(ty, TyInfo::TypeVar(sk)),
            | _ => ty,
        })
    }

    pub fn skolemize(&mut self, kinds: &[TyId], inner: TyId, scope: TypeVarScopeId) -> TyId {
        match self.types[inner].clone() {
            | TyInfo::TypeVar(var) if var.scope() == scope => {
                self.types.update(inner, TyInfo::Skolem(var, kinds[var.idx() as usize]))
            },
            | TyInfo::Skolem(sk, k) => {
                let k = self.skolemize(kinds, k, scope);

                self.types.update(inner, TyInfo::Skolem(sk, k))
            },
            | TyInfo::Row(fields, tail) => {
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: self.skolemize(kinds, f.ty, scope),
                    })
                    .collect();

                let tail = tail.map(|t| self.skolemize(kinds, t, scope));

                self.types.update(inner, TyInfo::Row(fields, tail))
            },
            | TyInfo::Tuple(tys) => {
                let tys = tys.iter().map(|&t| self.skolemize(kinds, t, scope)).collect();

                self.types.update(inner, TyInfo::Tuple(tys))
            },
            | TyInfo::App(a, b) => {
                let a = self.skolemize(kinds, a, scope);
                let b = b.iter().map(|&b| self.skolemize(kinds, b, scope)).collect();

                self.types.update(inner, TyInfo::App(a, b))
            },
            | TyInfo::Func(a, b) => {
                let a = a.iter().map(|&a| self.skolemize(kinds, a, scope)).collect();
                let b = self.skolemize(kinds, b, scope);

                self.types.update(inner, TyInfo::Func(a, b))
            },
            | TyInfo::Ctnt(ctnt, ty) => {
                let ctnt = CtntInfo {
                    class: ctnt.class,
                    types: ctnt.types.iter().map(|&t| self.skolemize(kinds, t, scope)).collect(),
                };

                let ty = self.skolemize(kinds, ty, scope);

                self.types.update(inner, TyInfo::Ctnt(ctnt, ty))
            },
            | TyInfo::ForAll(k, ty, s) => {
                let k = k.iter().map(|&k| self.skolemize(kinds, k, scope)).collect();
                let ty = self.skolemize(kinds, ty, scope);

                self.types.update(inner, TyInfo::ForAll(k, ty, s))
            },
            | _ => inner,
        }
    }
}

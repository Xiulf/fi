use crate::ctx::*;
use crate::ty::*;
use hir::ir::Span;

crate fn unskolemize(ty: Ty) -> Ty {
    ty.everywhere(&mut |t| match &*t {
        Type::Skolem(var, ..) => Ty::var(t.span(), t.file(), *var),
        _ => t,
    })
}

impl<'db> Ctx<'db> {
    /// Generate a new skolem constant.
    crate fn new_skolem_constant(&mut self) -> Skolem {
        let s = self.next_skolem;
        let _ = self.next_skolem += 1;

        Skolem(s)
    }

    /// Generate a new skolem scope.
    crate fn new_skolem_scope(&mut self) -> SkolemScope {
        let s = self.next_scope;
        let _ = self.next_scope += 1;

        SkolemScope(s)
    }

    /// Introduce a skolem scope at every occurrence of a ForAll.
    crate fn introduce_skolem_scope(&mut self, ty: Ty) -> Ty {
        ty.everywhere(&mut |t| match &*t {
            Type::ForAll(vars, r, None) => {
                Ty::forall(t.span(), t.file(), vars, r.clone(), self.new_skolem_scope())
            }
            _ => t,
        })
    }

    /// Skolemize type variables by replacing its instanes with fresh skolem constants.
    crate fn skolemize(
        &mut self,
        span: Span,
        file: source::FileId,
        vars: &[(TypeVar, Option<Ty>)],
        skolems: Vec<Skolem>,
        ty: Ty,
        scope: SkolemScope,
    ) -> Ty {
        ty.replace_vars(
            vars.iter()
                .zip(skolems)
                .map(|((v, k), s)| (*v, Ty::skolem(span, file, *v, k.clone(), s, scope)))
                .collect(),
        )
    }
}

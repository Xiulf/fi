use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;

impl<'db> Ctx<'db> {
    crate fn infer(&mut self, expr: &ir::Expr) -> Result<Ty> {
        let ty = match &expr.kind {
            ir::ExprKind::Error => Ty::error(expr.span),
            ir::ExprKind::Ident { res } => match res {
                ir::Res::Error => Ty::error(expr.span),
                ir::Res::Local(id) => {
                    let ty = self.tys[id].clone();
                    let ty = self.introduce_skolem_scope(ty);

                    if let Type::Ctnt(ctnt, ty) = &*ty {
                        todo!();
                        // ty.clone()
                    } else {
                        ty
                    }
                }
                ir::Res::Def(ir::DefKind::Ctor, id) => {
                    let ty = self.db.typecheck(*id).ty.clone();
                    let ty = self.instantiate(ty);

                    self.introduce_skolem_scope(ty)
                }
                ir::Res::Def(_, id) => {
                    let ty = self.db.typecheck(*id).ty.clone();
                    let ty = self.introduce_skolem_scope(ty);

                    if let Type::Ctnt(ctnt, ty) = &*ty {
                        todo!();
                        // ty.clone()
                    } else {
                        ty
                    }
                }
            },
            _ => unimplemented!("infer {:?}", expr),
        };

        Ok(ty)
    }
}

use hir_def::id::TypeDefId;
use hir_def::type_ref::{TypeMap, TypeRef, TypeRefId};
use triomphe::Arc;

use crate::ctx::Ctx;
use crate::ty::{FuncType, Ty, TyKind};

pub struct LowerCtx<'db, 'ctx> {
    ctx: &'ctx mut Ctx<'db>,
    type_map: Arc<TypeMap>,
}

impl<'db, 'ctx> LowerCtx<'db, 'ctx> {
    pub fn new(ctx: &'ctx mut Ctx<'db>, type_map: Arc<TypeMap>) -> Self {
        Self { ctx, type_map }
    }

    pub fn lower_type_ref(&mut self, id: TypeRefId) -> Ty {
        let ty = self.lower_type_ref_inner(id);

        // TODO: set kind
        ty
    }

    fn lower_type_ref_inner(&mut self, id: TypeRefId) -> Ty {
        let type_map = self.type_map.clone();

        match &type_map[id] {
            | TypeRef::Missing => Ty::new(self.db, TyKind::Error),
            | TypeRef::Hole => self.ctx.fresh_type(self.level, false),
            | TypeRef::Path { def: None, .. } => Ty::new(self.db, TyKind::Error),
            | TypeRef::Path { def: Some(def), .. } => match def {
                | TypeDefId::TypeVarId(id) => Ty::new(self.db, TyKind::Var(*id)),
                | TypeDefId::TypeCtorId(id) => Ty::new(self.db, TyKind::Ctor(*id)),
                | _ => todo!(),
            },
            | TypeRef::App { base, args } => {
                let base = self.lower_type_ref(*base);
                let args = args.iter().map(|&t| self.lower_type_ref(t)).collect();

                Ty::new(self.db, TyKind::App(base, args))
            },
            | TypeRef::Func { args, ret } => {
                let params = args.iter().map(|&t| self.lower_type_ref(t)).collect();
                let ret = self.lower_type_ref(*ret);
                let env = self.unit_type();
                let variadic = false;

                Ty::new(
                    self.db,
                    TyKind::Func(FuncType {
                        params,
                        ret,
                        env,
                        variadic,
                    }),
                )
            },
            | TypeRef::Where { clause: _, ty } => {
                // TODO: lower constraints
                self.lower_type_ref(*ty)
            },
        }
    }
}

impl<'db> std::ops::Deref for LowerCtx<'db, '_> {
    type Target = Ctx<'db>;

    fn deref(&self) -> &Self::Target {
        self.ctx
    }
}

impl<'db> std::ops::DerefMut for LowerCtx<'db, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ctx
    }
}

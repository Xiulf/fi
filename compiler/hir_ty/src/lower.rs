use hir_def::id::TypeDefId;
use hir_def::type_ref::{TypeMap, TypeRef, TypeRefId};
use triomphe::Arc;

use crate::ctx::Ctx;
use crate::ty::{FuncType, GeneralizedType, PrimitiveType, Ty, TyKind};

pub struct LowerCtx<'db, 'ctx> {
    pub(crate) ctx: &'ctx mut Ctx<'db>,
    type_map: Arc<TypeMap>,
}

impl<'db, 'ctx> LowerCtx<'db, 'ctx> {
    pub fn new(ctx: &'ctx mut Ctx<'db>, type_map: Arc<TypeMap>) -> Self {
        Self { ctx, type_map }
    }

    pub fn lower_type_ref(&mut self, id: TypeRefId, top_level: bool) -> Ty {
        let ty = self.lower_type_ref_inner(id, top_level);

        // TODO: set kind
        ty
    }

    fn lower_type_ref_inner(&mut self, id: TypeRefId, top_level: bool) -> Ty {
        let type_map = self.type_map.clone();

        match type_map[id] {
            | TypeRef::Missing => Ty::new(self.db, TyKind::Error),
            | TypeRef::Hole => self.ctx.fresh_type(self.level, false),
            | TypeRef::Path { def: None, .. } => Ty::new(self.db, TyKind::Error),
            | TypeRef::Path { def: Some(def), .. } => match def {
                | TypeDefId::TypeVarId(id) => Ty::new(self.db, TyKind::Var(id)),
                | TypeDefId::TypeCtorId(id) => {
                    if let Some(kind) = self.ctor_int_kind(id) {
                        let int = self.int_type();
                        let tag = Ty::new(self.db, TyKind::Primitive(PrimitiveType::Integer(kind)));
                        return Ty::new(self.db, TyKind::App(int, Box::new([tag])));
                    }

                    if let Some(kind) = self.ctor_float_kind(id) {
                        let float = self.float_type();
                        let tag = Ty::new(self.db, TyKind::Primitive(PrimitiveType::Float(kind)));
                        return Ty::new(self.db, TyKind::App(float, Box::new([tag])));
                    }

                    Ty::new(self.db, TyKind::Ctor(id))
                },
                | TypeDefId::TypeAliasId(id) => match crate::alias_ty(self.db, id) {
                    | GeneralizedType::Mono(ty) => ty,
                    | GeneralizedType::Poly(_, ty) => ty, // @TODO: report error
                },
                | _ => todo!(),
            },
            | TypeRef::App { base, ref args } => match type_map[base] {
                | TypeRef::Path {
                    def: Some(TypeDefId::TypeAliasId(def)),
                    ..
                } => {
                    let args = args.iter().map(|&t| self.lower_type_ref(t, false)).collect();

                    match crate::alias_ty(self.db, def) {
                        | GeneralizedType::Mono(ty) => Ty::new(self.db, TyKind::App(ty, args)),
                        | GeneralizedType::Poly(vars, ty) => {
                            let replacements = vars.iter().zip(args.iter()).map(|(&v, &a)| (v, a)).collect();
                            ty.replace_vars(self.db, &replacements)
                        },
                    }
                },
                | _ => {
                    let base = self.lower_type_ref(base, false);
                    let args = args.iter().map(|&t| self.lower_type_ref(t, false)).collect();
                    Ty::new(self.db, TyKind::App(base, args))
                },
            },
            | TypeRef::Func { env, ref args, ret } => {
                let params = args.iter().map(|&t| self.lower_type_ref(t, false)).collect();
                let ret = self.lower_type_ref(ret, false);
                let env = if let Some(env) = env {
                    self.lower_type_ref(env, false)
                } else if top_level {
                    self.unit_type()
                } else {
                    self.ctx.fresh_type(self.level, false)
                };

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
                self.lower_type_ref(ty, false)
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

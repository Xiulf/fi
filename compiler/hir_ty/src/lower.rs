use hir_def::id::TypeDefId;
use hir_def::lang_item;
use hir_def::type_ref::{TypeMap, TypeRef, TypeRefId};
use triomphe::Arc;

use crate::ctx::Ctx;
use crate::ty::{Constraint, FuncType, GeneralizedType, PrimitiveType, Ty, TyKind};

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
            | TypeRef::Lit { ref lit } => Ty::new(self.db, TyKind::Literal(lit.clone())),
            | TypeRef::Path { def: None, .. } => Ty::new(self.db, TyKind::Error),
            | TypeRef::Path { def: Some(def), .. } => {
                let (ty, ret) = self.lower_type_def_id(def);
                if ret {
                    return ty;
                }

                ty
            },
            | TypeRef::Ref { ty } => {
                let lifetime = self.fresh_unknown();
                let ty = self.lower_type_ref(ty, false);
                Ty::new(self.db, TyKind::Ref(lifetime, ty))
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
            | TypeRef::Func {
                env,
                ref args,
                ret,
                is_varargs,
            } => {
                let params = args.iter().map(|&t| self.lower_type_ref(t, false)).collect();
                let ret = self.lower_type_ref(ret, false);
                let env = if let Some(env) = env {
                    self.lower_type_ref(env, false)
                } else if top_level {
                    self.unit_type()
                } else {
                    self.ctx.fresh_type(self.level, false)
                };

                Ty::new(
                    self.db,
                    TyKind::Func(FuncType {
                        params,
                        ret,
                        env,
                        is_varargs,
                    }),
                )
            },
        }
    }

    fn lower_type_def_id(&mut self, def: TypeDefId) -> (Ty, bool) {
        match def {
            | TypeDefId::TypeVarId(id) => (Ty::new(self.db, TyKind::Var(id)), false),
            | TypeDefId::TypeCtorId(id) => {
                if self.lang_ctor(lang_item::NEVER_TYPE) == Some(id) {
                    return (Ty::new(self.db, TyKind::Never), false);
                }

                if let Some(kind) = self.ctor_int_kind(id) {
                    let int = self.int_type();
                    let tag = Ty::new(self.db, TyKind::Primitive(PrimitiveType::Integer(kind)));
                    return (Ty::new(self.db, TyKind::App(int, Box::new([tag]))), true);
                }

                if let Some(kind) = self.ctor_float_kind(id) {
                    let float = self.float_type();
                    let tag = Ty::new(self.db, TyKind::Primitive(PrimitiveType::Float(kind)));
                    return (Ty::new(self.db, TyKind::App(float, Box::new([tag]))), true);
                }

                (Ty::new(self.db, TyKind::Ctor(id)), false)
            },
            | TypeDefId::TypeAliasId(id) => match crate::alias_ty(self.db, id) {
                | GeneralizedType::Mono(ty) => (ty, false),
                | GeneralizedType::Poly(_, ty) => (ty, false), // @TODO: report error
            },
            | TypeDefId::FixityId(id) => {
                let data = hir_def::data::fixity_data(self.db, id);

                match data.def(self.db) {
                    | Some(def) => self.lower_type_def_id(def.unwrap_right()),
                    | None => (self.error(), false),
                }
            },
            | _ => todo!(),
        }
    }

    pub fn lower_constraint(&mut self, constraint: &hir_def::type_ref::Constraint) -> Option<Constraint> {
        Some(Constraint {
            trait_id: constraint.trait_id?,
            args: constraint.args.iter().map(|&a| self.lower_type_ref(a, false)).collect(),
        })
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

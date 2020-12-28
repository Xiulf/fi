use crate::error::TypeError;
use crate::ty::*;
use hir::ir;
use std::cmp::Ordering;
use std::collections::HashMap;

pub struct Ctx<'db> {
    crate db: &'db dyn crate::TypeDatabase,
    crate next_ty: u64,
    crate next_skolem: u64,
    crate next_scope: u64,
    crate subst: Substitution,
    crate tys: HashMap<ir::HirId, Ty>,
    crate errors: Vec<TypeError>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct UnkLevel(Vec<Unknown>);

pub struct Substitution {
    pub tys: HashMap<Unknown, Ty>,
    pub unsolved: HashMap<UnkLevel, Ty>,
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn crate::TypeDatabase) -> Self {
        Ctx {
            db,
            next_ty: 0,
            next_skolem: 0,
            next_scope: 0,
            subst: Substitution::empty(),
            tys: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> HashMap<ir::HirId, Ty> {
        self.tys
    }

    crate fn is_func(&self, ty: &Ty) -> bool {
        if let Type::Ctor(id) = **ty {
            id == self.db.lang_items().fn_ty().owner
        } else {
            false
        }
    }

    crate fn is_record(&self, ty: &Ty) -> bool {
        if let Type::Ctor(id) = **ty {
            id == self.db.lang_items().record_ty().owner
        } else {
            false
        }
    }

    crate fn ty_kind(&self, span: ir::Span) -> Ty {
        let ty_kind = self.db.lang_items().kind_type();

        Ty::ctor(span, ty_kind.owner)
    }

    crate fn row_kind(&self, span: ir::Span) -> Ty {
        let row_kind = self.db.lang_items().kind_row();

        Ty::ctor(span, row_kind.owner)
    }

    crate fn record_ty(&self, span: ir::Span) -> Ty {
        let record_ty = self.db.lang_items().record_ty();

        Ty::ctor(span, record_ty.owner)
    }

    crate fn array_ty(&self, span: ir::Span) -> Ty {
        let arr_ty = self.db.lang_items().array_ty();

        Ty::ctor(span, arr_ty.owner)
    }

    crate fn instantiate(&mut self, ty: Ty) -> Ty {
        if let Type::ForAll(vars, ret, _) = &*ty {
            let subst = vars
                .into_iter()
                .map(|(v, k)| {
                    if let Some(k) = k {
                        (v, self.fresh_type_with_kind(ty.span(), k))
                    } else {
                        (v, self.fresh_type(ty.span()))
                    }
                })
                .collect();

            ret.clone().replace_vars(subst)
        } else if let Type::Ctnt(ctnt, ret) = &*ty {
            todo!();
            // self.instantiate(ret.clone())
        } else {
            ty
        }
    }

    crate fn hir_ty(&mut self, ty: &ir::Type) -> Ty {
        match &ty.kind {
            ir::TypeKind::Error => Ty::error(ty.span),
            ir::TypeKind::Int { val } => Ty::int(ty.span, *val),
            ir::TypeKind::Func { params, ret } => {
                let func_ty = self.db.lang_items().fn_ty();
                let func_ty = Ty::ctor(ty.span, func_ty.owner);
                let params = Ty::tuple(ty.span, params.iter().map(|t| self.hir_ty(t)));
                let ret = self.hir_ty(ret);

                Ty::app(ty.span, func_ty, List::from([params, ret]))
            }
            ir::TypeKind::Infer => self.fresh_type_with_kind(ty.span, self.ty_kind(ty.span)),
            ir::TypeKind::App { base, args } => {
                let base = self.hir_ty(base);
                let args = args.iter().map(|a| self.hir_ty(a));

                Ty::app(ty.span, base, args)
            }
            ir::TypeKind::Ident { res } => match res {
                ir::Res::Error => Ty::error(ty.span),
                ir::Res::Def(ir::DefKind::Data, id) => Ty::ctor(ty.span, *id),
                ir::Res::Def(ir::DefKind::Alias, id) => self.db.typecheck(*id).ty.clone(),
                ir::Res::Def(_, _) => unreachable!(),
                ir::Res::Local(id) => Ty::var(ty.span, TypeVar(*id)),
            },
            _ => unimplemented!("hir_ty {:?}", ty),
        }
    }
}

impl Substitution {
    pub fn empty() -> Self {
        Substitution {
            tys: HashMap::new(),
            unsolved: HashMap::new(),
        }
    }
}

impl PartialOrd for UnkLevel {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.0.is_empty() && other.0.is_empty() {
            Some(Ordering::Equal)
        } else if other.0.is_empty() {
            Some(Ordering::Less)
        } else if self.0.is_empty() {
            Some(Ordering::Greater)
        } else {
            self.0.partial_cmp(&other.0)
        }
    }
}

impl Ord for UnkLevel {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl From<Unknown> for UnkLevel {
    fn from(src: Unknown) -> Self {
        UnkLevel(vec![src])
    }
}

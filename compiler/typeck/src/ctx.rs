use crate::error::TypeError;
use crate::ty::*;
use hir::ir;
use std::cmp::Ordering;
use std::collections::HashMap;

pub struct Ctx<'db> {
    crate db: &'db dyn crate::TypeDatabase,
    crate file: source::FileId,
    crate module: ir::ModuleId,
    crate next_ty: u64,
    crate next_skolem: u64,
    crate next_scope: u64,
    crate subst: Substitution,
    crate tys: HashMap<ir::HirId, Ty>,
    crate bounds: HashMap<ir::HirId, crate::BoundInfo>,
    crate ctnt_ctx: Vec<Ctnt>,
    crate ctnts: Vec<(ir::HirId, Ctnt, Vec<Ctnt>)>,
    crate errors: Vec<TypeError>,
    crate module_types: crate::ModuleTypes,
    crate all_instances: HashMap<ir::DefId, std::sync::Arc<Vec<Instance>>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnkLevel(crate Vec<Unknown>);

pub struct Substitution {
    pub tys: HashMap<Unknown, Ty>,
    pub unsolved: HashMap<Unknown, (UnkLevel, Ty)>,
}

impl<'db> Ctx<'db> {
    pub fn new(db: &'db dyn crate::TypeDatabase, file: source::FileId, module: ir::ModuleId) -> Self {
        Ctx {
            db,
            file,
            module,
            next_ty: 0,
            next_skolem: 0,
            next_scope: 0,
            subst: Substitution::empty(),
            tys: HashMap::new(),
            bounds: HashMap::new(),
            ctnt_ctx: Vec::new(),
            ctnts: Vec::new(),
            errors: Vec::new(),
            module_types: crate::ModuleTypes {
                items: HashMap::new(),
                variants: HashMap::new(),
                classes: HashMap::new(),
                instances: HashMap::new(),
            },
            all_instances: HashMap::new(),
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

    crate fn ty_kind(&self, span: ir::Span, file: source::FileId) -> Ty {
        let ty_kind = self.db.lang_items().kind_type();

        Ty::ctor(span, file, ty_kind.owner)
    }

    crate fn row_kind(&self, span: ir::Span, file: source::FileId) -> Ty {
        let row_kind = self.db.lang_items().kind_row();

        Ty::ctor(span, file, row_kind.owner)
    }

    crate fn symbol_kind(&self, span: ir::Span, file: source::FileId) -> Ty {
        let symbol_kind = self.db.lang_items().kind_symbol();

        Ty::ctor(span, file, symbol_kind.owner)
    }

    crate fn figure_kind(&self, span: ir::Span, file: source::FileId) -> Ty {
        let figure_kind = self.db.lang_items().kind_figure();

        Ty::ctor(span, file, figure_kind.owner)
    }

    crate fn record_ty(&self, span: ir::Span, file: source::FileId) -> Ty {
        let record_ty = self.db.lang_items().record_ty();

        Ty::ctor(span, file, record_ty.owner)
    }

    crate fn array_ty(&self, span: ir::Span, file: source::FileId) -> Ty {
        let arr_ty = self.db.lang_items().array_ty();

        Ty::ctor(span, file, arr_ty.owner)
    }

    crate fn func_ty(&self, span: ir::Span, file: source::FileId) -> Ty {
        let func_ty = self.db.lang_items().fn_ty();

        Ty::ctor(span, file, func_ty.owner)
    }

    pub fn args(&self, mut ret: Ty) -> (List<Ty>, Ty) {
        let mut args = Vec::new();

        while let Type::App(b, r) = &*ret {
            match &**b {
                | Type::App(f, a) if self.is_func(f) => {
                    args.push(a.clone());
                    ret = r.clone();
                },
                | _ => break,
            }
        }

        (args.into(), ret)
    }

    pub fn finalize(&mut self) {
        for mut e in std::mem::replace(&mut self.errors, Vec::new()) {
            if let TypeError::HoleType(name, ty) = e {
                let loc = ty.loc();
                e = TypeError::HoleType(name, self.subst_type(ty) ^ loc);
            }

            e.report(self.db);
        }

        let mut tys = std::mem::replace(&mut self.tys, HashMap::new());

        tys.values_mut().for_each(|t| {
            let ty = t.clone();
            let ty = self.subst_type(ty);
            let ty = crate::skolem::unskolemize(ty);

            *t = ty;
        });

        self.tys = tys;
    }

    crate fn instantiate(&mut self, id: ir::HirId, ty: Ty) -> Ty {
        if let Type::ForAll(var, kind, ret, _) = &*ty {
            let mut subst = HashMap::new();
            let ty = match kind {
                | Some(k) => self.fresh_type_with_kind(ty.span(), ty.file(), k.clone()),
                | None => self.fresh_type(ty.span(), ty.file()),
            };

            subst.insert(*var, ty);
            self.instantiate(id, ret.clone().replace_vars(subst))
        } else if let Type::Ctnt(ctnt, ret) = &*ty {
            self.ctnts.push((id, ctnt.clone(), self.ctnt_ctx.clone()));
            self.instantiate(id, ret.clone())
        } else {
            ty
        }
    }

    crate fn constrain(&self, unsolved: Vec<Ctnt>, ty: Ty) -> Ty {
        let span = ty.span();
        let file = ty.file();

        unsolved.into_iter().rfold(ty, |ret, ctnt| Ty::ctnt(span, file, ctnt, ret))
    }

    crate fn generalize(&mut self, ty: Ty, def: ir::DefId) -> Ty {
        let uk = ty.unknowns();

        if uk.is_empty() {
            ty
        } else {
            let mut repl = HashMap::with_capacity(uk.len());
            let vars = uk
                .into_iter()
                .map(|u| {
                    let kind = self.subst.unsolved[&u].1.clone();
                    let var = TypeVar(ir::HirId {
                        owner: def,
                        local_id: ir::LocalId(u32::max_value() - u.0 as u32),
                    });

                    let var_ty = Ty::var(ty.span(), ty.file(), var);

                    self.subst.tys.insert(u, var_ty.clone());
                    repl.insert(u, var_ty);

                    (var, Some(kind))
                })
                .collect::<Vec<_>>();

            let mut ty = ty.everywhere(&mut |t| match *t {
                | Type::Unknown(u) => match repl.get(&u) {
                    | None => t,
                    | Some(t2) => t2.clone(),
                },
                | _ => t,
            });

            for (v, k) in vars.into_iter().rev() {
                ty = Ty::forall(ty.span(), ty.file(), v, k, ty, None);
            }

            ty
        }
    }

    crate fn hir_ty(&mut self, ty: &ir::Type) -> Ty {
        match &ty.kind {
            | ir::TypeKind::Error => Ty::error(ty.span, self.file),
            | ir::TypeKind::Int { val } => Ty::int(ty.span, self.file, *val),
            | ir::TypeKind::Str { val } => Ty::string(ty.span, self.file, val.clone()),
            | ir::TypeKind::Func { param, ret } => {
                let func_ty = self.func_ty(ty.span, self.file);
                let param = self.hir_ty(param);
                let ret = self.hir_ty(ret);

                Ty::app(ty.span, self.file, Ty::app(ty.span, self.file, func_ty, param), ret)
            },
            | ir::TypeKind::Infer => self.fresh_type_with_kind(ty.span, self.file, self.ty_kind(ty.span, self.file)),
            | ir::TypeKind::App { base, arg } => {
                let base = self.hir_ty(base);
                let arg = self.hir_ty(arg);

                Ty::app(ty.span, self.file, base, arg)
            },
            | ir::TypeKind::Ident { res } => match res {
                | ir::Res::Error => Ty::error(ty.span, self.file),
                | ir::Res::Def(ir::DefKind::Data, id) => Ty::ctor(ty.span, self.file, *id),
                | ir::Res::Def(ir::DefKind::Alias, id) => self.typeck_def(*id),
                | ir::Res::Def(_, _) => unreachable!(),
                | ir::Res::Local(id) => Ty::var(ty.span, self.file, TypeVar(*id)),
            },
            | ir::TypeKind::Tuple { tys } => {
                let file = self.file;
                let tys = tys.iter().map(|t| self.hir_ty(t));

                Ty::tuple(ty.span, file, tys)
            },
            | ir::TypeKind::Record { row } => {
                let fields = row
                    .fields
                    .iter()
                    .map(|f| Field {
                        span: f.span,
                        name: f.name.symbol,
                        ty: self.hir_ty(&f.ty),
                    })
                    .collect::<List<_>>();

                let tail = row.tail.as_ref().map(|t| self.hir_ty(t));
                let row = Ty::row(row.span, self.file, fields, tail);
                let record_ty = self.record_ty(ty.span, self.file);

                Ty::app(ty.span, self.file, record_ty, row)
            },
            | ir::TypeKind::Forall { vars, ty: ret } => {
                let mut ret = self.hir_ty(ret);
                let vars = vars.iter().map(|v| (TypeVar(v.id), Some(self.hir_ty(&v.kind)))).collect::<Vec<_>>();

                for (v, k) in vars.into_iter().rev() {
                    ret = Ty::forall(ty.span, self.file, v, k, ret, None);
                }

                ret
            },
            | ir::TypeKind::Cons { cs, ty: ret } => {
                let ctnt = Ctnt {
                    span: cs.span,
                    file: self.file,
                    class: cs.class,
                    tys: cs.tys.iter().map(|t| self.hir_ty(t)).collect(),
                };

                let ret = self.hir_ty(ret);

                Ty::ctnt(ty.span, self.file, ctnt, ret)
            },
            | ir::TypeKind::Hole { name } => {
                let ty = self.fresh_type(ty.span, self.file);

                self.errors.push(TypeError::HoleType(*name, ty.clone()));

                ty
            },
            | ir::TypeKind::Kinded { ty: ret, kind } => {
                let ret = self.hir_ty(ret);
                let kind = self.hir_ty(kind);

                match self.check_kind(ret, kind) {
                    | Ok(v) => v,
                    | Err(e) => {
                        e.report(self.db);
                        Ty::error(ty.span, self.file)
                    },
                }
            },
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

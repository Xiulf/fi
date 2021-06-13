pub mod builder;

use crate::db::MirDatabase;
use crate::instance_record::InstanceRecord;
use crate::ir::*;
use crate::ty::{self, *};
use builder::Builder;
use hir::display::HirDisplay as _;
use hir::id::HasModule as _;
use hir::ty::{Ty, TyKind, TypeVar};
use hir::MethodSource;
use hir_def::resolver::{HasResolver, Resolver, ValueNs};
use rustc_hash::FxHashMap;
use std::sync::Arc;

impl Bodies {
    pub(crate) fn body_mir_query(db: &dyn MirDatabase, def: hir::id::DefWithBodyId) -> Arc<Self> {
        let body = db.body(def);
        let mut ty = match def {
            | hir::id::DefWithBodyId::FuncId(id) => db.value_ty(id.into()),
            | hir::id::DefWithBodyId::StaticId(id) => db.value_ty(id.into()),
            | hir::id::DefWithBodyId::ConstId(id) => db.value_ty(id.into()),
        };

        let mut args = Vec::new();
        let mut vars = Vec::new();
        let mut ctnts = Vec::new();
        let func_id = db.lang_item(def.module(db.upcast()).lib, "fn-type".into()).unwrap();
        let func_id = func_id.as_type_ctor().unwrap();

        while let TyKind::ForAll(var, ret) = ty.lookup(db.upcast()) {
            vars.push(var);
            ty = ret;
        }

        while let TyKind::Ctnt(ctnt, ret) = ty.lookup(db.upcast()) {
            ctnts.push(ctnt);
            ty = ret;
        }

        if !def.has_body(db.upcast()) {
            while let Some([arg, ret]) = ty.match_ctor(db.upcast(), func_id) {
                args.push(arg);
                ty = ret;
            }
        } else {
            while let Some([arg, ret]) = ty.match_ctor(db.upcast(), func_id) {
                if args.len() == body.params().len() {
                    break;
                }

                args.push(arg);
                ty = ret;
            }
        }

        let mut lcx = LowerCtx::new(db, def);
        let lib = def.module(db.upcast()).lib;
        let type_kind = db.lang_item(lib, "type-kind".into()).unwrap();
        let type_kind = type_kind.as_type_ctor().unwrap();
        let figure_kind = db.lang_item(lib, "figure-kind".into()).unwrap();
        let figure_kind = figure_kind.as_type_ctor().unwrap();
        let symbol_kind = db.lang_item(lib, "symbol-kind".into()).unwrap();
        let symbol_kind = symbol_kind.as_type_ctor().unwrap();

        for var in vars {
            let kind = var.lookup(db.upcast());

            if TyKind::Ctor(type_kind) == kind {
                lcx.add_type_var(Some(TypeVarKind::Type));
            } else if TyKind::Ctor(figure_kind) == kind {
                lcx.add_type_var(Some(TypeVarKind::Figure));
            } else if TyKind::Ctor(symbol_kind) == kind {
                lcx.add_type_var(Some(TypeVarKind::Symbol));
            } else {
                lcx.add_type_var(None);
            }
        }

        for ctnt in ctnts {
            lcx.add_instance_record(ctnt.class);
        }

        lcx.lower(ty, args);

        Arc::new(lcx.finish())
    }
}

struct LowerCtx<'a> {
    db: &'a dyn MirDatabase,
    def: hir::id::DefWithBodyId,
    hir: Arc<hir::Body>,
    infer: Arc<hir::InferenceResult>,
    type_vars: Vec<Option<TypeVarKind>>,
    instance_records: Vec<Arc<InstanceRecord>>,
    bodies: Bodies,
}

struct BodyLowerCtx<'a> {
    db: &'a dyn MirDatabase,
    def: hir::id::DefWithBodyId,
    hir: &'a Arc<hir::Body>,
    infer: &'a Arc<hir::InferenceResult>,
    builder: Builder<'a>,
    ret: LocalId,
    binders: FxHashMap<hir::PatId, Place>,
}

impl<'a> LowerCtx<'a> {
    fn new(db: &'a dyn MirDatabase, def: hir::id::DefWithBodyId) -> Self {
        Self {
            db,
            def,
            bodies: Bodies::default(),
            hir: db.body(def),
            infer: db.infer(def),
            type_vars: Vec::new(),
            instance_records: Vec::new(),
        }
    }

    fn finish(self) -> Bodies {
        self.bodies
    }

    fn add_type_var(&mut self, kind: Option<TypeVarKind>) {
        self.type_vars.push(kind);
    }

    fn add_instance_record(&mut self, id: hir::id::ClassId) {
        let record = self.db.instance_record(id.into());

        self.instance_records.push(record);
    }

    fn lower(&mut self, ret_ty: Ty, args: Vec<Ty>) {
        let local_id = self.bodies.add(self.type_vars.clone(), self.instance_records.clone());

        self.bodies.set_arity(local_id, args.len());

        if self.def.has_body(self.db.upcast()) && args.len() > 1 {
            let params = args.iter().map(|&t| self.db.mir_type(t)).collect();
            let ret = self.db.mir_type(ret_ty);
            let ty = Type::func(params, ret);

            self.generate_curry(ret_ty, &args, args.len() - 1, local_id, ty);
        }

        let mut builder = self.bodies.builder(local_id);
        let ret = builder.create_ret(self.db.mir_type(ret_ty));
        let mut bcx = BodyLowerCtx {
            db: self.db,
            def: self.def,
            hir: &self.hir,
            infer: &self.infer,
            builder,
            ret,
            binders: FxHashMap::default(),
        };

        if !bcx.def.has_body(bcx.db.upcast()) {
            for arg in args {
                let ty = bcx.db.mir_type(arg);

                bcx.builder.create_arg(ty);
            }
        }

        bcx.lower();

        eprintln!("{}", self.bodies.display(self.db.upcast()));
    }

    fn generate_curry(&mut self, ret_ty: Ty, arg_tys: &[Ty], arity: usize, parent: LocalBodyId, parent_ty: Arc<Type>) {
        let db = self.db;
        let local_id = self.bodies.add(self.type_vars.clone(), self.instance_records.clone());
        let (clos_id, clos_ret_ty, clos_env_ty) =
            self.generate_curry_closure(ret_ty, arg_tys, arity, parent, parent_ty);
        let mut builder = self.bodies.builder(local_id);
        let ret = builder.create_ret(clos_ret_ty.clone());
        let ret = Place::new(ret);
        let args = arg_tys[..arity]
            .iter()
            .map(|&t| {
                let ty = db.mir_type(t);

                builder.create_arg(ty)
            })
            .collect::<Vec<_>>();

        let entry = builder.create_block();

        builder.set_block(entry);
        builder.alloc(self.db, ret.clone().field(0), clos_env_ty);
        builder.use_op(
            ret.clone().field(1),
            Operand::Const(
                Const::Addr(BodyId {
                    def: self.def,
                    local_id: clos_id,
                }),
                clos_ret_ty,
            ),
        );

        let env = ret.clone().field(0).deref();

        for (i, arg) in args.into_iter().enumerate() {
            let place = env.clone().field(i);
            let arg = Place::new(arg);

            builder.use_op(place, Operand::Place(arg));
        }

        builder.ret();
        self.bodies.set_arity(local_id, arity);

        if arity > 1 {
            let ty = self.bodies.signature(local_id);

            self.generate_curry(ret_ty, arg_tys, arity - 1, local_id, ty);
        }
    }

    fn generate_curry_closure(
        &mut self,
        ret_ty: Ty,
        arg_tys: &[Ty],
        arity: usize,
        parent: LocalBodyId,
        parent_ty: Arc<Type>,
    ) -> (LocalBodyId, Arc<Type>, Arc<Type>) {
        let env = arg_tys[..arity].iter().map(|&t| self.db.mir_type(t));
        let env_ty = Type::and(env);
        let env = Type::ref_(env_ty.clone());
        let arg_tys = arg_tys[arity..]
            .iter()
            .map(|&t| self.db.mir_type(t))
            .collect::<Vec<_>>();
        let mut ret_ty = self.db.mir_type(ret_ty);

        for ty in arg_tys[1..].iter().rev() {
            ret_ty = Type::closure(ty.clone(), ret_ty, None);
        }

        let local_id = self.bodies.add(Vec::new(), Vec::new());
        let mut builder = self.bodies.builder(local_id);
        let ret = builder.create_ret(ret_ty.clone());
        let ret = Place::new(ret);

        let env = builder.create_arg(env);
        let env = Place::new(env);

        let arg = builder.create_arg(arg_tys[0].clone());
        let arg = Place::new(arg);

        let entry = builder.create_block();

        builder.set_block(entry);

        let parent = Const::Addr(BodyId {
            def: self.def,
            local_id: parent,
        });

        let args = (0..arity)
            .map(|i| env.clone().deref().field(i))
            .map(Operand::Place)
            .chain(Some(Operand::Place(arg)))
            .collect::<Vec<_>>();

        builder.call(ret, Operand::Const(parent, parent_ty), args);
        builder.ret();

        (
            local_id,
            Type::closure(arg_tys[0].clone(), ret_ty, Some(env_ty.clone())),
            env_ty,
        )
    }
}

impl<'a> BodyLowerCtx<'a> {
    fn lower(&mut self) {
        let entry = self.builder.create_block();

        self.builder.set_block(entry);

        for param in self.hir.params().to_vec() {
            let ty = self.infer.type_of_pat[param];
            let ty = self.db.mir_type(ty);
            let arg = self.builder.create_arg(ty);
            let place = Place::new(arg);

            self.lower_pat(param, place);
        }

        let ret = Place::new(self.ret);

        self.lower_expr(self.hir.body_expr(), Some(ret));
        self.builder.ret();
    }

    fn lower_pat(&mut self, id: hir::PatId, place: Place) {
        let body = Arc::clone(self.hir);
        let ty = self.infer.type_of_pat[id];

        match body[id] {
            | hir::Pat::Missing => {},
            | hir::Pat::Wildcard => {},
            | hir::Pat::Typed { pat, .. } => self.lower_pat(pat, place),
            | hir::Pat::Bind { subpat, .. } => {
                self.binders.insert(id, place.clone());

                if let Some(subpat) = subpat {
                    self.lower_pat(subpat, place);
                }
            },
            | hir::Pat::Tuple { ref pats } => {
                for (i, &pat) in pats.iter().enumerate() {
                    self.lower_pat(pat, place.clone().field(i));
                }
            },
            | hir::Pat::Record { ref fields, has_rest } => {
                if let TyKind::App(_, row) = ty.lookup(self.db.upcast()) {
                    if let TyKind::Row(ty_fields, tail) = row.lookup(self.db.upcast()) {
                        if has_rest {
                            let record = place.clone().field(0).deref();
                            let offsets = place.field(1).deref();
                            let uint_lyt = Type::ptr_sized_int(self.db, false);

                            for field in fields {
                                let idx = ty_fields.iter().position(|f| f.name == field.name).unwrap();
                                let idx = Operand::Const(Const::Scalar(idx as u128), uint_lyt.clone());
                                let offset = Operand::Place(offsets.clone().index(idx));

                                self.lower_pat(field.val, record.clone().offset(offset));
                            }
                        } else {
                            for field in fields {
                                let idx = ty_fields.iter().position(|f| f.name == field.name).unwrap();

                                self.lower_pat(field.val, place.clone().field(idx));
                            }
                        }
                    }
                }
            },
            | _ => unimplemented!(),
        }
    }

    fn lower_expr(&mut self, id: hir::ExprId, mut ret: Option<Place>) -> Operand {
        let op = self.lower_expr_impl(id, &mut ret);

        if let Some(ret) = ret {
            self.builder.use_op(ret.clone(), op);

            Operand::Place(ret)
        } else {
            op
        }
    }

    fn lower_expr_impl(&mut self, id: hir::ExprId, ret: &mut Option<Place>) -> Operand {
        let body = Arc::clone(&self.hir);
        let hir_ty = self.infer.type_of_expr[id];
        let ty = self.db.mir_type(hir_ty);

        // eprintln!("{:?}: {:?} :: {}", id, body[id], hir_ty.display(self.db.upcast()));

        match body[id] {
            | hir::Expr::Missing => Operand::Const(Const::Undefined, ty),
            | hir::Expr::Typed { expr, .. } => self.lower_expr_impl(expr, ret),
            | hir::Expr::Path { ref path } => self.lower_path(id, path, hir_ty, ret),
            | hir::Expr::Lit { ref lit } => match *lit {
                | hir::Literal::Int(i) => Operand::Const(Const::Scalar(i as u128), ty),
                | hir::Literal::Float(i) => Operand::Const(Const::Scalar(i as u128), ty),
                | hir::Literal::Char(i) => Operand::Const(Const::Scalar(i as u128), ty),
                | hir::Literal::String(ref s) => Operand::Const(Const::String(s.clone()), ty),
            },
            | hir::Expr::Infix { ref op, lhs, rhs } => {
                let resolver = Resolver::for_expr(self.db.upcast(), self.def, id);

                if let Some(ValueNs::Fixity(f)) = resolver.resolve_value_fully(self.db.upcast(), op) {
                    let fixity = self.db.fixity_data(f);
                    let resolver = f.resolver(self.db.upcast());

                    match resolver.resolve_value_fully(self.db.upcast(), &fixity.func) {
                        | Some(ValueNs::Func(mut f)) => {
                            if let Some(method) = self.infer.methods.get(&id) {
                                match method {
                                    | MethodSource::Instance(inst) => {
                                        let data = self.db.instance_data(*inst);
                                        let item = data.item(fixity.func.segments().last().unwrap()).unwrap();

                                        f = match item {
                                            | hir::id::AssocItemId::FuncId(id) => id,
                                            | _ => unreachable!(),
                                        };
                                    },
                                    | MethodSource::Record(_) => unimplemented!(),
                                }
                            }

                            self.lower_func_app(f, id, vec![lhs, rhs], hir_ty, ret.take())
                        },
                        | Some(ValueNs::Ctor(c)) => self.lower_ctor_app(c, vec![lhs, rhs], hir_ty, ret.take()),
                        | _ => Operand::Const(Const::Undefined, ty),
                    }
                } else {
                    Operand::Const(Const::Undefined, ty)
                }
            },
            | hir::Expr::App { .. } => self.lower_app(id, hir_ty, ret.take()),
            | hir::Expr::Tuple { ref exprs } => {
                let ret = ret.take().unwrap_or_else(|| Place::new(self.builder.create_var(ty)));

                for (i, &expr) in exprs.iter().enumerate() {
                    self.lower_expr(expr, Some(ret.clone().field(i)));
                }

                Operand::Place(ret)
            },
            | hir::Expr::Record { ref fields } => {
                let row_fields = match hir_ty.lookup(self.db.upcast()) {
                    | TyKind::App(_, row) => match row.lookup(self.db.upcast()) {
                        | TyKind::Row(row_fields, _) => row_fields,
                        | _ => unreachable!(),
                    },
                    | _ => unreachable!(),
                };

                let ret = ret.take().unwrap_or_else(|| Place::new(self.builder.create_var(ty)));

                for field in fields {
                    let idx = row_fields.iter().position(|f| f.name == field.name).unwrap();

                    self.lower_expr(field.val, Some(ret.clone().field(idx)));
                }

                Operand::Place(ret)
            },
            | hir::Expr::Do { ref stmts } => {
                let last = stmts.len() - 1;

                for (i, &stmt) in stmts.iter().enumerate() {
                    match stmt {
                        | hir::Stmt::Expr { expr } => {
                            if i == last {
                                return self.lower_expr(expr, ret.take());
                            } else {
                                self.lower_expr(expr, None);
                            }
                        },
                        | hir::Stmt::Bind { pat, val } | hir::Stmt::Let { pat, val } => {
                            let hir_ty = self.infer.type_of_expr[val];
                            let ty = self.db.mir_type(hir_ty);
                            let place = self.builder.create_var(ty);
                            let place = Place::new(place);

                            self.lower_expr(val, Some(place.clone()));
                            self.lower_pat(pat, place);
                        },
                    }
                }

                Operand::Const(Const::Tuple(Vec::new()), ty)
            },
            | ref e => unimplemented!("{:?}", e),
        }
    }

    fn lower_path(&mut self, expr: hir::ExprId, path: &hir::Path, mut hir_ty: Ty, ret: &mut Option<Place>) -> Operand {
        let resolver = Resolver::for_expr(self.db.upcast(), self.def, expr);

        match resolver.resolve_value_fully(self.db.upcast(), path) {
            | Some(ValueNs::Func(mut id)) => {
                if let Some(method) = self.infer.methods.get(&expr) {
                    match method {
                        | MethodSource::Instance(inst) => {
                            let data = self.db.instance_data(*inst);
                            let item = data.item(path.segments().last().unwrap()).unwrap();

                            id = match item {
                                | hir::id::AssocItemId::FuncId(id) => id,
                                | _ => unreachable!(),
                            };
                        },
                        | MethodSource::Record(_) => unimplemented!(),
                    }
                }

                while let TyKind::ForAll(_, t) = hir_ty.lookup(self.db.upcast()) {
                    hir_ty = t;
                }

                while let TyKind::Ctnt(_, t) = hir_ty.lookup(self.db.upcast()) {
                    hir_ty = t;
                }

                let def = id.into();
                let bodies = self.db.body_mir(def);
                let lib = self.def.module(self.db.upcast()).lib;
                let func_id = self.db.lang_item(lib, "fn-type".into()).unwrap();
                let func_id = func_id.as_type_ctor().unwrap();
                let mut arity = 0;

                while let Some([_, r]) = hir_ty.match_ctor(self.db.upcast(), func_id) {
                    arity += 1;
                    hir_ty = r;
                }

                let (id, arity) = bodies.arity(def, arity);

                if arity > 0 {
                    Operand::Const(Const::Addr(id), self.db.mir_type(hir_ty))
                } else {
                    let ret_ty = self.db.mir_type(hir_ty);
                    let func_lyt = Type::unit_func(ret_ty.clone());
                    let ret = ret
                        .take()
                        .unwrap_or_else(|| Place::new(self.builder.create_var(ret_ty)));

                    let func = Operand::Const(Const::Addr(id), func_lyt);

                    self.builder.call(ret.clone(), func, Vec::new());

                    Operand::Place(ret)
                }
            },
            | Some(ValueNs::Local(pat)) => Operand::Place(self.binders[&pat].clone()),
            | Some(ValueNs::Const(id)) => match self.db.eval(id.into()) {
                | crate::eval::EvalResult::Finished(c) => Operand::Const(c, self.db.mir_type(hir_ty)),
                | _ => Operand::Const(Const::Undefined, self.db.mir_type(hir_ty)),
            },
            | Some(ValueNs::Ctor(id)) => {
                let ret = ret.take().unwrap_or_else(|| {
                    let ty = self.db.mir_type(hir_ty);

                    Place::new(self.builder.create_var(ty))
                });

                Operand::Place(ret)
            },
            | r => unimplemented!("{:?}", r),
        }
    }

    fn lower_app(&mut self, mut base: hir::ExprId, ret_ty: Ty, ret: Option<Place>) -> Operand {
        let body = Arc::clone(&self.hir);
        let mut args = vec![];

        // eprintln!("{}", self.db.mir_type(ret_ty));

        while let hir::Expr::App { base: base2, arg } = body[base] {
            args.insert(0, arg);
            base = base2;
        }

        if let hir::Expr::Path { path } = &body[base] {
            let resolver = Resolver::for_expr(self.db.upcast(), self.def, base);

            match resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some(ValueNs::Func(mut id)) => {
                    if let Some(method) = self.infer.methods.get(&base) {
                        match *method {
                            | MethodSource::Instance(inst) => {
                                let data = self.db.instance_data(inst);
                                let item = data.item(path.segments().last().unwrap()).unwrap();

                                id = match item {
                                    | hir::id::AssocItemId::FuncId(id) => id,
                                    | _ => unreachable!(),
                                };
                            },
                            | MethodSource::Record(idx) => {
                                let record = self.builder.body().records[idx].clone();

                                unimplemented!("{}", record.to_type())
                            },
                        }
                    }

                    return self.lower_func_app(id, base, args, ret_ty, ret);
                },
                | Some(ValueNs::Ctor(id)) => return self.lower_ctor_app(id, args, ret_ty, ret),
                | _ => {},
            }
        }

        let ret = ret.unwrap_or_else(|| {
            let ty = self.db.mir_type(ret_ty);

            Place::new(self.builder.create_var(ty))
        });

        let func = self.lower_expr(base, None);
        let args = args.into_iter().map(|a| self.lower_expr(a, None)).collect();

        self.builder.call(ret.clone(), func, args);

        Operand::Place(ret)
    }

    fn lower_func_app(
        &mut self,
        func: hir::id::FuncId,
        func_expr: hir::ExprId,
        mut args: Vec<hir::ExprId>,
        ret_ty: Ty,
        ret: Option<Place>,
    ) -> Operand {
        let ret = ret.unwrap_or_else(|| {
            let ty = self.db.mir_type(ret_ty);

            Place::new(self.builder.create_var(ty))
        });

        if self.db.attrs(func.into()).by_key("intrinsic").exists() {
            let name = self.db.func_data(func).name.to_string();

            match name.as_str() {
                | "unsafe" => return self.lower_expr(args.remove(0), Some(ret)),
                | "apply" => match self.lower_expr(args.remove(0), None) {
                    | Operand::Const(
                        Const::Addr(BodyId {
                            def: hir::id::DefWithBodyId::FuncId(f),
                            ..
                        }),
                        _,
                    ) => return self.lower_func_app(f.into(), func_expr, args, ret_ty, Some(ret)),
                    | f => {
                        let args = args.into_iter().map(|a| self.lower_expr(a, None)).collect();

                        self.builder.call(ret.clone(), f, args)
                    },
                },
                | _ => {
                    let args = args.into_iter().map(|a| self.lower_expr(a, None)).collect();

                    self.builder.intrinsic(ret.clone(), name, args)
                },
            }
        } else {
            let func_ty = self.db.value_ty(func.into());
            let func_lyt = self.db.mir_type(func_ty);
            let def = func.into();
            let bodies = self.db.body_mir(def);
            let (func, _) = bodies.arity(def, args.len());
            let func = Operand::Const(Const::Addr(func), func_lyt);
            let args = args.into_iter().map(|a| self.lower_expr(a, None)).collect();

            self.builder.call(ret.clone(), func, args);
        }

        Operand::Place(ret)
    }

    fn lower_ctor_app(
        &mut self,
        id: hir::id::CtorId,
        args: Vec<hir::ExprId>,
        ret_ty: Ty,
        ret: Option<Place>,
    ) -> Operand {
        let data = self.db.type_ctor_data(id.parent);
        let ret = ret.unwrap_or_else(|| {
            let ty = self.db.mir_type(ret_ty);

            Place::new(self.builder.create_var(ty))
        });

        if data.ctors.len() == 1 {
            for (i, arg) in args.into_iter().enumerate() {
                self.lower_expr(arg, Some(ret.clone().field(i)));
            }
        } else {
            let idx: u32 = id.local_id.into_raw().into();
            let p = ret.clone().downcast(idx as usize);

            for (i, arg) in args.into_iter().enumerate() {
                self.lower_expr(arg, Some(p.clone().field(i)));
            }

            self.builder.set_discr(ret.clone(), idx as u128);
        }

        Operand::Place(ret)
    }

    /* fn lower_generic_call(
        &mut self,
        ret: Place,
        inst: Vec<Ty>,
        mut func_ty: Ty,
        func: Operand,
        mut args: Vec<Operand>,
        arg_tys: Vec<Ty>,
    ) {
        let func_id = self.lang_type("fn-type");
        let record_id = self.lang_type("record-type");
        let type_kind = TyKind::Ctor(self.lang_type("type-kind")).intern(self.db.upcast());
        let figure_kind = TyKind::Ctor(self.lang_type("figure-kind")).intern(self.db.upcast());
        let symbol_kind = TyKind::Ctor(self.lang_type("symbol-kind")).intern(self.db.upcast());
        let type_info = layout::type_info(self.db);
        let type_info = layout::reference(self.db, type_info);
        let mut arg_offset = 0;
        let mut inst_offset = 0;

        while let TyKind::ForAll(kind, t) = func_ty.lookup(self.db.upcast()) {
            if kind == type_kind {
                let arg = Const::type_info(&self.db.layout_of(inst[inst_offset]));
                let arg = Operand::Const(arg, type_info.clone());

                args.insert(arg_offset, arg);
                arg_offset += 1;
            } else if kind == figure_kind {
                if let TyKind::Figure(f) = inst[inst_offset].lookup(self.db.upcast()) {
                    let int_layout = layout::ptr_sized_int(self.db, true);
                    let arg = Operand::Const(Const::Scalar(f as u128), int_layout);

                    args.insert(arg_offset, arg);
                    arg_offset += 1;
                }
            } else if kind == symbol_kind {
                if let TyKind::Symbol(s) = inst[inst_offset].lookup(self.db.upcast()) {
                    let str_layout = layout::str_slice(self.db);
                    let arg = Operand::Const(Const::String(s), str_layout);

                    args.insert(arg_offset, arg);
                    arg_offset += 1;
                }
            }

            inst_offset += 1;
            func_ty = t;
        }

        while let TyKind::Ctnt(_, t) = func_ty.lookup(self.db.upcast()) {
            // arg_offset += 1;
            func_ty = t;
        }

        let arg_base = arg_offset;

        while let Some([arg_ty, ret]) = func_ty.match_ctor(self.db.upcast(), func_id) {
            if let TyKind::TypeVar(_) = arg_ty.lookup(self.db.upcast()) {
                let arg = self.builder.placed(args[arg_offset].clone());
                let arg_ref = self.by_ref(arg);

                args[arg_offset] = Operand::Place(arg_ref);
            } else if let Some([row]) = arg_ty.match_ctor(self.db.upcast(), record_id) {
                if let TyKind::Row(fields, Some(_)) = row.lookup(self.db.upcast()) {
                    let param_lyt = self.db.layout_of(arg_ty);
                    let param = self.builder.create_var(param_lyt);
                    let param = Place::new(param);
                    let arg = self.builder.placed(args[arg_offset].clone());
                    let mut offsets = Vec::new();

                    if let TyKind::App(_, row) = arg_tys[arg_offset - arg_base].lookup(self.db.upcast()) {
                        if let TyKind::Row(ty_fields, _) = row.lookup(self.db.upcast()) {
                            let layout = self.db.layout_of(arg_tys[arg_offset - arg_base]);

                            for field in fields.iter() {
                                let idx = ty_fields.iter().position(|f| f.name == field.name).unwrap();
                                let offset = layout.fields.offset(idx);

                                offsets.push(Const::Scalar(offset.bytes() as u128));
                            }
                        }
                    }

                    let uint = layout::ptr_sized_int(self.db, false);
                    let lyt = layout::array(uint, offsets.len());
                    let arr = Const::Tuple(offsets);
                    let meta = self.builder.create_var(lyt.clone());
                    let meta = Place::new(meta);

                    self.builder.use_op(meta.clone(), Operand::Const(arr, lyt));
                    self.builder.addr_of(param.clone().field(0), arg);
                    self.builder.addr_of(param.clone().field(1), meta);
                    args[arg_offset] = Operand::Place(param);
                }
            }

            arg_offset += 1;
            func_ty = ret;
        }

        if let TyKind::TypeVar(_) = func_ty.lookup(self.db.upcast()) {
            let ret_ref = self.by_ref(ret);
            let unit = self.builder.create_var(Arc::new(Layout::UNIT));
            let unit = Place::new(unit);

            args.insert(0, Operand::Place(ret_ref));
            self.builder.call(unit, func, args);
        } else {
            self.builder.call(ret, func, args);
        }
    } */

    /* fn by_ref(&mut self, place: Place) -> Place {
        let lyt = self.builder.place_layout(self.db, &place);
        let lyt = layout::reference(self.db, lyt);
        let var = self.builder.create_var(lyt);
        let var = Place::new(var);

        self.builder.addr_of(var.clone(), place);
        var
    } */

    fn lang_type(&self, name: &'static str) -> hir::id::TypeCtorId {
        let item = self
            .db
            .lang_item(self.def.module(self.db.upcast()).lib, name.into())
            .unwrap();

        item.as_type_ctor().unwrap()
    }
}

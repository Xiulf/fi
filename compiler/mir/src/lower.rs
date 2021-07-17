pub mod builder;
pub mod pattern;

use crate::db::MirDatabase;
use crate::instance_record::InstanceRecord;
use crate::ir::*;
use crate::ty::{self, *};
use builder::Builder;
use hir::display::HirDisplay as _;
use hir::id::{DefWithBodyId, HasModule as _};
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

        if !matches!(body[body.body_expr()], hir::Expr::Missing) {
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

    fn finish(mut self) -> Bodies {
        crate::post::postprocess(self.db, &mut self.bodies);
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

        // if self.def.has_body(self.db.upcast()) && args.len() > 1 {
        //     let params = args.iter().map(|&t| self.db.mir_type(t)).collect();
        //     let ret = self.db.mir_type(ret_ty);
        //     let ty = Type::func(params, ret);
        //
        //     self.generate_curry(ret_ty, &args, args.len() - 1, local_id, ty);
        // }

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

        // eprintln!("{}", self.bodies.display(self.db.upcast()));
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

macro_rules! resolve_method {
    ($self:ident, $expr:ident, $path:expr, | $i:ident | $instance:expr, | $r:ident | $record:expr, || $else:expr) => {
        'block: {
            if let Some(method) = $self.infer.methods.get(&$expr) {
                match *method {
                    | MethodSource::Instance(inst) => {
                        let data = $self.db.instance_data(inst);

                        if let Some(item) = data.item($path.segments().last().unwrap()) {
                            if let hir::id::AssocItemId::FuncId($i) = item {
                                break 'block $instance;
                            }
                        }
                    },
                    | MethodSource::Record($r) => {
                        break 'block $record;
                    },
                }
            }

            $else
        }
    };
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
            let pat = self.convert_pat(param, place, &mut FxHashMap::default());

            assert!(pat.is_none());
            // self.lower_pat(param, place);
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
                        if has_rest && tail.is_some() {
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

                if let Some((ValueNs::Fixity(f), _)) = resolver.resolve_value_fully(self.db.upcast(), op) {
                    let fixity = self.db.fixity_data(f);
                    let resolver = f.resolver(self.db.upcast());

                    match resolver.resolve_value_fully(self.db.upcast(), &fixity.func) {
                        | Some((ValueNs::Func(mut f), _)) => {
                            resolve_method!(
                                self,
                                id,
                                &fixity.func,
                                |inst| self.lower_func_app(inst, id, vec![lhs, rhs], hir_ty, ret.take()),
                                |idx| {
                                    // @TODO: use record
                                    self.lower_func_app(f, id, vec![lhs, rhs], hir_ty, ret.take())
                                },
                                || self.lower_func_app(f, id, vec![lhs, rhs], hir_ty, ret.take())
                            )
                        },
                        | Some((ValueNs::Ctor(c), _)) => self.lower_ctor_app(c, vec![lhs, rhs], hir_ty, ret.take()),
                        | _ => Operand::Const(Const::Undefined, ty),
                    }
                } else {
                    Operand::Const(Const::Undefined, ty)
                }
            },
            | hir::Expr::App { .. } => self.lower_app(id, hir_ty, ret.take()),
            | hir::Expr::Field { base, ref field } => {
                if let Some(idx) = field.as_tuple_index() {
                    let base = self.lower_expr(base, ret.take());
                    let base = self.builder.placed(base);

                    Operand::Place(base.field(idx))
                } else {
                    let record_id = self.lang_type("record-type");
                    let base_ty = self.infer.type_of_expr[base];
                    let base = self.lower_expr(base, ret.take());
                    let base = self.builder.placed(base);

                    if let Some([row]) = base_ty.match_ctor(self.db.upcast(), record_id) {
                        if let TyKind::Row(fields, _) = row.lookup(self.db.upcast()) {
                            let idx = fields.iter().position(|f| &f.name == field).unwrap();

                            return Operand::Place(base.field(idx));
                        }
                    }

                    unreachable!();
                }
            },
            | hir::Expr::Tuple { ref exprs } => {
                let ret = ret.take().unwrap_or_else(|| Place::new(self.builder.create_var(ty)));

                for (i, &expr) in exprs.iter().enumerate() {
                    self.lower_expr(expr, Some(ret.clone().field(i)));
                }

                Operand::Place(ret)
            },
            | hir::Expr::Record { ref fields } => {
                let record_id = self.lang_type("record-type");
                let row_fields = match hir_ty.match_ctor(self.db.upcast(), record_id) {
                    | Some([row]) => match row.lookup(self.db.upcast()) {
                        | TyKind::Row(row_fields, _) => row_fields,
                        | _ => unreachable!(),
                    },
                    | None => unreachable!(),
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
            | hir::Expr::If {
                cond,
                then,
                else_,
                inverse,
            } => {
                let exit_block = self.builder.create_block();
                let cond = self.lower_expr(cond, None);
                let ret = ret.take().unwrap_or_else(|| Place::new(self.builder.create_var(ty)));

                if let Some(else_) = else_ {
                    let then_block = self.builder.create_block();
                    let else_block = self.builder.create_block();

                    if inverse {
                        self.builder.switch(cond, vec![0], vec![then_block, else_block]);
                    } else {
                        self.builder.switch(cond, vec![0], vec![else_block, then_block]);
                    }

                    self.builder.set_block(then_block);
                    self.lower_expr(then, Some(ret.clone()));
                    self.builder.jump(exit_block);

                    self.builder.set_block(else_block);
                    self.lower_expr(else_, Some(ret.clone()));
                    self.builder.jump(exit_block);
                } else {
                    let then_block = self.builder.create_block();

                    if inverse {
                        self.builder.switch(cond, vec![0], vec![then_block, exit_block]);
                    } else {
                        self.builder.switch(cond, vec![0], vec![exit_block, then_block]);
                    }

                    self.builder.set_block(then_block);
                    self.lower_expr(then, None);
                    self.builder.jump(exit_block);
                }

                self.builder.set_block(exit_block);

                Operand::Place(ret)
            },
            | hir::Expr::Case { pred, ref arms } => {
                let pred = self.lower_expr(pred, None);
                let pred = self.builder.placed(pred);
                let case = self.convert_arms(vec![pred], &arms);

                self.lower_case(case, ty, ret.take())
            },
            | ref e => unimplemented!("{:?}", e),
        }
    }

    fn lower_path(&mut self, expr: hir::ExprId, path: &hir::Path, mut hir_ty: Ty, ret: &mut Option<Place>) -> Operand {
        let resolver = Resolver::for_expr(self.db.upcast(), self.def, expr);

        match resolver.resolve_value_fully(self.db.upcast(), path) {
            | Some((ValueNs::Func(mut id), _)) => {
                resolve_method!(self, expr, path, |inst| id = inst, |rec| unimplemented!(), || {});

                while let TyKind::ForAll(_, t) = hir_ty.lookup(self.db.upcast()) {
                    hir_ty = t;
                }

                while let TyKind::Ctnt(_, t) = hir_ty.lookup(self.db.upcast()) {
                    hir_ty = t;
                }

                let def: DefWithBodyId = id.into();
                let lib = self.def.module(self.db.upcast()).lib;
                let func_id = self.db.lang_item(lib, "fn-type".into()).unwrap();
                let func_id = func_id.as_type_ctor().unwrap();
                let mut arity = 0;

                while let Some([_, r]) = hir_ty.match_ctor(self.db.upcast(), func_id) {
                    arity += 1;
                    hir_ty = r;
                }

                let (id, arity) = if def == self.def {
                    self.builder.arity(def, arity)
                } else {
                    let bodies = self.db.body_mir(def);

                    bodies.arity(def, arity)
                };

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
            | Some((ValueNs::Local(pat), _)) => Operand::Place(self.binders[&pat].clone()),
            | Some((ValueNs::Const(id), _)) => match self.db.eval(id.into()) {
                | crate::eval::EvalResult::Finished(c) => Operand::Const(c, self.db.mir_type(hir_ty)),
                | _ => Operand::Const(Const::Undefined, self.db.mir_type(hir_ty)),
            },
            | Some((ValueNs::Ctor(id), _)) => {
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

        while let hir::Expr::App { base: base2, arg } = body[base] {
            args.insert(0, arg);
            base = base2;
        }

        let func = if let hir::Expr::Path { path } = &body[base] {
            match self.resolve_path(base, path) {
                | Some(ValueNs::Func(mut id)) => {
                    resolve_method!(
                        self,
                        base,
                        path,
                        |inst| return self.lower_func_app(inst, base, args, ret_ty, ret),
                        |idx| Operand::Record(idx, path.segments().last().unwrap().clone()),
                        || return self.lower_func_app(id, base, args, ret_ty, ret)
                    )
                },
                | Some(ValueNs::Ctor(id)) => return self.lower_ctor_app(id, args, ret_ty, ret),
                | _ => self.lower_expr(base, None),
            }
        } else {
            self.lower_expr(base, None)
        };

        let args = args.into_iter().map(|a| self.lower_expr(a, None)).collect();
        let ret = ret.unwrap_or_else(|| {
            let ty = self.db.mir_type(ret_ty);

            Place::new(self.builder.create_var(ty))
        });

        self.builder.call(ret.clone(), func, args);

        Operand::Place(ret)
    }

    fn resolve_path(&self, expr: hir::ExprId, path: &hir::Path) -> Option<ValueNs> {
        let resolver = Resolver::for_expr(self.db.upcast(), self.def, expr);

        resolver.resolve_value_fully(self.db.upcast(), path).map(|it| it.0)
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
                | "size_of" => {
                    let arg_ty = self.infer.type_of_expr[args[0]];
                    let proxy_id = self.lang_type("proxy-type");

                    if let Some([ty]) = arg_ty.match_ctor(self.db.upcast(), proxy_id) {
                        let ty = self.db.mir_type(ty);
                        let layout = self.db.layout_of(ty);
                        let size = Const::Scalar(layout.size.bytes() as u128);
                        let ty = self.builder.place_type(&ret);

                        self.builder.use_op(ret.clone(), Operand::Const(size, ty));
                    } else {
                        unreachable!();
                    }
                },
                | "addr_of" => {
                    let arg = self.lower_expr(args[0], None);
                    let arg = self.builder.placed(arg);

                    self.builder.addr_of(ret.clone(), arg);
                },
                | "ptr_read" => {
                    let arg = self.lower_expr(args[0], None);
                    let arg = self.builder.placed(arg);

                    self.builder.use_op(ret.clone(), Operand::Place(arg.deref()));
                },
                | "ptr_write" => {
                    let ptr = self.lower_expr(args[0], None);
                    let ptr = self.builder.placed(ptr);
                    let val = self.lower_expr(args[1], None);

                    self.builder.use_op(ptr.deref(), val);
                },
                | _ => {
                    let args = args.into_iter().map(|a| self.lower_expr(a, None)).collect();

                    self.builder.intrinsic(ret.clone(), name, args)
                },
            }
        } else {
            let func_ty = self.db.value_ty(func.into());
            let func_lyt = self.db.mir_type(func_ty);
            let def: DefWithBodyId = func.into();
            let (func, _) = if def == self.def {
                self.builder.arity(def, args.len())
            } else {
                let bodies = self.db.body_mir(def);

                bodies.arity(def, args.len())
            };

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

    fn lang_type(&self, name: &'static str) -> hir::id::TypeCtorId {
        let item = self
            .db
            .lang_item(self.def.module(self.db.upcast()).lib, name.into())
            .unwrap();

        item.as_type_ctor().unwrap()
    }
}

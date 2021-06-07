pub mod builder;

use crate::db::MirDatabase;
use crate::ir::*;
use crate::layout::{self, Layout};
use builder::Builder;
use hir::display::HirDisplay as _;
use hir::id::HasModule as _;
use hir::ty::{Ty, TyKind, TypeVar};
use hir::MethodSource;
use hir_def::resolver::{HasResolver, Resolver, ValueNs};
use rustc_hash::FxHashMap;
use std::sync::Arc;

impl Body {
    pub(crate) fn body_mir_query(db: &dyn MirDatabase, def: hir::id::DefWithBodyId) -> Arc<Self> {
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

        while let Some([arg, ret]) = ty.match_ctor(db.upcast(), func_id) {
            args.push(arg);
            ty = ret;
        }

        let mut lcx = LowerCtx::new(db, def, ty);

        for var in vars {
            let layout = layout::type_var(db, def.module(db.upcast()).lib, var);

            lcx.add_type_var(layout);
        }

        lcx.lower();

        Arc::new(lcx.builder.finish())
    }
}

struct LowerCtx<'a> {
    db: &'a dyn MirDatabase,
    def: hir::id::DefWithBodyId,
    hir: Arc<hir::Body>,
    infer: Arc<hir::InferenceResult>,
    builder: Builder,
    ret: LocalId,
    binders: FxHashMap<hir::PatId, Place>,
    type_vars: Vec<Option<LocalId>>,
}

impl<'a> LowerCtx<'a> {
    fn new(db: &'a dyn MirDatabase, def: hir::id::DefWithBodyId, ret_ty: Ty) -> Self {
        let mut builder = Builder::default();
        let ret = builder.create_ret(db, ret_ty);

        Self {
            db,
            def,
            ret,
            builder,
            hir: db.body(def),
            infer: db.infer(def),
            binders: FxHashMap::default(),
            type_vars: Vec::new(),
        }
    }

    fn add_type_var(&mut self, type_info: Option<Arc<Layout>>) {
        let type_var = type_info.map(|layout| self.builder.create_arg(layout, false));

        self.type_vars.push(type_var);
    }

    fn lower(&mut self) {
        let entry = self.builder.create_block();

        self.builder.set_block(entry);

        for param in self.hir.params().to_vec() {
            let ty = self.infer.type_of_pat[param];
            let by_ref = matches!(ty.lookup(self.db.upcast()), TyKind::TypeVar(_));
            let lyt = self.db.layout_of(ty);
            let arg = self.builder.create_arg(lyt, by_ref);
            let place = Place::new(arg);

            self.lower_pat(param, place);
        }

        let ret = Place::new(self.ret);

        self.lower_expr(self.hir.body_expr(), Some(ret));
        self.builder.ret();
    }

    fn lower_pat(&mut self, id: hir::PatId, place: Place) {
        let body = Arc::clone(&self.hir);
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
                            let uint_lyt = layout::ptr_sized_int(self.db, false);

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

    fn type_info(&self, var: TypeVar) -> LocalId {
        let idx = self.type_vars.len() - 1 - var.debruijn().depth() as usize;

        self.type_vars[idx].unwrap()
    }

    fn lower_expr(&mut self, id: hir::ExprId, mut ret: Option<Place>) -> Operand {
        let op = self.lower_expr_impl(id, &mut ret);

        if let Some(ret) = ret {
            if let TyKind::TypeVar(var) = self.infer.type_of_expr[id].lookup(self.db.upcast()) {
                let info = self.type_info(var);
                let info = Place::new(info);
                let rhs = self.builder.placed(op);
                let place = if self.builder.is_by_ref(&rhs) {
                    rhs
                } else {
                    let layout = self.builder.place_layout(self.db, &rhs);
                    let place = self.builder.create_var(layout::reference(self.db, layout));
                    let place = Place::new(place);

                    self.builder.addr_of(place.clone(), rhs);
                    place
                };

                self.builder.memcpy(ret.clone(), place, info.deref().field(0));
            } else {
                self.builder.use_op(ret.clone(), op);
            }

            Operand::Place(ret)
        } else {
            op
        }
    }

    fn lower_expr_impl(&mut self, id: hir::ExprId, ret: &mut Option<Place>) -> Operand {
        let body = Arc::clone(&self.hir);
        let ty = self.infer.type_of_expr[id];
        let lyt = self.db.layout_of(ty);

        match body[id] {
            | hir::Expr::Missing => Operand::Const(Const::Undefined, lyt),
            | hir::Expr::Typed { expr, .. } => self.lower_expr_impl(expr, ret),
            | hir::Expr::Path { ref path } => self.lower_path(id, path, ty, ret),
            | hir::Expr::Lit { ref lit } => match *lit {
                | hir::Literal::Int(i) => Operand::Const(Const::Scalar(i as u128), lyt),
                | hir::Literal::Float(i) => Operand::Const(Const::Scalar(i as u128), lyt),
                | hir::Literal::Char(i) => Operand::Const(Const::Scalar(i as u128), lyt),
                | hir::Literal::String(ref s) => Operand::Const(Const::String(s.clone()), lyt),
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
                                }
                            }

                            self.lower_func_app(f, id, vec![lhs, rhs], ty, ret.take())
                        },
                        | Some(ValueNs::Ctor(c)) => self.lower_ctor_app(c, vec![lhs, rhs], ty, ret.take()),
                        | _ => Operand::Const(Const::Undefined, lyt),
                    }
                } else {
                    Operand::Const(Const::Undefined, lyt)
                }
            },
            | hir::Expr::App { .. } => self.lower_app(id, ty, ret.take()),
            | hir::Expr::Tuple { ref exprs } => {
                let ret = ret.take().unwrap_or_else(|| {
                    let lyt = self.db.layout_of(ty);

                    Place::new(self.builder.create_var(lyt))
                });

                for (i, &expr) in exprs.iter().enumerate() {
                    self.lower_expr(expr, Some(ret.clone().field(i)));
                }

                Operand::Place(ret)
            },
            | hir::Expr::Record { ref fields } => {
                let row_fields = match ty.lookup(self.db.upcast()) {
                    | TyKind::App(_, row) => match row.lookup(self.db.upcast()) {
                        | TyKind::Row(row_fields, _) => row_fields,
                        | _ => unreachable!(),
                    },
                    | _ => unreachable!(),
                };

                let ret = ret.take().unwrap_or_else(|| Place::new(self.builder.create_var(lyt)));

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
                            let ty = self.infer.type_of_expr[val];
                            let lyt = self.db.layout_of(ty);
                            let place = self.builder.create_var(lyt);
                            let place = Place::new(place);

                            self.lower_expr(val, Some(place.clone()));
                            self.lower_pat(pat, place);
                        },
                    }
                }

                Operand::Const(Const::Tuple(Vec::new()), lyt)
            },
            | ref e => unimplemented!("{:?}", e),
        }
    }

    fn lower_path(&mut self, expr: hir::ExprId, path: &hir::Path, mut ty: Ty, ret: &mut Option<Place>) -> Operand {
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
                    }
                }

                while let TyKind::ForAll(_, t) = ty.lookup(self.db.upcast()) {
                    ty = t;
                }

                while let TyKind::Ctnt(_, t) = ty.lookup(self.db.upcast()) {
                    ty = t;
                }

                if let TyKind::App(..) = ty.lookup(self.db.upcast()) {
                    Operand::Const(Const::FuncAddr(id.into()), self.db.layout_of(ty))
                } else {
                    let func_lyt = self.func_layout();
                    let ret = ret.take().unwrap_or_else(|| {
                        let lyt = self.db.layout_of(ty);

                        Place::new(self.builder.create_var(lyt))
                    });

                    let func = Operand::Const(Const::FuncAddr(id.into()), func_lyt);

                    self.builder.call(ret.clone(), func, Vec::new());

                    Operand::Place(ret)
                }
            },
            | Some(ValueNs::Local(pat)) => Operand::Place(self.binders[&pat].clone()),
            | Some(ValueNs::Const(id)) => match self.db.eval(id.into()) {
                | crate::eval::EvalResult::Finished(c) => Operand::Const(c, self.db.layout_of(ty)),
                | _ => Operand::Const(Const::Undefined, self.db.layout_of(ty)),
            },
            | Some(ValueNs::Ctor(id)) => {
                let ret = ret.take().unwrap_or_else(|| {
                    let lyt = self.db.layout_of(ty);

                    Place::new(self.builder.create_var(lyt))
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

        if let hir::Expr::Path { path } = &body[base] {
            let resolver = Resolver::for_expr(self.db.upcast(), self.def, base);

            match resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some(ValueNs::Func(mut id)) => {
                    if let Some(method) = self.infer.methods.get(&base) {
                        match method {
                            | MethodSource::Instance(inst) => {
                                let data = self.db.instance_data(*inst);
                                let item = data.item(path.segments().last().unwrap()).unwrap();

                                id = match item {
                                    | hir::id::AssocItemId::FuncId(id) => id,
                                    | _ => unreachable!(),
                                };
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
            let lyt = self.db.layout_of(ret_ty);

            Place::new(self.builder.create_var(lyt))
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
            let lyt = self.db.layout_of(ret_ty);

            Place::new(self.builder.create_var(lyt))
        });

        if self.db.attrs(func.into()).by_key("intrinsic").exists() {
            let name = self.db.func_data(func).name.to_string();

            match name.as_str() {
                | "unsafe" => return self.lower_expr(args.remove(0), Some(ret)),
                | "apply" => match self.lower_expr(args.remove(0), None) {
                    | Operand::Const(Const::FuncAddr(f), _) => {
                        return self.lower_func_app(f.into(), func_expr, args, ret_ty, Some(ret))
                    },
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
            let func_lyt = self.db.layout_of(func_ty);
            let func = Operand::Const(Const::FuncAddr(func.into()), func_lyt);
            let arg_tys = args.iter().map(|&a| self.infer.type_of_expr[a]).collect();
            let args = args.into_iter().map(|a| self.lower_expr(a, None)).collect();

            if let Some(inst) = self.infer.instances.get(&func_expr) {
                self.lower_generic_call(ret.clone(), inst.clone(), func_ty, func, args, arg_tys);
            } else {
                self.builder.call(ret.clone(), func, args);
            }
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
            let lyt = self.db.layout_of(ret_ty);

            Place::new(self.builder.create_var(lyt))
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

    fn func_layout(&self) -> Arc<Layout> {
        let func_ty = self
            .db
            .lang_item(self.def.module(self.db.upcast()).lib, "fn-type".into())
            .unwrap();
        let func_ty = TyKind::Ctor(func_ty.as_type_ctor().unwrap()).intern(self.db.upcast());

        self.db.layout_of(func_ty)
    }

    fn lower_generic_call(
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
    }

    fn by_ref(&mut self, place: Place) -> Place {
        let lyt = self.builder.place_layout(self.db, &place);
        let lyt = layout::reference(self.db, lyt);
        let var = self.builder.create_var(lyt);
        let var = Place::new(var);

        self.builder.addr_of(var.clone(), place);
        var
    }

    fn lang_type(&self, name: &'static str) -> hir::id::TypeCtorId {
        let item = self
            .db
            .lang_item(self.def.module(self.db.upcast()).lib, name.into())
            .unwrap();

        item.as_type_ctor().unwrap()
    }
}

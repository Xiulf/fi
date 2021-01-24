#![feature(label_break_value)]
#![feature(partition_point)]
#![feature(iterator_fold_self)]

pub mod javascript;
pub mod pattern;

use hir::ir as hir;
use lowlang::ir;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::query_group(LowerDatabaseStorage)]
pub trait LowerDatabase: typeck::TypeDatabase {
    fn lower(&self, lib: hir::LibId, module: hir::ModuleId) -> Arc<ir::Module>;

    #[salsa::invoke(javascript::lower)]
    fn lower_js(&self, lib: hir::LibId, module: hir::ModuleId) -> Arc<javascript::JsModule>;
}

pub fn lower(db: &dyn LowerDatabase, lib: hir::LibId, module: hir::ModuleId) -> Arc<ir::Module> {
    let file = db.module_tree(lib).file(module);
    let hir = db.module_hir(file);
    // let start = std::time::Instant::now();
    let mut converter = Converter::new(db);

    converter.convert(&hir);

    let mut low = converter.finish();

    // println!("{}", low);
    lowlang::analysis::mandatory(&mut low, db.target(lib).triple());
    // println!("lowered in {:?}", start.elapsed());

    Arc::new(low)
}

pub struct Converter<'db> {
    db: &'db dyn LowerDatabase,
    decls: ir::Decls,
    impls: ir::Impls,
    bodies: ir::Bodies,
}

pub struct BodyConverter<'db, 'c> {
    db: &'db dyn LowerDatabase,
    _hir: &'db hir::Module,
    types: Arc<typeck::TypecheckResult>,
    builder: ir::Builder<'c>,
    decls: &'c HashMap<hir::DefId, ir::DeclId>,
    locals: HashMap<hir::HirId, ir::Local>,
}

impl<'db> Converter<'db> {
    pub fn new(db: &'db dyn LowerDatabase) -> Self {
        Converter {
            db,
            decls: ir::Decls::new(),
            impls: ir::Impls::new(),
            bodies: ir::Bodies::new(),
        }
    }

    pub fn finish(self) -> ir::Module {
        ir::Module {
            decls: self.decls,
            impls: self.impls,
            bodies: self.bodies,
        }
    }

    pub fn convert(&mut self, hir: &hir::Module) {
        let mut decls = HashMap::with_capacity(hir.imports.len() + hir.items.len());

        for &id in &hir.imports {
            let ty = self.db.typecheck(id);
            let declid = self.decls.next_idx();
            let attrs = if id.lib == self.db.lib() {
                let file = self.db.module_tree(id.lib).file(id.module);
                let hir = self.db.module_hir(file);
                let def = hir.def(id);

                if let hir::Def::Item(item) = def {
                    ir::Attrs {
                        c_abi: item.abi() == Some("C"),
                    }
                } else {
                    ir::Attrs::default()
                }
            } else {
                ir::Attrs::default()
            };

            if let Some(link_name) = self.link_name(id) {
                decls.insert(id, declid);
                self.decls.insert(declid, ir::Decl {
                    id: declid,
                    name: link_name,
                    ty: lower_type(self.db, &ty.ty),
                    linkage: ir::Linkage::Import,
                    attrs,
                });
            }
        }

        for (_, item) in &hir.items {
            match &item.kind {
                | hir::ItemKind::Func { .. } => {
                    let ty = self.db.typecheck(item.id.owner);
                    let declid = self.decls.next_idx();
                    let mut ty = lower_type(self.db, &ty.ty);

                    if !matches!(ty.kind, ir::Type::Func(_)) {
                        ty = ir::Ty::new(ir::Type::Func(ir::Signature {
                            params: Vec::new(),
                            rets: vec![ty],
                        }));
                    }

                    decls.insert(item.id.owner, declid);
                    self.decls.insert(declid, ir::Decl {
                        id: declid,
                        name: self.link_name(item.id.owner).unwrap(),
                        ty,
                        linkage: ir::Linkage::Export,
                        attrs: ir::Attrs {
                            c_abi: item.abi() == Some("C"),
                        },
                    });
                },
                | hir::ItemKind::Static { .. } => {
                    let ty = self.db.typecheck(item.id.owner);
                    let declid = self.decls.next_idx();

                    decls.insert(item.id.owner, declid);
                    self.decls.insert(declid, ir::Decl {
                        id: declid,
                        name: self.link_name(item.id.owner).unwrap(),
                        ty: lower_type(self.db, &ty.ty),
                        linkage: ir::Linkage::Export,
                        attrs: ir::Attrs::default(),
                    });
                },
                | hir::ItemKind::Const { .. } => unimplemented!(),
                | hir::ItemKind::Foreign { kind, .. } => {
                    if !item.is_intrinsic() {
                        let ty = self.db.typecheck(item.id.owner);
                        let declid = self.decls.next_idx();
                        let mut ty = lower_type(self.db, &ty.ty);

                        if let hir::ForeignKind::Func = kind {
                            if !matches!(ty.kind, ir::Type::Func(_)) {
                                ty = ir::Ty::new(ir::Type::Func(ir::Signature {
                                    params: Vec::new(),
                                    rets: vec![ty],
                                }));
                            }
                        }

                        decls.insert(item.id.owner, declid);
                        self.decls.insert(declid, ir::Decl {
                            id: declid,
                            name: item.name.to_string(),
                            ty,
                            linkage: ir::Linkage::Export,
                            attrs: ir::Attrs {
                                c_abi: item.abi() == Some("C"),
                            },
                        });
                    }
                },
                | _ => {},
            }
        }

        for (_, item) in &hir.instance_items {
            match &item.kind {
                | hir::InstanceItemKind::Func { .. } => {
                    let ty = self.db.typecheck(item.id.owner);
                    let declid = self.decls.next_idx();

                    decls.insert(item.id.owner, declid);
                    self.decls.insert(declid, ir::Decl {
                        id: declid,
                        name: self.link_name(item.id.owner).unwrap(),
                        ty: lower_type(self.db, &ty.ty),
                        linkage: ir::Linkage::Export,
                        attrs: ir::Attrs { c_abi: false },
                    });
                },
            }
        }

        for (_, item) in &hir.items {
            match &item.kind {
                | hir::ItemKind::Func { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let declid = decls[&item.id.owner];
                    let bodyid = self.bodies.next_idx();
                    let mut b = ir::Body::new(bodyid, declid);
                    let builder = ir::Builder::new(&mut b);
                    let conv = BodyConverter::new(self.db, hir, ty, builder, &decls);

                    conv.convert(body);
                    self.bodies.insert(bodyid, b);
                },
                | hir::ItemKind::Static { .. } => unimplemented!(),
                | hir::ItemKind::Const { .. } => unimplemented!(),
                | _ => {},
            }
        }

        for (_, item) in &hir.instance_items {
            match &item.kind {
                | hir::InstanceItemKind::Func { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let declid = decls[&item.id.owner];
                    let bodyid = self.bodies.next_idx();
                    let mut b = ir::Body::new(bodyid, declid);
                    let builder = ir::Builder::new(&mut b);
                    let conv = BodyConverter::new(self.db, hir, ty, builder, &decls);

                    conv.convert(body);
                    self.bodies.insert(bodyid, b);
                },
            }
        }
    }

    fn link_name(&self, id: hir::DefId) -> Option<String> {
        let name = if id.lib == self.db.lib() {
            let file = self.db.module_tree(id.lib).file(id.module);
            let hir = self.db.module_hir(file);
            let def = hir.def(id);

            match def {
                | hir::Def::Item(item) => {
                    if item.is_intrinsic() {
                        return None;
                    } else if item.is_main() {
                        return Some(String::from("main"));
                    } else if item.is_no_mangle() {
                        return Some(item.name.to_string());
                    } else if let hir::ItemKind::Foreign { .. } = item.kind {
                        return Some(item.name.to_string());
                    } else {
                        format!("{}.{}", hir.name, item.name)
                    }
                },
                | hir::Def::ClassItem(_) => return None,
                | hir::Def::InstanceItem(item) => {
                    let owner = hir.def(item.owner.owner);

                    format!("{}.{}.{}", hir.name, owner.name(), item.name)
                },
            }
        } else {
            let external = self.db.external_item_data(id.lib, id.module);

            if external.is_intrinsic(&id) {
                return None;
            } else if external.is_no_mangle(&id) {
                return Some(external.items[&id].0.to_string());
            } else {
                let module = self.db.external_modules(self.db.lib()).iter().find(|m| m.id == id.module).unwrap().name;

                match external.items[&id].2 {
                    | ::hir::ExternalItemOrigin::Item => format!("{}.{}", module, external.items[&id].0),
                    | ::hir::ExternalItemOrigin::InstanceItem(instance) => {
                        let instace_name = external.items[&instance].0;

                        format!("{}.{}.{}", module, instace_name, external.items[&id].0)
                    },
                    | ::hir::ExternalItemOrigin::ClassItem(_) => return None,
                }
            }
        };

        Some(mangling::mangle(name.bytes()))
    }
}

impl<'db, 'c> BodyConverter<'db, 'c> {
    pub fn new(
        db: &'db dyn LowerDatabase,
        _hir: &'db hir::Module,
        types: Arc<typeck::TypecheckResult>,
        builder: ir::Builder<'c>,
        decls: &'c HashMap<hir::DefId, ir::DeclId>,
    ) -> Self {
        BodyConverter {
            db,
            _hir,
            types,
            builder,
            decls,
            locals: HashMap::new(),
        }
    }

    pub fn convert(mut self, body: &hir::Body) {
        let ret = self.create_header(&body.params, self.types.ty.clone());
        let entry = self.builder.create_block();
        let _ = self.builder.set_block(entry);
        let res = self.convert_expr(&body.value);

        self.builder.use_op(ir::Place::new(ret), res);
        self.builder.return_();
    }

    fn create_header(&mut self, params: &[hir::Param], ty: typeck::ty::Ty) -> ir::Local {
        use typeck::ty::Type;

        match &*ty {
            | Type::ForAll(_, _, t2, _) => self.create_header(params, t2.clone()),
            | Type::Ctnt(_, t2) => self.create_header(params, t2.clone()),
            | _ => {
                let (args, ret) = ty_get_args(self.db, ty);
                let ret = self.builder.create_ret(lower_type(self.db, &ret));

                for (param, ty) in params.iter().zip(args) {
                    let local = self.builder.create_arg(lower_type(self.db, &ty));

                    self.locals.insert(param.id, local);
                }

                ret
            },
        }
    }

    fn convert_expr(&mut self, expr: &hir::Expr) -> ir::Operand {
        let ty = lower_type(self.db, &self.types.tys[&expr.id]);

        match &expr.kind {
            | hir::ExprKind::Error => unreachable!(),
            | hir::ExprKind::Hole { .. } => ir::Operand::Const(ir::Const::Undefined(ty)),
            | hir::ExprKind::Ident { res, name } => match res {
                | hir::Res::Error => unreachable!(),
                | hir::Res::Def(d, id) => match d {
                    | hir::DefKind::Func | hir::DefKind::Static => ir::Operand::Const(ir::Const::Addr(self.decls[id])),
                    | hir::DefKind::Ctor => {
                        let idx = if id.lib == self.db.lib() {
                            let file = self.db.module_tree(id.lib).file(id.module);
                            let hir = self.db.module_hir(file);
                            let ctor = hir.items[&(*id).into()].ctor();
                            let data = hir.items[&ctor.0].data_body();

                            data.iter().position(|id| hir.items[id].name.symbol == name.symbol).unwrap()
                        } else {
                            let external = self.db.external_item_data(id.lib, id.module);

                            external.datas[id].iter().position(|(n, _)| n.symbol == name.symbol).unwrap()
                        };

                        let cs = if let ir::Type::Box(to) = ty.kind {
                            ir::Const::Ptr(Box::new(ir::Const::Variant(idx, Vec::new(), *to)))
                        } else {
                            ir::Const::Variant(idx, Vec::new(), ty)
                        };

                        ir::Operand::Const(cs)
                    },
                    | _ => unreachable!(),
                },
                | hir::Res::Local(id) => ir::Operand::Place(ir::Place::new(self.locals[id].clone())),
            },
            | hir::ExprKind::Int { val } => ir::Operand::Const(ir::Const::Scalar(*val, ty)),
            | hir::ExprKind::Float { bits } => ir::Operand::Const(ir::Const::Scalar(*bits as u128, ty)),
            | hir::ExprKind::Char { val } => ir::Operand::Const(ir::Const::Scalar(*val as u128, ty)),
            | hir::ExprKind::Str { val: _ } => unimplemented!(),
            | hir::ExprKind::App { base, arg } => {
                let mut base = base;
                let mut args = vec![&**arg];

                while let hir::ExprKind::App { base: b, arg } = &base.kind {
                    base = b;
                    args.push(&**arg);
                }

                args.reverse();
                self.convert_app(base, args, ty)
            },
            | hir::ExprKind::Tuple { exprs } => {
                if exprs.is_empty() {
                    ir::Operand::Const(ir::Const::Tuple(Vec::new()))
                } else {
                    let res = self.builder.create_tmp(ty);
                    let res = ir::Place::new(res);

                    for (i, expr) in exprs.iter().enumerate() {
                        let op = self.convert_expr(expr);

                        self.builder.use_op(res.clone().field(i), op);
                    }

                    ir::Operand::Place(res)
                }
            },
            | hir::ExprKind::Record { fields } => {
                if fields.is_empty() {
                    ir::Operand::Const(ir::Const::Tuple(Vec::new()))
                } else {
                    let res = self.builder.create_tmp(ty);
                    let res = ir::Place::new(res);

                    for (i, field) in fields.iter().enumerate() {
                        let op = self.convert_expr(&field.val);

                        self.builder.use_op(res.clone().field(i), op);
                    }

                    ir::Operand::Place(res)
                }
            },
            | hir::ExprKind::Field { base, field } => {
                let base_ty = self.types.tys[&base.id].clone();

                if let typeck::ty::Type::App(_, arg) = &*base_ty {
                    if let typeck::ty::Type::Row(fields, _) = &**arg {
                        if let Some(i) = fields.iter().position(|f| f.name == field.symbol) {
                            let op = self.convert_expr(base);
                            let op = self.builder.placed(op, lower_type(self.db, &base_ty));

                            ir::Operand::Place(op.field(i))
                        } else {
                            unreachable!();
                        }
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            },
            | hir::ExprKind::If { cond, then, else_ } => {
                let res = self.builder.create_tmp(ty);
                let res = ir::Place::new(res);
                let cond = self.convert_expr(cond);
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                self.builder.switch(cond, vec![0], vec![else_block, then_block]);
                self.builder.set_block(then_block);

                let then = self.convert_expr(then);

                self.builder.use_op(res.clone(), then);
                self.builder.jump(exit_block);
                self.builder.set_block(else_block);

                let else_ = self.convert_expr(else_);

                self.builder.use_op(res.clone(), else_);
                self.builder.jump(exit_block);
                self.builder.set_block(exit_block);

                ir::Operand::Place(res)
            },
            | hir::ExprKind::Case { pred, arms } => {
                let preds = pred
                    .iter()
                    .map(|e| {
                        let op = self.convert_expr(e);
                        let e_ty = &self.types.tys[&e.id];

                        self.builder.placed(op, lower_type(self.db, e_ty))
                    })
                    .collect();

                let case = self.convert_arms(preds, arms);

                self.compile_case(case, ty)
            },
            | hir::ExprKind::Do { block } => self.convert_block(block, ty),
            | hir::ExprKind::Typed { expr, .. } => self.convert_expr(expr),
            | _ => unimplemented!(),
        }
    }

    fn convert_block(&mut self, block: &hir::Block, ty: ir::Ty) -> ir::Operand {
        for (i, stmt) in block.stmts.iter().enumerate() {
            match &stmt.kind {
                | hir::StmtKind::Bind { binding } => {
                    let op = self.convert_expr(&binding.val);
                    let bind_ty = &self.types.tys[&binding.val.id];
                    let op = self.builder.placed(op, lower_type(self.db, bind_ty));

                    self.convert_binder_pat(op, &binding.pat);
                },
                | hir::StmtKind::Discard { expr } => {
                    let op = self.convert_expr(expr);

                    if i == block.stmts.len() - 1 {
                        return op;
                    }
                },
            }
        }

        ir::Operand::Const(ir::Const::Undefined(ty))
    }

    fn convert_binder_pat(&mut self, pred: ir::Place, pat: &hir::Pat) {
        if let Some(pat) = self.convert_pat(pat, pred) {
            self.bind_pat(pat);
        }
    }

    fn bind_pat(&mut self, pat: pattern::Pattern) {
        match pat {
            | pattern::Pattern::Bind(local, place) => {
                self.builder.use_op(ir::Place::new(local), ir::Operand::Place(place));
            },
            | pattern::Pattern::Seq(pats) => {
                for pat in pats {
                    self.bind_pat(pat);
                }
            },
            | _ => unreachable!(),
        }
    }

    fn convert_app(&mut self, base: &hir::Expr, args: Vec<&hir::Expr>, ty: ir::Ty) -> ir::Operand {
        match &base.kind {
            | hir::ExprKind::Ident {
                res: hir::Res::Def(hir::DefKind::Ctor, id),
                name,
            } => {
                let idx = if id.lib == self.db.lib() {
                    let file = self.db.module_tree(id.lib).file(id.module);
                    let hir = self.db.module_hir(file);
                    let ctor = hir.items[&(*id).into()].ctor();
                    let data = hir.items[&ctor.0].data_body();

                    data.iter().position(|id| hir.items[id].name.symbol == name.symbol).unwrap()
                } else {
                    let external = self.db.external_item_data(id.lib, id.module);

                    external.datas[id].iter().position(|(n, _)| n.symbol == name.symbol).unwrap()
                };

                let res = self.builder.create_tmp(ty);
                let res = ir::Place::new(res);
                let as_variant = res.clone().downcast(idx);

                for (i, arg) in args.iter().enumerate() {
                    let op = self.convert_expr(arg);

                    self.builder.use_op(as_variant.clone().field(i), op);
                }

                self.builder.set_discr(res.clone(), idx as u128);

                ir::Operand::Place(res)
            },
            | _ => {
                if let hir::ExprKind::Ident {
                    res: hir::Res::Def(hir::DefKind::Func, id),
                    name,
                } = &base.kind
                {
                    if id.lib != self.db.lib() {
                        let external = self.db.external_item_data(id.lib, id.module);

                        if external.is_intrinsic(id) {
                            return self.convert_intrinsic(name, args, ty);
                        } else if let ::hir::ExternalItemOrigin::ClassItem(_) = external.items[id].2 {
                            return self.convert_bound_app(&base.id, args, external.items[id].0, ty);
                        }
                    } else {
                        let file = self.db.module_tree(id.lib).file(id.module);
                        let hir = self.db.module_hir(file);
                        let item_id = (*id).into();

                        if let Some(item) = hir.items.get(&item_id) {
                            if item.is_intrinsic() {
                                return self.convert_intrinsic(&item.name, args, ty);
                            }
                        } else if let Some(item) = hir.class_items.get(&hir::ClassItemId(item_id)) {
                            return self.convert_bound_app(&base.id, args, item.name, ty);
                        }
                    }
                }

                let res = self.builder.create_tmp(ty.clone());
                let res = ir::Place::new(res);
                let base = self.convert_expr(base);
                let args = args.iter().map(|a| self.convert_expr(a)).collect();

                self.builder.call(vec![res.clone()], base, args);

                ir::Operand::Place(res)
            },
        }
    }

    fn convert_intrinsic(&mut self, name: &hir::Ident, args: Vec<&hir::Expr>, ty: ir::Ty) -> ir::Operand {
        let mut args = args.into_iter().map(|a| (self.convert_expr(a), lower_type(self.db, &self.types.tys[&a.id])));

        // @INTRINSICS
        match &**name.symbol {
            | "unsafe_read" => {
                let (arg, arg_ty) = args.next().unwrap();
                let place = self.builder.placed(arg, arg_ty);

                ir::Operand::Place(place.deref())
            },
            | "unsafe_store" => {
                let (ptr, ptr_ty) = args.next().unwrap();
                let val = args.next().unwrap().0;
                let place = self.builder.placed(ptr, ptr_ty);

                self.builder.use_op(place.deref(), val);

                ir::Operand::Const(ir::Const::Tuple(Vec::new()))
            },
            | _ => {
                let args = args.map(|(a, _)| a).collect();
                let res = self.builder.create_tmp(ty);
                let res = ir::Place::new(res);

                self.builder.intrinsic(res.clone(), name.to_string(), args);

                ir::Operand::Place(res)
            },
        }
    }

    fn convert_bound_app(&mut self, base_id: &hir::HirId, args: Vec<&hir::Expr>, item_name: hir::Ident, ty: ir::Ty) -> ir::Operand {
        let bound = &self.types.bounds[base_id];

        match bound.source {
            | typeck::BoundSource::Instance(id) => {
                let method = if id.lib == self.db.lib() {
                    let file = self.db.module_tree(id.lib).file(id.module);
                    let hir = self.db.module_hir(file);
                    let imp = hir.items[&id.into()].instance_body();
                    let method = imp.items.iter().find(|it| it.name.symbol == item_name.symbol).unwrap();

                    self.decls[&method.id.0.owner]
                } else {
                    let external = self.db.external_item_data(id.lib, id.module);
                    let item = external.instances[&id].iter().find(|(n, _)| n.symbol == item_name.symbol).unwrap();

                    self.decls[&item.1.owner]
                };

                let method = ir::Operand::Const(ir::Const::Addr(method));
                let args = args.iter().map(|a| self.convert_expr(a)).collect();
                let res = self.builder.create_tmp(ty.clone());
                let res = ir::Place::new(res);

                self.builder.call(vec![res.clone()], method, args);

                return ir::Operand::Place(res);
            },
        }
    }
}

fn is_func_ty(db: &dyn LowerDatabase, ty: &typeck::ty::Ty) -> bool {
    if let typeck::ty::Type::Ctor(id) = &**ty {
        *id == db.lang_items().fn_ty().owner
    } else {
        false
    }
}

fn is_recursive(ty: &typeck::ty::Ty, variants: &[typeck::ty::Variant]) -> bool {
    let mut res = false;

    variants.iter().for_each(|v| {
        v.tys.iter().for_each(|t| {
            t.everything(&mut |t| {
                res = res || t.equal(ty);
            });
        });
    });

    res
}

fn ty_get_args(db: &dyn LowerDatabase, mut ret: typeck::ty::Ty) -> (Vec<typeck::ty::Ty>, typeck::ty::Ty) {
    let mut args = Vec::new();

    while let typeck::ty::Type::App(b, r) = &*ret {
        match &**b {
            | typeck::ty::Type::App(f, a) if is_func_ty(db, f) => {
                args.push(a.clone());
                ret = r.clone();
            },
            | _ => break,
        }
    }

    (args, ret)
}

fn lower_type(db: &dyn LowerDatabase, ty: &typeck::ty::Ty) -> ir::Ty {
    lower_type_rec(db, ty, Vec::new(), 0)
}

fn lower_type_rec<'a>(db: &dyn LowerDatabase, ty: &'a typeck::ty::Ty, mut base: Vec<(&'a typeck::ty::Ty, usize)>, lvl: usize) -> ir::Ty {
    use typeck::ty::Type;

    for &(base, l) in &base {
        if base.equal(ty) {
            return ir::Ty::new(ir::Type::Recurse(lvl - l));
        }
    }

    match &**ty {
        | Type::Error => unreachable!(),
        | Type::Int(_) => unreachable!(),
        | Type::String(_) => unreachable!(),
        | Type::Unknown(_) => unreachable!(),
        | Type::Skolem(_, _, _, _) => unreachable!(),
        | Type::Row(_, _) => unreachable!(),
        | Type::KindApp(_, _) => unreachable!(),
        | Type::Var(var) => ir::Ty::new(ir::Type::Opaque(var.0.local_id.0.to_string())),
        | Type::ForAll(_, _, ty, _) => lower_type_rec(db, ty, base, lvl),
        | Type::Tuple(tys) => ir::Ty::new(ir::Type::Tuple(tys.iter().map(|t| lower_type_rec(db, t, base.clone(), lvl + 1)).collect())),
        | Type::Ctnt(_, ty) => lower_type_rec(db, ty, base, lvl),
        | Type::App(f, arg) => {
            let mut f2 = f;
            let mut args = vec![arg.clone()];

            while let Type::App(b, arg) = &**f2 {
                f2 = b;
                args.push(arg.clone());
            }

            args.reverse();

            match &**f2 {
                | Type::Ctor(def) => {
                    if *def == db.lang_items().fn_ty().owner {
                        let (args, ret) = ty_get_args(db, ty.clone());

                        ir::Ty::new(ir::Type::Func(ir::Signature {
                            params: args.iter().map(|t| lower_type_rec(db, t, base.clone(), lvl + 1)).collect(),
                            rets: vec![lower_type_rec(db, &ret, base, lvl + 1)],
                        }))
                    } else if *def == db.lang_items().ptr_ty().owner {
                        assert_eq!(args.len(), 1);

                        ir::Ty::new(ir::Type::Ptr(Box::new(lower_type_rec(db, &args[0], base, lvl + 1))))
                    } else if *def == db.lang_items().array_ty().owner {
                        unimplemented!();
                    } else if *def == db.lang_items().slice_ty().owner {
                        unimplemented!();
                    } else {
                        base.push((ty, lvl));
                        lower_type_ctor(db, ty, *def, args, base, lvl)
                    }
                },
                | _ => lower_type_rec(db, f2, base, lvl),
            }
        },
        | Type::Ctor(id) => {
            base.push((ty, lvl));
            lower_type_ctor(db, ty, *id, Vec::new(), base, lvl)
        },
    }
}

fn lower_type_ctor(
    db: &dyn LowerDatabase,
    orig_ty: &typeck::ty::Ty,
    id: hir::DefId,
    args: Vec<typeck::ty::Ty>,
    base: Vec<(&typeck::ty::Ty, usize)>,
    lvl: usize,
) -> ir::Ty {
    let (variants, repr) = if id.lib == db.lib() {
        let file = db.module_tree(id.lib).file(id.module);
        let hir = db.module_hir(file);
        let def = hir.def(id);
        let repr = if let hir::Def::Item(item) = def {
            item.repr().map(|s| s.to_string())
        } else {
            None
        };

        (db.variants(id).apply(args.into()), repr)
    } else {
        let external = db.external_types(id.lib, id.module);
        let external2 = db.external_item_data(id.lib, id.module);
        let repr = external2.repr(&id).map(|s| s.to_string());

        (external.variants[&id].clone().apply(args.into()), repr)
    };

    let recursive = is_recursive(orig_ty, &variants);

    let mut ty = if variants.is_empty() {
        if let Some(repr) = repr {
            return ir::Ty::new(match repr.as_str() {
                | "u8" => ir::Type::U8,
                | "u16" => ir::Type::U16,
                | "u32" => ir::Type::U32,
                | "u64" => ir::Type::U64,
                | "u128" => ir::Type::U128,
                | "i8" => ir::Type::I8,
                | "i16" => ir::Type::I16,
                | "i32" => ir::Type::I32,
                | "i64" => ir::Type::I64,
                | "i128" => ir::Type::I128,
                | "f32" => ir::Type::F32,
                | "f64" => ir::Type::F64,
                | _ => unreachable!("unknown repr {}", repr),
            });
        }

        unreachable!("data type with no constructors must have a repr attribute");
    } else if variants.len() == 1 {
        ir::Ty::new(ir::Type::Tuple(
            variants[0]
                .tys
                .iter()
                .map(|t| lower_type_rec(db, t, base.clone(), lvl + 1 + recursive as usize))
                .collect(),
        ))
    } else {
        let tys = variants
            .iter()
            .map(|v| {
                let tys = v
                    .tys
                    .iter()
                    .map(|t| lower_type_rec(db, t, base.clone(), lvl + 2 + recursive as usize))
                    .collect();

                ir::Ty::new(ir::Type::Tuple(tys))
            })
            .collect();

        ir::Ty::new(ir::Type::Tagged(tys))
    };

    if let Some(repr) = repr {
        use ir::layout::*;
        let target = db.target(id.lib);
        let target = target.triple();

        ty = ty.with_abi(match repr.as_str() {
            | "u8" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I8, false), target)),
            | "u16" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I16, false), target)),
            | "u32" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I32, false), target)),
            | "u64" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I64, false), target)),
            | "u128" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I128, false), target)),
            | "i8" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I8, true), target)),
            | "i16" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I16, true), target)),
            | "i32" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I32, true), target)),
            | "i64" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I64, true), target)),
            | "i128" => Abi::Scalar(Scalar::unit(Primitive::Int(Integer::I128, true), target)),
            | "f32" => Abi::Scalar(Scalar::unit(Primitive::F32, target)),
            | "f64" => Abi::Scalar(Scalar::unit(Primitive::F64, target)),
            | _ => unreachable!("unknown repr {}", repr),
        });
    }

    if recursive {
        ty = ir::Ty::new(ir::Type::Box(Box::new(ty)));
    }

    ty
}

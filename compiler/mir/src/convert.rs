use crate::builder::Builder;
use crate::{ir, MirDatabase};
use hir::ir as hir;
use std::collections::HashMap;
use std::sync::Arc;

pub fn convert(db: &dyn MirDatabase, lib: hir::LibId, id: hir::ModuleId) -> ir::Module {
    let file = db.module_tree(lib).file(id);
    let hir = db.module_hir(file);
    let mut converter = Converter::new(db);

    converter.convert(&hir);
    converter.finish()
}

pub struct Converter<'db> {
    db: &'db dyn MirDatabase,
    foreigns: Vec<ir::Foreign>,
    bodies: Vec<ir::Body>,
}

pub struct BodyConverter<'db> {
    db: &'db dyn MirDatabase,
    hir: &'db hir::Module,
    types: Arc<check::TypeCheckResult>,
    builder: Builder,
    locals: HashMap<hir::HirId, ir::Local>,
}

impl<'db> Converter<'db> {
    pub fn new(db: &'db dyn MirDatabase) -> Self {
        Converter {
            db,
            foreigns: Vec::new(),
            bodies: Vec::new(),
        }
    }

    pub fn finish(self) -> ir::Module {
        ir::Module {
            foreigns: self.foreigns,
            bodies: self.bodies,
        }
    }

    pub fn convert(&mut self, hir: &hir::Module) {
        for (_, item) in &hir.items {
            match &item.kind {
                hir::ItemKind::Func { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let conv =
                        BodyConverter::new(self.db, hir, ty, item.id.owner, ir::BodyKind::Func);

                    self.bodies.push(conv.convert(body));
                }
                hir::ItemKind::Const { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let conv =
                        BodyConverter::new(self.db, hir, ty, item.id.owner, ir::BodyKind::Const);

                    self.bodies.push(conv.convert(body));
                }
                hir::ItemKind::Static { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let conv =
                        BodyConverter::new(self.db, hir, ty, item.id.owner, ir::BodyKind::Static);

                    self.bodies.push(conv.convert(body));
                }
                hir::ItemKind::Foreign { kind, .. } => {
                    if !item.is_intrinsic() {
                        let kind = match kind {
                            hir::ForeignKind::Func => ir::ForeignKind::Func,
                            hir::ForeignKind::Static => ir::ForeignKind::Static,
                        };

                        self.foreigns.push(ir::Foreign {
                            def: item.id.owner,
                            kind,
                        });
                    }
                }
                _ => {}
            }
        }

        // @todo: convert impl items
    }
}

impl<'db> BodyConverter<'db> {
    pub fn new(
        db: &'db dyn MirDatabase,
        hir: &'db hir::Module,
        types: Arc<check::TypeCheckResult>,
        def: hir::DefId,
        kind: ir::BodyKind,
    ) -> Self {
        BodyConverter {
            db,
            hir,
            types,
            builder: Builder::new(def, kind),
            locals: HashMap::new(),
        }
    }

    pub fn convert(mut self, body: &hir::Body) -> ir::Body {
        self.create_header(&body.params);

        let entry = self.builder.create_block();
        let _ = self.builder.set_bock(entry);
        let res = self.convert_expr(&body.value);

        self.builder.use_op(ir::Place::local(ir::Local::RET), res);
        self.builder.return_();
        self.builder.finish()
    }

    fn create_header(&mut self, params: &[hir::Param]) {
        use check::ty::Type;
        let mut ty = &self.types.ty;
        let mut vars = None::<&check::ty::List<check::ty::TypeVar>>;

        match &**ty {
            Type::ForAll(vars2, ty2) => {
                ty = ty2;
                vars = Some(vars2);
            }
            _ => {}
        }

        if let Type::Func(param_tys, ret) = &**ty {
            self.builder.create_ret(ret.clone());

            for (param, ty) in params.iter().zip(param_tys) {
                let local = self.builder.create_arg(ty);

                self.locals.insert(param.id, local);
            }
        } else {
            self.builder.create_ret(ty.clone());
        }

        if let Some(vars) = vars {
            let type_info = self.db.lang_items().type_info();
            let type_info = self.db.typecheck(type_info.owner).ty.clone();
            let ptr_ty = self.db.lang_items().ptr_ty();
            let ptr_ty = ir::Ty::data(ptr_ty.owner);
            let ty = ir::Ty::app(ptr_ty.clone(), ptr_ty, vec![type_info].into());

            for var in vars {
                let local = self.builder.create_arg(ty.clone());

                self.locals.insert(var.0, local);
            }
        }
    }

    fn convert_expr(&mut self, expr: &hir::Expr) -> ir::Operand {
        let ty = self.types.tys[&expr.id].clone();

        match &expr.kind {
            hir::ExprKind::Error => unreachable!(),
            hir::ExprKind::Hole { .. } => ir::Operand::Const(ir::Const::Undefined, ty),
            hir::ExprKind::Ident { res } => match res {
                hir::Res::Error => unreachable!(),
                hir::Res::Def(d, id) => match d {
                    hir::DefKind::Func => ir::Operand::Const(ir::Const::FuncAddr(*id), ty),
                    hir::DefKind::Static => ir::Operand::Copy(ir::Place::static_(*id)),
                    _ => unimplemented!(),
                },
                hir::Res::Local(id) => ir::Operand::Copy(ir::Place::local(self.locals[id].clone())),
            },
            hir::ExprKind::Int { val } => ir::Operand::Const(ir::Const::Scalar(*val), ty),
            hir::ExprKind::Float { bits } => {
                ir::Operand::Const(ir::Const::Scalar(*bits as u128), ty)
            }
            hir::ExprKind::Char { val } => ir::Operand::Const(ir::Const::Scalar(*val as u128), ty),
            hir::ExprKind::Str { val } => {
                ir::Operand::Const(ir::Const::Bytes(val.clone().into_bytes().into()), ty)
            }
            hir::ExprKind::App { base, args } => self.convert_app(base, args, ty),
            hir::ExprKind::Tuple { exprs } => {
                if exprs.is_empty() {
                    ir::Operand::Const(ir::Const::Tuple(Vec::new()), ty)
                } else {
                    let res = self.builder.create_tmp(ty.clone());
                    let res = ir::Place::local(res);
                    let ops = exprs.iter().map(|e| self.convert_expr(e)).collect();

                    self.builder.init(res.clone(), ty, ops);
                    ir::Operand::Move(res)
                }
            }
            hir::ExprKind::Field { base, field } => {
                let base_ty = &self.types.tys[&base.id];

                if let check::ty::Type::Record(fields, _) = &**base_ty {
                    if let Some(i) = fields.iter().position(|f| f.name == field.symbol) {
                        let op = self.convert_expr(base);
                        let op = self.builder.placed(op);

                        ir::Operand::Copy(op.field(i))
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            }
            hir::ExprKind::Case { pred, arms } => {
                let preds = pred
                    .iter()
                    .map(|e| {
                        let op = self.convert_expr(e);

                        self.builder.placed(op)
                    })
                    .collect::<Vec<_>>();

                self.convert_arms(preds, arms, ty)
            }
            hir::ExprKind::Do { block } => self.convert_block(block, ty),
            _ => ir::Operand::Const(ir::Const::Undefined, ty),
        }
    }

    fn convert_block(&mut self, block: &hir::Block, ty: ir::Ty) -> ir::Operand {
        for (i, stmt) in block.stmts.iter().enumerate() {
            match &stmt.kind {
                hir::StmtKind::Bind { binding } => {
                    let op = self.convert_expr(&binding.val);
                    let op = self.builder.placed(op);
                    let block = self.builder.get_block();

                    self.convert_pat(op, &binding.pat, block, block);
                }
                hir::StmtKind::Discard { expr } => {
                    let op = self.convert_expr(expr);

                    if i == block.stmts.len() - 1 {
                        let res = self.builder.create_tmp(ty);
                        let res = ir::Place::local(res);

                        self.builder.use_op(res.clone(), op);

                        return ir::Operand::Move(res);
                    }
                }
            }
        }

        ir::Operand::Const(ir::Const::Undefined, ty)
    }

    fn convert_arms(
        &mut self,
        preds: Vec<ir::Place>,
        arms: &[hir::CaseArm],
        ty: ir::Ty,
    ) -> ir::Operand {
        let res = self.builder.create_tmp(ty);
        let res = ir::Place::local(res);
        let exit_block = self.builder.create_block();

        for arm in arms {
            let mut next = self.builder.create_block();

            for (pred, pat) in preds.iter().zip(&arm.pats) {
                next = self.convert_pat(pred.clone(), pat, next, exit_block);
            }

            if self.builder.term_unset() {
                self.builder.jump(next);
            }

            self.builder.set_bock(next);
            self.convert_guarded(&arm.val, res.clone(), exit_block);
            self.builder.jump(exit_block);
        }

        self.builder.jump(exit_block);
        self.builder.set_bock(exit_block);

        ir::Operand::Move(res)
    }

    fn convert_guarded(&mut self, guarded: &hir::Guarded, res: ir::Place, _exit_block: ir::Block) {
        match guarded {
            hir::Guarded::Unconditional(expr) => {
                let val = self.convert_expr(expr);

                self.builder.use_op(res, val);
            }
            hir::Guarded::Guarded(_) => unimplemented!(),
        }
    }

    fn convert_pat(
        &mut self,
        pred: ir::Place,
        pat: &hir::Pat,
        next_block: ir::Block,
        exit_block: ir::Block,
    ) -> ir::Block {
        let ty = self.types.tys[&pat.id].clone();

        match &pat.kind {
            hir::PatKind::Error => unreachable!(),
            hir::PatKind::Wildcard => next_block,
            hir::PatKind::Bind { sub: None, .. } => match pred.base {
                ir::PlaceBase::Local(local)
                    if pred.elems.is_empty() && self.builder.local_ty(local) == ty =>
                {
                    self.locals.insert(pat.id, local);
                    next_block
                }
                _ => {
                    let local = self.builder.create_var(ty);

                    self.locals.insert(pat.id, local);
                    self.builder
                        .use_op(ir::Place::local(local), ir::Operand::Move(pred));
                    next_block
                }
            },
            hir::PatKind::Ctor { ctor: _, pats } => {
                // @todo: implement this properly
                for (i, pat) in pats.iter().enumerate() {
                    self.convert_pat(pred.clone().field(i), pat, next_block, exit_block);
                }

                next_block
            }
            _ => next_block,
        }
    }

    fn convert_app(&mut self, base: &hir::Expr, args: &[hir::Expr], ty: ir::Ty) -> ir::Operand {
        match &base.kind {
            hir::ExprKind::Ident {
                res: hir::Res::Def(hir::DefKind::Ctor, _id),
            } => {
                unimplemented!();
            }
            hir::ExprKind::Ident {
                res: hir::Res::Def(hir::DefKind::Func, id),
            } if self.hir.items[&hir::HirId {
                owner: *id,
                local_id: hir::LocalId(0),
            }]
                .is_intrinsic() =>
            {
                let item = &self.hir.items[&hir::HirId {
                    owner: *id,
                    local_id: hir::LocalId(0),
                }];

                let mut args = args.iter().map(|a| self.convert_expr(a));

                // @INTRINSICS
                match &**item.name.symbol {
                    "unsafe_read" => {
                        let arg = args.next().unwrap();
                        let place = self.builder.placed(arg);

                        ir::Operand::Copy(place.deref())
                    }
                    _ => unreachable!("unknown intrinsic function"),
                }
            }
            _ => {
                use check::ty::{List, Ty, Type};
                let base_ty = self.types.tys[&base.id].clone();
                let res = self.builder.create_tmp(ty.clone());
                let res = ir::Place::local(res);
                let mut res2 = res.clone();
                let next = self.builder.create_block();
                let base_ = self.convert_expr(base);
                let mut arg_ops = args
                    .iter()
                    .map(|a| self.convert_expr(a))
                    .collect::<Vec<_>>();

                match &*base_ty {
                    Type::App(_, orig, targs) => {
                        if let Type::Func(arg_tys, ret_ty) = &**orig {
                            assert_eq!(arg_ops.len(), arg_tys.len());

                            let ptr_ty = self.db.lang_items().ptr_ty();
                            let ptr_ty = Ty::data(ptr_ty.owner);

                            for i in 0..arg_tys.len() {
                                if let Type::Var(_) = &*arg_tys[i] {
                                    let arg_ref = self.builder.create_tmp(Ty::app(
                                        ptr_ty.clone(),
                                        ptr_ty.clone(),
                                        vec![self.types.tys[&args[i].id].clone()].into(),
                                    ));

                                    let arg_ref = ir::Place::local(arg_ref);
                                    let arg = self.builder.placed(arg_ops[i].clone());

                                    self.builder.addrof(arg_ref.clone(), arg);
                                    arg_ops[i] = ir::Operand::Move(arg_ref);
                                }
                            }

                            if let Type::Var(_) = &**ret_ty {
                                let res_ref = self.builder.create_tmp(Ty::app(
                                    ptr_ty.clone(),
                                    ptr_ty,
                                    vec![ty].into(),
                                ));

                                let res_ref = ir::Place::local(res_ref);

                                self.builder.addrof(res_ref.clone(), res.clone());

                                let new_res = self.builder.create_tmp(Ty::tuple(List::new()));

                                res2 = ir::Place::local(new_res);
                                arg_ops.insert(0, ir::Operand::Move(res_ref));
                            }
                        }

                        for ty in targs {
                            arg_ops.push(self.db.type_info(base.id.owner.lib, ty));
                        }
                    }
                    _ => {}
                }

                self.builder.call(res2, base_, arg_ops, next);
                self.builder.set_bock(next);

                ir::Operand::Move(res)
            }
        }
    }
}

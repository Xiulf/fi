use crate::builder::Builder;
use crate::{ir, MirDatabase};
use hir::ir as hir;
use std::collections::HashMap;
use std::sync::Arc;

pub fn convert(db: &dyn MirDatabase, lib: hir::LibId, id: hir::ModuleId) -> Arc<ir::Module> {
    let file = db.module_tree(lib).file(id);
    let hir = db.module_hir(file);
    let mut converter = Converter::new(db);

    converter.convert(&hir);

    Arc::new(converter.finish())
}

pub struct Converter<'db> {
    db: &'db dyn MirDatabase,
    bodies: Vec<ir::Body>,
}

pub struct BodyConverter<'db> {
    db: &'db dyn MirDatabase,
    types: Arc<check::TypeCheckResult>,
    builder: Builder,
    locals: HashMap<hir::HirId, ir::Local>,
}

impl<'db> Converter<'db> {
    pub fn new(db: &'db dyn MirDatabase) -> Self {
        Converter {
            db,
            bodies: Vec::new(),
        }
    }

    pub fn finish(self) -> ir::Module {
        ir::Module {
            bodies: self.bodies,
        }
    }

    pub fn convert(&mut self, hir: &hir::Module) {
        for (_, item) in &hir.items {
            match &item.kind {
                hir::ItemKind::Func { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let conv = BodyConverter::new(self.db, ty, item.id.owner, ir::BodyKind::Func);

                    self.bodies.push(conv.convert(body));
                }
                hir::ItemKind::Const { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let conv = BodyConverter::new(self.db, ty, item.id.owner, ir::BodyKind::Const);

                    self.bodies.push(conv.convert(body));
                }
                hir::ItemKind::Static { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let conv = BodyConverter::new(self.db, ty, item.id.owner, ir::BodyKind::Static);

                    self.bodies.push(conv.convert(body));
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
        types: Arc<check::TypeCheckResult>,
        def: hir::DefId,
        kind: ir::BodyKind,
    ) -> Self {
        BodyConverter {
            db,
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
            let ty = ir::Ty::app(ptr_ty, vec![type_info].into());

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
            hir::ExprKind::App { base, args } => self.convert_app(base, args, ty),
            hir::ExprKind::Case { pred, arms } => {
                let preds = pred
                    .iter()
                    .map(|e| self.convert_expr(e))
                    .collect::<Vec<_>>();

                self.convert_arms(preds, arms, ty)
            }
            _ => ir::Operand::Const(ir::Const::Undefined, ty),
        }
    }

    fn convert_arms(
        &mut self,
        preds: Vec<ir::Operand>,
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

    fn convert_guarded(&mut self, guarded: &hir::Guarded, res: ir::Place, exit_block: ir::Block) {
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
        pred: ir::Operand,
        pat: &hir::Pat,
        next_block: ir::Block,
        exit_block: ir::Block,
    ) -> ir::Block {
        let ty = self.types.tys[&pat.id].clone();

        match &pat.kind {
            hir::PatKind::Error => unreachable!(),
            hir::PatKind::Wildcard => next_block,
            hir::PatKind::Bind { sub: None, .. } => {
                let local = self.builder.create_var(ty);

                self.locals.insert(pat.id, local);
                self.builder.use_op(ir::Place::local(local), pred);
                next_block
            }
            _ => next_block,
        }
    }

    fn convert_app(&mut self, base: &hir::Expr, args: &[hir::Expr], ty: ir::Ty) -> ir::Operand {
        if let hir::ExprKind::Ident {
            res: hir::Res::Def(hir::DefKind::Ctor, id),
        } = &base.kind
        {
            unimplemented!()
        } else {
            use check::ty::Type;
            let base_ty = self.types.tys[&base.id].clone();
            let res = self.builder.create_tmp(ty);
            let res = ir::Place::local(res);
            let next = self.builder.create_block();
            let base = self.convert_expr(base);
            let mut args = args
                .iter()
                .map(|a| self.convert_expr(a))
                .collect::<Vec<_>>();

            match &*base_ty {
                Type::App(_, targs) => {
                    for ty in targs {
                        args.push(self.db.type_info(ty));
                    }
                }
                _ => {}
            }

            self.builder.call(res.clone(), base, args, next);
            self.builder.set_bock(next);

            ir::Operand::Move(res)
        }
    }
}

use crate::*;
use check::tcx::Tcx;
use std::collections::HashMap;

pub fn convert<'tcx>(tcx: &Tcx<'tcx>, package: &hir::Package) -> Package<'tcx> {
    let mut converter = Converter::new(tcx, package.name);

    converter.convert(package);
    converter.finish()
}

pub struct Converter<'a, 'tcx> {
    tcx: &'a Tcx<'tcx>,
    package: Package<'tcx>,
}

pub(crate) struct BodyConverter<'a, 'tcx> {
    tcx: &'a Tcx<'tcx>,
    hir: &'a hir::Package,
    pub(crate) builder: Builder<'a, 'tcx>,
    locals: HashMap<hir::Id, LocalId>,
    loops: Vec<(Option<&'a hir::Id>, BlockId, BlockId)>,
    deferred: Vec<Vec<hir::Id>>,
}

impl<'a, 'tcx> Converter<'a, 'tcx> {
    pub fn new(tcx: &'a Tcx<'tcx>, name: hir::Symbol) -> Self {
        Converter {
            tcx,
            package: Package::new(name),
        }
    }

    pub fn finish(mut self) -> Package<'tcx> {
        crate::optimize::optimize(&mut self.package);
        // crate::lifetime::mark_lifetimes(&mut self.package);
        self.package
    }

    pub fn convert(&mut self, package: &hir::Package) {
        for (_, item) in &package.items {
            self.convert_item(package, item);
        }

        for (_, import) in &package.imports.0 {
            self.convert_import(import);
        }
    }

    pub fn convert_item(&mut self, package: &hir::Package, item: &hir::Item) {
        let attrs = item.attrs.clone();

        match &item.kind {
            hir::ItemKind::Extern { abi: _, ty } => {
                self.package
                    .declare_extern(item.id, attrs, item.name, self.tcx.type_of(ty))
            }
            hir::ItemKind::Func {
                params,
                ret,
                body,
                generics: _,
            } => {
                let mut params = params.clone();
                let func_ty = self.tcx.type_of(&item.id);

                let (param_tys, ret_ty) = if let Type::Forall(gparams, new_ty) = func_ty {
                    if item.is_poly() {
                        let mut param_tys = gparams
                            .iter()
                            .map(|_| self.tcx.builtin.typeid)
                            .collect::<Vec<_>>();
                        let (_, tparams, ret) = new_ty.func().unwrap();

                        params = gparams.iter().chain(params).collect();
                        param_tys.extend(tparams.iter().map(|p| p.ty));

                        (param_tys, ret)
                    } else {
                        unimplemented!("function instances");
                    }
                } else {
                    let (_, params, ret) = func_ty.func().unwrap();

                    (params.iter().map(|p| p.ty).collect(), ret)
                };

                self.package
                    .declare_body(item.id, attrs, item.name, &param_tys, ret_ty);

                let mut converter = BodyConverter {
                    tcx: self.tcx,
                    hir: package,
                    builder: self.package.define_body(item.id),
                    locals: HashMap::new(),
                    loops: Vec::new(),
                    deferred: Vec::new(),
                };

                converter.convert(&params, ret, body);
            }
            hir::ItemKind::Method {
                owner,
                self_param,
                params,
                ret,
                body,
                generics: _,
            } => {
                let mut params = params.clone();
                let func_ty = self.tcx.type_of(&item.id);

                params.insert(0, *self_param);

                let (param_tys, ret_ty) = if let Type::Forall(gparams, new_ty) = func_ty {
                    if item.is_poly() {
                        let mut param_tys = gparams
                            .iter()
                            .map(|_| self.tcx.builtin.typeid)
                            .collect::<Vec<_>>();
                        let (_, tparams, ret) = new_ty.func().unwrap();

                        params = gparams.iter().chain(params).collect();
                        param_tys.push(self.tcx.type_of(self_param));
                        param_tys.extend(tparams.iter().map(|p| p.ty));

                        (param_tys, ret)
                    } else {
                        unimplemented!("function instances");
                    }
                } else {
                    let (_, params, ret) = func_ty.func().unwrap();

                    (
                        std::iter::once(self.tcx.type_of(self_param))
                            .chain(params.iter().map(|p| p.ty))
                            .collect(),
                        ret,
                    )
                };

                self.package
                    .declare_body(item.id, attrs, item.name, &param_tys, ret_ty);

                let mut converter = BodyConverter {
                    tcx: self.tcx,
                    hir: package,
                    builder: self.package.define_body(item.id),
                    locals: HashMap::new(),
                    loops: Vec::new(),
                    deferred: Vec::new(),
                };

                converter.convert(&params, ret, body);
            }
            hir::ItemKind::Var {
                global: true,
                ty,
                val,
            } => {
                let ty = self.tcx.type_of(ty);

                self.package.declare_global(item.id, attrs, item.name, ty);

                if let Some(val) = val {
                    let tcx = self.tcx;

                    self.package.define_global(tcx, item.id, |builder| {
                        let mut converter = BodyConverter {
                            tcx,
                            hir: package,
                            builder,
                            locals: HashMap::new(),
                            loops: Vec::new(),
                            deferred: vec![Vec::new()],
                        };

                        let entry = converter.builder.create_block();

                        converter.builder.use_block(entry);

                        let res = converter.trans_expr(val);

                        converter.builder.use_(Place::local(LocalId::RET), res);
                        converter.builder.return_();
                    });
                }
            }
            _ => {}
        }
    }

    pub fn convert_import(&mut self, import: &hir::Import) {
        let attrs = import.attrs.clone();

        self.package
            .declare_extern(import.id, attrs, import.name, self.tcx.type_of(&import.id));
    }
}

impl<'a, 'tcx> BodyConverter<'a, 'tcx> {
    pub(crate) fn new(
        tcx: &'a Tcx<'tcx>,
        hir: &'a hir::Package,
        builder: Builder<'a, 'tcx>,
    ) -> Self {
        BodyConverter {
            tcx,
            hir,
            builder,
            locals: HashMap::new(),
            loops: Vec::new(),
            deferred: Vec::new(),
        }
    }

    fn convert(&mut self, params: &[Id], _ret: &hir::Id, body: &hir::Block) {
        let entry = self.builder.create_block();

        self.builder.use_block(entry);

        for (&id, param) in params.iter().zip(self.builder.body.params()) {
            self.locals.insert(id, param.id);
        }

        self.trans_block(body, LocalId(0));

        for id in params {
            self.builder.var_dead(self.locals[id]);
        }

        self.builder.return_();
    }

    fn trans_block(&mut self, block: &hir::Block, res: LocalId) -> Operand<'tcx> {
        let res = Place::local(res);
        let mut locals = Vec::new();

        self.deferred.push(Vec::new());

        for (i, stmt) in block.stmts.iter().enumerate() {
            match &stmt.kind {
                hir::StmtKind::Item(id) => {
                    if let hir::ItemKind::Var {
                        global: false, val, ..
                    } = &self.hir.items[id].kind
                    {
                        let var = self.builder.create_var(self.tcx.type_of(id));

                        self.locals.insert(*id, var);
                        locals.push(var);

                        if let Some(val) = val {
                            let val = self.trans_expr(val);

                            self.builder.use_(Place::local(var), val);
                        }
                    }
                }
                hir::StmtKind::Semi(id) => {
                    self.trans_expr(id);
                }
                hir::StmtKind::Expr(id) => {
                    let op = self.trans_expr(id);

                    if i == block.stmts.len() - 1 {
                        self.builder.use_(res.clone(), op);
                    }
                }
            }
        }

        for id in self.deferred.pop().unwrap().iter().rev() {
            self.trans_expr(id);
        }

        for id in locals {
            self.builder.var_dead(id);
        }

        Operand::Move(res)
    }

    pub(crate) fn trans_expr(&mut self, id: &hir::Id) -> Operand<'tcx> {
        let expr = &self.hir.exprs[id];

        match &expr.kind {
            hir::ExprKind::Err => unreachable!(),
            hir::ExprKind::Path { res } => match res {
                hir::Res::Module(_) => unreachable!(),
                hir::Res::Label(_) => unreachable!(),
                hir::Res::PrimTy(_) => unreachable!(),
                hir::Res::Item(id) => {
                    if let Some(item) = self.hir.items.get(id) {
                        match &item.kind {
                            hir::ItemKind::Func { .. } => {
                                Operand::Const(Const::FuncAddr(*id), self.tcx.type_of(id))
                            }
                            hir::ItemKind::Extern { ty, .. } => {
                                if self.tcx.type_of(ty).func().is_some() {
                                    Operand::Const(Const::FuncAddr(*id), self.tcx.type_of(id))
                                } else {
                                    Operand::Move(Place::global(*id))
                                }
                            }
                            hir::ItemKind::Var { global: true, .. } => {
                                Operand::Move(Place::global(*id))
                            }
                            hir::ItemKind::Ctor {
                                item,
                                variant,
                                params: None,
                            } => {
                                let ty = self.tcx.type_of(item);
                                let res = self.builder.create_tmp(ty);
                                let res = Place::local(res);

                                self.builder.init(res.clone(), ty, *variant, Vec::new());

                                Operand::Move(res)
                            }
                            hir::ItemKind::Const { val, .. } => {
                                let ty = self.tcx.type_of(&expr.id);

                                Operand::Const(
                                    crate::constant::eval_expr(
                                        self.tcx,
                                        self.hir,
                                        val,
                                        ty,
                                        &*self.tcx.subst_of(ty).unwrap(),
                                    ),
                                    ty,
                                )
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        match &self.hir.imports.0[id].kind {
                            hir::ImportKind::Func => {
                                Operand::Const(Const::FuncAddr(*id), self.tcx.type_of(id))
                            }
                            hir::ImportKind::Var => Operand::Move(Place::global(*id)),
                            hir::ImportKind::Extern { abi: _ } => {
                                if let Some(_) = self.tcx.type_of(id).func() {
                                    Operand::Const(Const::FuncAddr(*id), self.tcx.type_of(id))
                                } else {
                                    Operand::Move(Place::global(*id))
                                }
                            }
                            hir::ImportKind::Struct | hir::ImportKind::Enum => unreachable!(),
                        }
                    }
                }
                hir::Res::Local(id) => Operand::Copy(Place::local(self.locals[id])),
                hir::Res::PrimVal(prim) => match prim {
                    hir::PrimVal::True => Operand::Const(Const::Scalar(1), self.tcx.builtin.bool),
                    hir::PrimVal::False => Operand::Const(Const::Scalar(0), self.tcx.builtin.bool),
                    hir::PrimVal::Undefined => {
                        Operand::Const(Const::Undefined, self.tcx.type_of(id))
                    }
                },
            },
            hir::ExprKind::Apply { expr, .. } => self.trans_expr(expr),
            hir::ExprKind::Int { val } => Operand::Const(Const::Scalar(*val), self.tcx.type_of(id)),
            hir::ExprKind::Float { bits } => {
                Operand::Const(Const::Scalar(*bits as u128), self.tcx.type_of(id))
            }
            hir::ExprKind::Char { val } => {
                Operand::Const(Const::Scalar(*val as u128), self.tcx.type_of(id))
            }
            hir::ExprKind::String { val } => {
                Operand::Const(Const::Bytes(val.as_bytes().into()), self.tcx.builtin.str)
            }
            hir::ExprKind::Type { ty } => {
                Operand::Const(Const::Type(self.tcx.type_of(ty)), self.tcx.builtin.typeid)
            }
            hir::ExprKind::Array { exprs } => {
                let ty = self.tcx.type_of(id);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let ops = exprs.iter().map(|e| self.trans_expr(e)).collect();

                self.builder.init(res.clone(), ty, 0, ops);

                Operand::Move(res)
            }
            hir::ExprKind::Tuple { exprs } => {
                let ty = self.tcx.type_of(id);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let ops = exprs.iter().map(|e| self.trans_expr(e)).collect();

                self.builder.init(res.clone(), ty, 0, ops);

                Operand::Move(res)
            }
            hir::ExprKind::Range { lo, hi } => {
                let ty = self.tcx.type_of(id);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let lo = self.trans_expr(lo);
                let hi = self.trans_expr(hi);

                self.builder.init(res.clone(), ty, 0, vec![lo, hi]);

                Operand::Move(res)
            }
            hir::ExprKind::Block { block } => {
                let var = self.builder.create_tmp(self.tcx.type_of(id));

                self.trans_block(block, var)
            }
            hir::ExprKind::Call { func, args } => self.trans_call(func, args),
            hir::ExprKind::MethodCall { obj, method, args } => {
                self.trans_method_call(obj, method, args)
            }
            hir::ExprKind::Field { obj, field } => self.trans_field(obj, field),
            hir::ExprKind::Index { list, index } => {
                let mut list_ty = self.tcx.type_of(list);
                let list = self.trans_expr(list);
                let mut list = self.builder.placed(list, list_ty);

                while let Type::Ptr(check::ty::PtrKind::Single, to) = list_ty {
                    list_ty = to;
                    list = list.deref();
                }

                let index = self.trans_expr(index);

                Operand::Copy(list.index(index))
            }
            hir::ExprKind::Slice { list, low, high } => {
                let list_ty = self.tcx.type_of(list);
                let list = self.trans_expr(list);
                let list = self.builder.placed(list, list_ty);
                let low = if let Some(low) = low {
                    self.trans_expr(low)
                } else {
                    Operand::Const(Const::Scalar(0), self.tcx.builtin.usize)
                };

                let high = if let Some(high) = high {
                    self.trans_expr(high)
                } else if let Type::Array(_, len) = list_ty {
                    Operand::Const(Const::Scalar(*len as u128), self.tcx.builtin.usize)
                } else {
                    Operand::Move(list.clone().field(1))
                };

                Operand::Move(list.slice(low, high))
            }
            hir::ExprKind::Ref { expr } => {
                let ty = self.tcx.type_of(id);
                let expr_ty = self.tcx.type_of(expr);
                let expr = self.trans_expr(expr);
                let expr = self.builder.placed(expr, expr_ty);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);

                self.builder.ref_(res.clone(), expr);

                Operand::Move(res)
            }
            hir::ExprKind::Deref { expr } => {
                let expr_ty = self.tcx.type_of(expr);
                let expr = self.trans_expr(expr);
                let place = self.builder.placed(expr, expr_ty);

                Operand::Copy(place.deref())
            }
            hir::ExprKind::TypeOf { expr } => {
                let expr_ty = self.tcx.type_of(expr);

                Operand::Const(Const::Type(expr_ty), self.tcx.builtin.typeid)
            }
            hir::ExprKind::Cast { expr, ty } => {
                let ty = self.tcx.type_of(ty);
                let expr = self.trans_expr(expr);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let kind = match ty {
                    Type::Param(_) | Type::Object => CastKind::Object,
                    _ => CastKind::Misc,
                };

                self.builder.cast(res.clone(), ty, expr, kind);

                Operand::Move(res)
            }
            hir::ExprKind::Box { expr } => {
                let expr_ty = self.tcx.type_of(expr);
                let expr = self.trans_expr(expr);
                let ty = self.tcx.type_of(id);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let next = self.builder.create_block();
                let n = Operand::Const(
                    Const::Scalar(self.tcx.layout(expr_ty).size.bytes() as u128),
                    self.tcx.builtin.usize,
                );

                let box_alloc = self.tcx.lang_items.box_alloc().unwrap();
                let box_alloc =
                    Operand::Const(Const::FuncAddr(box_alloc), self.tcx.type_of(&box_alloc));

                self.builder.call(res.clone(), box_alloc, vec![n], next);
                self.builder.use_block(next);
                self.builder.use_(res.clone().deref(), expr);

                Operand::Move(res)
            }
            hir::ExprKind::Unbox { expr } => {
                let expr_ty = self.tcx.type_of(expr);
                let expr = self.trans_expr(expr);
                let expr = self.builder.placed(expr, expr_ty);
                let ty = self.tcx.type_of(id);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let next = self.builder.create_block();
                let unit = self.builder.create_tmp(self.tcx.builtin.unit);
                let unit = Place::local(unit);
                let box_free = self.tcx.lang_items.box_free().unwrap();
                let box_free =
                    Operand::Const(Const::FuncAddr(box_free), self.tcx.type_of(&box_free));

                self.builder
                    .use_(res.clone(), Operand::Copy(expr.clone().deref()));
                self.builder
                    .call(unit, box_free, vec![Operand::Move(expr)], next);
                self.builder.use_block(next);

                Operand::Move(res)
            }
            hir::ExprKind::Assign { lhs, rhs } => {
                let lhs_ty = self.tcx.type_of(lhs);
                let lhs = self.trans_expr(lhs);
                let lhs = self.builder.placed(lhs, lhs_ty);
                let rhs = self.trans_expr(rhs);

                self.builder.use_(lhs.clone(), rhs);

                Operand::Move(lhs)
            }
            hir::ExprKind::BinOp { op, lhs, rhs } => {
                let lhs = self.trans_expr(lhs);
                let rhs = self.trans_expr(rhs);
                let res = self.builder.create_tmp(self.tcx.type_of(id));
                let res = Place::local(res);
                let op = match op {
                    hir::BinOp::And => {
                        let true_block = self.builder.create_block();
                        let false_block = self.builder.create_block();
                        let exit_block = self.builder.create_block();

                        self.builder
                            .switch(lhs, vec![0], vec![false_block, true_block]);
                        self.builder.use_block(false_block);
                        self.builder.use_(
                            res.clone(),
                            Operand::Const(Const::Scalar(0), self.tcx.builtin.bool),
                        );
                        self.builder.jump(exit_block);
                        self.builder.use_block(true_block);
                        self.builder.use_(res.clone(), rhs);
                        self.builder.jump(exit_block);
                        self.builder.use_block(exit_block);

                        return Operand::Move(res);
                    }
                    hir::BinOp::Or => {
                        let true_block = self.builder.create_block();
                        let false_block = self.builder.create_block();
                        let exit_block = self.builder.create_block();

                        self.builder
                            .switch(lhs, vec![0], vec![false_block, true_block]);
                        self.builder.use_block(true_block);
                        self.builder.use_(
                            res.clone(),
                            Operand::Const(Const::Scalar(1), self.tcx.builtin.bool),
                        );
                        self.builder.jump(exit_block);
                        self.builder.use_block(false_block);
                        self.builder.use_(res.clone(), rhs);
                        self.builder.jump(exit_block);
                        self.builder.use_block(exit_block);

                        return Operand::Move(res);
                    }
                    hir::BinOp::Add => BinOp::Add,
                    hir::BinOp::Sub => BinOp::Sub,
                    hir::BinOp::Mul => BinOp::Mul,
                    hir::BinOp::Div => BinOp::Div,
                    hir::BinOp::Rem => BinOp::Rem,
                    hir::BinOp::Lt => BinOp::Lt,
                    hir::BinOp::Le => BinOp::Le,
                    hir::BinOp::Gt => BinOp::Gt,
                    hir::BinOp::Ge => BinOp::Ge,
                    hir::BinOp::Eq => BinOp::Eq,
                    hir::BinOp::Ne => BinOp::Ne,
                    hir::BinOp::BitAnd => BinOp::BitAnd,
                    hir::BinOp::BitOr => BinOp::BitOr,
                    hir::BinOp::BitXOr => BinOp::BitXOr,
                    hir::BinOp::Shl => BinOp::Shl,
                    hir::BinOp::Shr => BinOp::Shr,
                };

                self.builder.binop(res.clone(), op, lhs, rhs);

                Operand::Move(res)
            }
            hir::ExprKind::UnOp { op, rhs } => {
                let ty = self.tcx.type_of(id);
                let rhs = self.trans_expr(rhs);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let op = match op {
                    hir::UnOp::Neg => UnOp::Neg,
                    hir::UnOp::Not => UnOp::Not,
                };

                self.builder.unop(res.clone(), op, rhs);

                Operand::Move(res)
            }
            hir::ExprKind::IfElse { cond, then, else_ } => {
                let cond = self.trans_expr(cond);
                let then_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                if let Some(else_) = else_ {
                    let else_block = self.builder.create_block();
                    let res = self.builder.create_tmp(self.tcx.type_of(id));

                    self.builder
                        .switch(cond, vec![0], vec![else_block, then_block]);
                    self.builder.use_block(then_block);
                    self.trans_block(then, res);
                    self.builder.jump(exit_block);
                    self.builder.use_block(else_block);
                    self.trans_block(else_, res);
                    self.builder.jump(exit_block);
                    self.builder.use_block(exit_block);

                    Operand::Move(Place::local(res))
                } else {
                    self.builder
                        .switch(cond, vec![0], vec![exit_block, then_block]);
                    self.builder.use_block(then_block);

                    let then_ty = self.tcx.infer_block(then);
                    let then_ = self.builder.create_tmp(then_ty);

                    self.trans_block(then, then_);
                    self.builder.jump(exit_block);
                    self.builder.use_block(exit_block);

                    Operand::Const(Const::Tuple(Vec::new()), self.tcx.builtin.unit)
                }
            }
            hir::ExprKind::While { label, cond, body } => {
                let cond_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                self.loops.push((label.as_ref(), cond_block, exit_block));
                self.builder.jump(cond_block);
                self.builder.use_block(cond_block);

                let cond = self.trans_expr(cond);

                self.builder
                    .switch(cond, vec![0], vec![exit_block, body_block]);
                self.builder.use_block(body_block);

                let tmp = self.builder.create_tmp(self.tcx.builtin.unit);

                self.trans_block(body, tmp);
                self.builder.jump(cond_block);
                self.builder.use_block(exit_block);
                self.loops.pop().unwrap();

                Operand::Const(Const::Tuple(Vec::new()), self.tcx.builtin.unit)
            }
            hir::ExprKind::Defer { expr } => {
                self.deferred.last_mut().unwrap().push(*expr);

                Operand::Const(Const::Tuple(Vec::new()), self.tcx.builtin.unit)
            }
            _ => unimplemented!("{}", expr),
        }
    }

    fn trans_call(&mut self, func: &hir::Id, args: &[hir::Arg]) -> Operand<'tcx> {
        let func_ty = self.tcx.type_of(func);
        let args = self.trans_call_args(func_ty, args);

        if let hir::ExprKind::Path {
            res: hir::Res::Item(func_id),
        } = &self.hir.exprs[func].kind
        {
            if let Some(item) = self.hir.items.get(func_id) {
                if let hir::ItemKind::Ctor { variant, .. } = &item.kind {
                    let (_, _, ret_ty) = func_ty.func().unwrap();
                    let res = self.builder.create_tmp(ret_ty);
                    let res = Place::local(res);

                    self.builder.init(res.clone(), ret_ty, *variant, args);

                    return Operand::Move(res);
                }
            }
        }

        let func = self.trans_expr(func);

        self.trans_call_inner(func, func_ty, args)
    }

    fn trans_method_call(
        &mut self,
        obj: &hir::Id,
        method: &hir::Ident,
        args: &[hir::Arg],
    ) -> Operand<'tcx> {
        let obj_ty = self.tcx.type_of(obj);
        let obj_id = match obj_ty {
            Type::Struct(id, _) | Type::Enum(id, _) => id,
            _ => unreachable!(),
        };

        let method = self.tcx.find_method(obj_id, method.symbol).unwrap();
        let method_ty = self.tcx.type_of(&method);
        let obj = self.trans_expr(obj);
        let obj = self.builder.placed(obj, obj_ty);
        let obj_ref = self.builder.create_tmp(
            self.tcx
                .intern_ty(Type::Ptr(check::ty::PtrKind::Single, obj_ty)),
        );

        let obj_ref = Place::local(obj_ref);
        let _ = self.builder.ref_(obj_ref.clone(), obj);
        let mut args = self.trans_call_args(method_ty, args);

        args.insert(0, Operand::Move(obj_ref));

        let method = Operand::Const(Const::FuncAddr(method), method_ty);

        self.trans_call_inner(method, method_ty, args)
    }

    fn trans_call_args(&mut self, func_ty: Ty<'tcx>, args: &[hir::Arg]) -> Vec<Operand<'tcx>> {
        let (_, param_tys, _) = func_ty.func().unwrap();
        let mut call_args = Vec::with_capacity(args.len());
        let mut skip = Vec::with_capacity(args.len());

        for arg in args {
            if let Some(name) = &arg.name {
                let i = param_tys
                    .iter()
                    .position(|p| p.name.symbol == name.symbol)
                    .unwrap();

                skip.push(i);
                call_args.insert(i, self.trans_expr(&arg.value));
            }
        }

        for arg in args {
            if let None = &arg.name {
                let mut i = 0;

                while skip.contains(&i) {
                    i += 1;
                }

                skip.push(i);
                call_args.insert(i, self.trans_expr(&arg.value));
            }
        }

        call_args
    }

    fn trans_call_inner(
        &mut self,
        func: Operand<'tcx>,
        func_ty: Ty<'tcx>,
        mut args: Vec<Operand<'tcx>>,
    ) -> Operand<'tcx> {
        let (func_id, param_tys, ret_ty) = func_ty.func().unwrap();
        let res = self.builder.create_tmp(ret_ty);
        let res2 = res;
        let mut res = Place::local(res);

        if let Some(func_id) = func_id {
            let is_poly = if let Some(item) = self.hir.items.get(func_id) {
                item.is_poly()
            } else {
                self.hir.imports.0[func_id].is_poly()
            };

            let (_, param_tys_orig, ret_ty_orig) = match self.tcx.type_of(func_id) {
                Type::Func(a, b, c) => (a.as_ref(), *b, *c),
                Type::Forall(_, ty) => ty.func().unwrap(),
                _ => unreachable!(),
            };

            for (i, (curr, orig)) in param_tys.iter().zip(param_tys_orig.iter()).enumerate() {
                self.auto_cast(curr.ty, orig.ty, &mut args[i], is_poly);
            }

            if !ret_ty.is_object(is_poly) && ret_ty_orig.is_object(is_poly) {
                // return type is an object, but we expect a known type
                let obj = self.builder.create_tmp(ret_ty_orig);

                res = Place::local(obj);
            }

            if is_poly {
                if let Type::Forall(params, _) = self.tcx.type_of(func_id) {
                    let subst = self.tcx.subst_of(func_ty).unwrap();
                    let subst = params.iter().map(|p| subst[&p]);

                    args = subst
                        .map(|ty| Operand::Const(Const::Type(ty), self.tcx.builtin.typeid))
                        .chain(args)
                        .collect();
                }
            }
        }

        let next_block = self.builder.create_block();

        self.builder.call(res.clone(), func, args, next_block);
        self.builder.use_block(next_block);

        if let Some(func_id) = func_id {
            let is_poly = if let Some(item) = self.hir.items.get(func_id) {
                item.is_poly()
            } else {
                self.hir.imports.0[func_id].is_poly()
            };

            let (_, _, ret_ty_orig) = match self.tcx.type_of(func_id) {
                Type::Func(a, b, c) => (a.as_ref(), *b, *c),
                Type::Forall(_, ty) => ty.func().unwrap(),
                _ => unreachable!(),
            };

            if !ret_ty.is_object(is_poly) && ret_ty_orig.is_object(is_poly) {
                // return type is an object, but we expect a known type
                self.builder
                    .use_(Place::local(res2), Operand::Move(res.field(0).deref()));

                res = Place::local(res2);
            }
        }

        Operand::Move(res)
    }

    fn trans_field(&mut self, obj: &hir::Id, field: &hir::Ident) -> Operand<'tcx> {
        let mut obj_ty = self.tcx.type_of(obj);
        let obj = self.trans_expr(obj);

        if let Operand::Const(Const::Type(ty), _) = obj {
            if let Type::Param(id) = ty {
                let param_id = self.locals[id];
                let param = Place::local(param_id).deref();
                let param = if &**field.symbol == "size" {
                    param.field(0)
                } else if &**field.symbol == "align" {
                    param.field(1)
                } else if &**field.symbol == "stride" {
                    param.field(2)
                } else {
                    unreachable!();
                };

                Operand::Copy(param)
            } else {
                let layout = self.tcx.layout(ty);

                if &**field.symbol == "size" {
                    Operand::Const(
                        Const::Scalar(layout.size.bytes() as u128),
                        self.tcx.builtin.usize,
                    )
                } else if &**field.symbol == "align" {
                    Operand::Const(
                        Const::Scalar(layout.align.bytes() as u128),
                        self.tcx.builtin.usize,
                    )
                } else {
                    unreachable!();
                }
            }
        } else {
            let mut derefs = 0;

            while let Type::Ptr(check::ty::PtrKind::Single, to) = obj_ty {
                obj_ty = to;
                derefs += 1;
            }

            if let Type::Array(_, len) = obj_ty {
                if &**field.symbol == "len" {
                    Operand::Const(Const::Scalar(*len as u128), self.tcx.builtin.usize)
                } else {
                    unreachable!();
                }
            } else {
                let mut obj = self.builder.placed(obj, obj_ty);

                for _ in 0..derefs {
                    obj = obj.deref();
                }

                let fields = obj_ty.fields(self.tcx);
                let idx = fields
                    .into_iter()
                    .position(|(name, _)| name == field.symbol)
                    .unwrap();

                Operand::Copy(obj.field(idx))
            }
        }
    }

    fn auto_cast(&mut self, a: Ty<'tcx>, b: Ty<'tcx>, value: &mut Operand<'tcx>, is_poly: bool) {
        if !a.is_object(is_poly) && b.is_object(is_poly) {
            let obj = self.builder.create_tmp(b);

            self.builder
                .cast(Place::local(obj), b, value.clone(), CastKind::Object);

            *value = Operand::Move(Place::local(obj));
        }
    }
}

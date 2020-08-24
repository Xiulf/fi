use crate::*;
use check::tcx::Tcx;
use std::collections::HashMap;

pub fn convert<'tcx>(tcx: &Tcx<'tcx>, package: &hir::Package) -> Package<'tcx> {
    let mut converter = Converter::new(tcx);

    converter.convert(package);
    converter.finish()
}

pub struct Converter<'a, 'tcx> {
    tcx: &'a Tcx<'tcx>,
    package: Package<'tcx>,
}

struct BodyConverter<'a, 'tcx> {
    tcx: &'a Tcx<'tcx>,
    hir: &'a hir::Package,
    builder: Builder<'a, 'tcx>,
    locals: HashMap<hir::Id, LocalId>,
    loops: Vec<(Option<&'a hir::Id>, BlockId, BlockId)>,
}

impl<'a, 'tcx> Converter<'a, 'tcx> {
    pub fn new(tcx: &'a Tcx<'tcx>) -> Self {
        Converter {
            tcx,
            package: Package::new(),
        }
    }

    pub fn finish(mut self) -> Package<'tcx> {
        crate::optimize::optimize(&mut self.package);
        self.package
    }

    pub fn convert(&mut self, package: &hir::Package) {
        for (_, item) in &package.items {
            self.convert_item(package, item);
        }
    }

    pub fn convert_item(&mut self, package: &hir::Package, item: &hir::Item) {
        match &item.kind {
            hir::ItemKind::Extern { abi: _, ty } => {
                self.package
                    .declare_extern(item.id, item.name, self.tcx.type_of(ty))
            }
            hir::ItemKind::Func { params, ret, body } => {
                let (param_tys, ret_ty) = self.tcx.type_of(&item.id).func().unwrap();

                self.package
                    .declare_body(item.id, item.name, param_tys, ret_ty);

                let mut converter = BodyConverter {
                    tcx: self.tcx,
                    hir: package,
                    builder: self.package.define_body(item.id),
                    locals: HashMap::new(),
                    loops: Vec::new(),
                };

                converter.convert(params, ret, body);
            }
            hir::ItemKind::Var {
                global: true,
                ty,
                val,
            } => {
                let ty = self.tcx.type_of(ty);

                self.package.declare_global(item.id, item.name, ty);

                if let Some(val) = val {
                    let tcx = self.tcx;

                    self.package.define_global(tcx, item.id, |builder| {
                        let mut converter = BodyConverter {
                            tcx,
                            hir: package,
                            builder,
                            locals: HashMap::new(),
                            loops: Vec::new(),
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
}

impl<'a, 'tcx> BodyConverter<'a, 'tcx> {
    fn convert(&mut self, params: &[Id], _ret: &hir::Id, body: &hir::Block) {
        let entry = self.builder.create_block();

        self.builder.use_block(entry);

        for (&id, param) in params.iter().zip(self.builder.body.params()) {
            self.locals.insert(id, param.id);
        }

        self.trans_block(body, LocalId(0));
        self.builder.return_();
    }

    fn trans_block(&mut self, block: &hir::Block, res: LocalId) -> Operand<'tcx> {
        let res = Place::local(res);

        for (i, stmt) in block.stmts.iter().enumerate() {
            match &stmt.kind {
                hir::StmtKind::Item(id) => {
                    if let hir::ItemKind::Var {
                        global: false, val, ..
                    } = &self.hir.items[id].kind
                    {
                        let var = self.builder.create_var(self.tcx.type_of(id));

                        self.locals.insert(*id, var);

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

        Operand::Place(res)
    }

    fn trans_expr(&mut self, id: &hir::Id) -> Operand<'tcx> {
        let expr = &self.hir.exprs[id];

        match &expr.kind {
            hir::ExprKind::Err => unreachable!(),
            hir::ExprKind::Path { res } => match res {
                hir::Res::Module(_) => unreachable!(),
                hir::Res::Label(_) => unreachable!(),
                hir::Res::PrimTy(_) => unreachable!(),
                hir::Res::Item(id) => match &self.hir.items[id].kind {
                    hir::ItemKind::Func { .. } => Operand::Const(Const::FuncAddr(*id)),
                    hir::ItemKind::Extern { ty, .. } => {
                        if self.tcx.type_of(ty).func().is_some() {
                            Operand::Const(Const::FuncAddr(*id))
                        } else {
                            Operand::Place(Place::global(*id))
                        }
                    }
                    hir::ItemKind::Var { global: true, .. } => Operand::Place(Place::global(*id)),
                    _ => unreachable!(),
                },
                hir::Res::Local(id) => Operand::Place(Place::local(self.locals[id])),
                hir::Res::PrimVal(prim) => match prim {
                    hir::PrimVal::True => Operand::Const(Const::Scalar(1, self.tcx.builtin.bool)),
                    hir::PrimVal::False => Operand::Const(Const::Scalar(0, self.tcx.builtin.bool)),
                },
            },
            hir::ExprKind::Int { val } => Operand::Const(Const::Scalar(*val, self.tcx.type_of(id))),
            hir::ExprKind::Float { bits } => {
                Operand::Const(Const::Scalar(*bits as u128, self.tcx.type_of(id)))
            }
            hir::ExprKind::Char { val } => {
                Operand::Const(Const::Scalar(*val as u128, self.tcx.type_of(id)))
            }
            hir::ExprKind::String { val } => Operand::Const(Const::Bytes(val.as_bytes().into())),
            hir::ExprKind::Type { ty } => Operand::Const(Const::Type(self.tcx.type_of(ty))),
            hir::ExprKind::Array { exprs } => {
                let ty = self.tcx.type_of(id);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let ops = exprs.iter().map(|e| self.trans_expr(e)).collect();

                self.builder.init(res.clone(), ty, ops);

                Operand::Place(res)
            }
            hir::ExprKind::Tuple { exprs } => {
                let ty = self.tcx.type_of(id);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);
                let ops = exprs.iter().map(|e| self.trans_expr(e)).collect();

                self.builder.init(res.clone(), ty, ops);

                Operand::Place(res)
            }
            hir::ExprKind::Block { block } => {
                let var = self.builder.create_tmp(self.tcx.type_of(id));

                self.trans_block(block, var)
            }
            hir::ExprKind::Call { func, args } => self.trans_call(func, args),
            hir::ExprKind::Field { obj, field } => self.trans_field(obj, field),
            hir::ExprKind::Index { list, index } => {
                let list_ty = self.tcx.type_of(list);
                let list = self.trans_expr(list);
                let list = self.builder.placed(list, list_ty);
                let index = self.trans_expr(index);
                let index = self.builder.placed(index, self.tcx.builtin.usize);

                Operand::Place(list.index(index))
            }
            hir::ExprKind::Ref { expr } => {
                let ty = self.tcx.type_of(id);
                let expr_ty = self.tcx.type_of(expr);
                let expr = self.trans_expr(expr);
                let expr = self.builder.placed(expr, expr_ty);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);

                self.builder.ref_(res.clone(), expr);

                Operand::Place(res)
            }
            hir::ExprKind::Deref { expr } => {
                let expr_ty = self.tcx.type_of(expr);
                let expr = self.trans_expr(expr);
                let place = self.builder.placed(expr, expr_ty);

                Operand::Place(place.deref())
            }
            hir::ExprKind::TypeOf { expr } => {
                let expr_ty = self.tcx.type_of(expr);

                Operand::Const(Const::Type(expr_ty))
            }
            hir::ExprKind::Cast { expr, ty } => {
                let ty = self.tcx.type_of(ty);
                let expr = self.trans_expr(expr);
                let res = self.builder.create_tmp(ty);
                let res = Place::local(res);

                self.builder.cast(res.clone(), ty, expr);

                Operand::Place(res)
            }
            hir::ExprKind::Assign { lhs, rhs } => {
                let lhs_ty = self.tcx.type_of(lhs);
                let lhs = self.trans_expr(lhs);
                let lhs = self.builder.placed(lhs, lhs_ty);
                let rhs = self.trans_expr(rhs);

                self.builder.use_(lhs.clone(), rhs);

                Operand::Place(lhs)
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
                            Operand::Const(Const::Scalar(0, self.tcx.builtin.bool)),
                        );
                        self.builder.jump(exit_block);
                        self.builder.use_block(true_block);
                        self.builder.use_(res.clone(), rhs);
                        self.builder.jump(exit_block);
                        self.builder.use_block(exit_block);

                        return Operand::Place(res);
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
                            Operand::Const(Const::Scalar(1, self.tcx.builtin.bool)),
                        );
                        self.builder.jump(exit_block);
                        self.builder.use_block(false_block);
                        self.builder.use_(res.clone(), rhs);
                        self.builder.jump(exit_block);
                        self.builder.use_block(exit_block);

                        return Operand::Place(res);
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

                Operand::Place(res)
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

                Operand::Place(res)
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

                    Operand::Place(Place::local(res))
                } else {
                    self.builder
                        .switch(cond, vec![0], vec![exit_block, then_block]);
                    self.builder.use_block(then_block);

                    let then_ty = self.tcx.infer_block(then);
                    let then_ = self.builder.create_tmp(then_ty);

                    self.trans_block(then, then_);
                    self.builder.jump(exit_block);
                    self.builder.use_block(exit_block);

                    Operand::Const(Const::Tuple(Vec::new()))
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

                Operand::Const(Const::Tuple(Vec::new()))
            }
            _ => unimplemented!("{}", expr),
        }
    }

    fn trans_call(&mut self, func: &hir::Id, args: &[hir::Arg]) -> Operand<'tcx> {
        let (param_tys, ret_ty) = self.tcx.type_of(func).func().unwrap();
        let res = self.builder.create_tmp(ret_ty);
        let res = Place::local(res);
        let func = self.trans_expr(func);
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

        let next_block = self.builder.create_block();

        self.builder.call(res.clone(), func, call_args, next_block);
        self.builder.use_block(next_block);

        Operand::Place(res)
    }

    fn trans_field(&mut self, obj: &hir::Id, field: &hir::Ident) -> Operand<'tcx> {
        let obj_ty = self.tcx.type_of(obj);
        let obj = self.trans_expr(obj);

        if let Operand::Const(Const::Type(ty)) = obj {
            let layout = self.tcx.layout(ty);

            if &**field.symbol == "size" {
                Operand::Const(Const::Scalar(
                    layout.size.bytes() as u128,
                    self.tcx.builtin.usize,
                ))
            } else if &**field.symbol == "align" {
                Operand::Const(Const::Scalar(
                    layout.align.bytes() as u128,
                    self.tcx.builtin.usize,
                ))
            } else {
                unreachable!();
            }
        } else if let Type::Array(_, len) = obj_ty {
            if &**field.symbol == "len" {
                Operand::Const(Const::Scalar(*len as u128, self.tcx.builtin.usize))
            } else {
                unreachable!();
            }
        } else {
            let obj = self.builder.placed(obj, obj_ty);
            let fields = obj_ty.fields(self.tcx);
            let idx = fields
                .into_iter()
                .position(|(name, _)| name == field.symbol)
                .unwrap();

            Operand::Place(obj.field(idx))
        }
    }
}

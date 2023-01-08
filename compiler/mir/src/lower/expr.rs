use hir::id::{AssocItemId, HasModule};
use hir::{Expr, HasResolver, Literal, MethodSource, Resolver, ValueNs};

use super::*;
use crate::repr::Repr;

#[derive(Debug)]
pub enum Arg {
    ExprId(hir::ExprId),
    Op(Operand),
}

impl BodyLowerCtx<'_> {
    pub fn lower_expr(&mut self, expr: hir::ExprId, store_in: &mut Option<Place>) -> Operand {
        let body = self.body.clone();

        match body[expr] {
            | Expr::Missing | Expr::Hole => unreachable!(),
            | Expr::Typed { expr, .. } => self.lower_expr(expr, store_in),
            | Expr::Unit => Operand::Const(Const::Unit, Repr::unit()),
            | Expr::Lit { ref lit } => {
                let repr = self.db.repr_of(self.infer.type_of_expr[expr]);

                match *lit {
                    | Literal::Int(v) => Operand::Const(Const::Int(v), repr),
                    | Literal::Float(v) => Operand::Const(Const::Float(v), repr),
                    | Literal::Char(v) => Operand::Const(Const::Char(v), repr),
                    | Literal::String(ref v) => Operand::Const(Const::String(v.clone()), repr),
                }
            },
            | Expr::Array { ref exprs } => {
                let res = self.store_in(store_in, self.infer.type_of_expr[expr]);

                for (i, &expr) in exprs.iter().enumerate() {
                    let idx = (Const::Int(i as i128), Repr::usize());
                    let mut place = Some(res.clone().index(idx));
                    let op = self.lower_expr(expr, &mut place);

                    if let Some(place) = place {
                        self.builder.assign(place, op);
                    }
                }

                Operand::Move(res)
            },
            | Expr::Path { ref path } => self.lower_path(
                &Resolver::for_expr(self.db.upcast(), self.builder.origin().def, expr),
                (expr, 0),
                path,
                store_in,
            ),
            | Expr::Do { ref stmts } => self.lower_block(stmts, store_in),
            | Expr::App { mut base, arg } => {
                let mut args = vec![Arg::ExprId(arg)];

                while let Expr::App { base: b, arg } = body[base] {
                    base = b;
                    args.push(Arg::ExprId(arg));
                }

                args.reverse();
                self.lower_app(expr, Arg::ExprId(base), args, store_in)
            },
            | Expr::Infix { ref exprs, ref ops } => self.lower_infix_expr(expr, exprs, ops, store_in),
            | Expr::If {
                cond,
                then,
                else_: Some(else_),
            } => self.lower_if_else(expr, cond, then, else_, store_in),
            | Expr::If {
                cond,
                then,
                else_: None,
            } => self.lower_if(expr, cond, then),
            | Expr::Case { pred, ref arms } => self.lower_case(expr, pred, arms),
            | Expr::Return { expr: e } => {
                let repr = self.db.repr_of(self.infer.type_of_expr[expr]);
                let val = self.lower_expr(e, &mut None);

                self.builder.return_(val);

                Operand::Const(Const::Undefined, repr)
            },
            | ref e => todo!("{:?}", e),
        }
    }

    pub fn lower_block(&mut self, stmts: &[hir::Stmt], store_in: &mut Option<Place>) -> Operand {
        for (i, stmt) in stmts.iter().enumerate() {
            match *stmt {
                | hir::Stmt::Expr { expr } if i == stmts.len() - 1 => {
                    return self.lower_expr(expr, store_in);
                },
                | hir::Stmt::Expr { expr } => {
                    self.lower_expr(expr, &mut None);
                },
                | hir::Stmt::Let { pat, val } => {
                    let ty = self.infer.type_of_pat[pat];
                    let repr = self.db.repr_of(ty);
                    let local = self.builder.add_local(LocalKind::Var, repr);
                    self.builder.init(local);
                    let mut store_in = Some(Place::new(local));
                    let op = self.lower_expr(val, &mut store_in);

                    if store_in.is_some() {
                        self.builder.assign(Place::new(local), op);
                    }

                    self.define_pat(pat, Place::new(local));
                },
                | hir::Stmt::Bind { .. } => unreachable!(),
            }
        }

        Operand::Const(Const::Unit, Repr::unit())
    }

    pub fn lower_if(&mut self, _expr: hir::ExprId, cond: hir::ExprId, then: hir::ExprId) -> Operand {
        let cond = self.lower_expr(cond, &mut None);
        let cond = self.place_op(cond);
        let cond = self.get_discriminant(cond);
        let then_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder
            .switch(cond, vec![0], [exit_block.into(), then_block.into()]);
        self.builder.switch_block(then_block);
        self.lower_expr(then, &mut None);
        self.builder.jump(exit_block);
        self.builder.switch_block(exit_block);

        Operand::Const(Const::Unit, Repr::unit())
    }

    pub fn lower_if_else(
        &mut self,
        expr: hir::ExprId,
        cond: hir::ExprId,
        then: hir::ExprId,
        else_: hir::ExprId,
        _store_in: &mut Option<Place>,
    ) -> Operand {
        let cond = self.lower_expr(cond, &mut None);
        let cond = self.place_op(cond);
        let cond = self.get_discriminant(cond);
        let repr = self.db.repr_of(self.infer.type_of_expr[expr]);
        let res = self.builder.add_local(LocalKind::Arg, repr);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder
            .switch(cond, vec![0], [else_block.into(), then_block.into()]);
        self.builder.add_block_param(exit_block, res);
        self.builder.switch_block(then_block);
        let then = self.lower_expr(then, &mut None);
        self.builder.jump((exit_block, [then]));
        self.builder.switch_block(else_block);
        let else_ = self.lower_expr(else_, &mut None);
        self.builder.jump((exit_block, [else_]));
        self.builder.switch_block(exit_block);

        Operand::Move(Place::new(res))
    }

    pub fn lower_app(&mut self, expr: hir::ExprId, base: Arg, args: Vec<Arg>, store_in: &mut Option<Place>) -> Operand {
        if let Arg::ExprId(base) = base {
            let body = self.body.clone();

            if let Expr::Path { ref path } = body[base] {
                let resolver = Resolver::for_expr(self.db.upcast(), self.builder.origin().def, base);

                return self.lower_path_app(&resolver, expr, (base, 0), path, args, store_in);
            }
        }

        let _base = self.lower_arg(base, &mut None);

        todo!()
    }

    pub fn lower_path_app(
        &mut self,
        resolver: &Resolver,
        expr: hir::ExprId,
        base: (hir::ExprId, usize),
        path: &hir::Path,
        mut args: Vec<Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();
        let (mut base, params) = match resolved {
            | ValueNs::Local(id) => (Operand::Copy(self.locals[id].clone()), 1),
            | ValueNs::Fixity(id) => {
                let resolver = id.resolver(self.db.upcast());
                let data = self.db.fixity_data(id);

                return self.lower_path_app(&resolver, expr, base, &data.func, args, store_in);
            },
            | ValueNs::Func(id) => {
                let func = hir::Func::from(id);

                if func.is_intrinsic(self.db.upcast()) {
                    return self.lower_intrinsic(expr, &func.name(self.db.upcast()).to_string(), args, store_in);
                }

                (self.lower_path(resolver, base, path, &mut None), self.func_params(id))
            },
            | ValueNs::Ctor(id) => {
                let ty = self.infer.type_of_expr[expr];
                let res = self.store_in(store_in, ty);
                let downcast = if self.db.type_ctor_data(id.parent).ctors.len() == 1 {
                    res.clone()
                } else {
                    self.builder.set_discriminant(res.clone(), id.into());
                    res.clone().downcast(id.into())
                };

                for (i, arg) in args.into_iter().enumerate() {
                    let arg = self.lower_arg(arg, &mut None);
                    self.builder.assign(downcast.clone().field(i), arg);
                }

                return Operand::Move(res);
            },
            | _ => (self.lower_path(resolver, base, path, &mut None), args.len()),
        };

        if params > 0 {
            let repr = self.app_ret_ty(&base, params);
            let args2 = args
                .drain(..params)
                .map(|a| self.lower_arg(a, &mut None))
                .collect::<Vec<_>>();

            if args.is_empty() {
                let res = self.store_in_repr(store_in, repr);

                self.builder.call(res.clone(), base, args2);
                return Operand::Move(res);
            }

            let res = self.store_in_repr(&mut None, repr);

            self.builder.call(res.clone(), base, args2);
            base = Operand::Move(res);
        }

        if !args.is_empty() {
            self.lower_app(expr, Arg::Op(base), args, store_in)
        } else {
            base
        }
    }

    pub fn lower_path(
        &mut self,
        resolver: &Resolver,
        expr: (hir::ExprId, usize),
        path: &hir::Path,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();

        match resolved {
            | ValueNs::Local(id) => Operand::Copy(self.locals[id].clone()),
            | ValueNs::Ctor(id) => {
                let ty_ctor = self.db.type_ctor_data(id.parent);
                let ctor = &ty_ctor.ctors[id.local_id];

                if ctor.types.is_empty() {
                    let ty = self.infer.type_of_expr[expr.0];
                    // let repr = self.db.repr_of(ty);

                    // return Operand::Const(Const::Ctor(id), repr);
                    let res = self.store_in(store_in, ty);

                    if ty_ctor.ctors.len() != 1 {
                        self.builder.set_discriminant(res.clone(), id.into());
                    }

                    return Operand::Move(res);
                }

                todo!("reference ctor function")
            },
            | ValueNs::Func(id) => {
                let func = hir::Func::from(id);
                let infer = self.infer.clone();
                let is_method = func.as_assoc_item(self.db.upcast()).is_some();
                let mut methods = infer.methods.get(&expr).map(|m| m.iter().copied());
                let (mut base, sig) = if is_method {
                    let methods = methods.as_mut().expect(&format!("{:?}", expr));

                    match methods.next().unwrap() {
                        | MethodSource::Member(id) => {
                            let member = self.db.member_data(id);
                            let member_info = self.db.lower_member(id);
                            let func = match member.item(path.segments().last().unwrap()).unwrap() {
                                | AssocItemId::FuncId(id) => hir::Func::from(id),
                                | AssocItemId::StaticId(_) => unreachable!(),
                            };

                            let types = infer.instances.get(&expr.0).map(|t| &t[..]).unwrap_or_default();
                            let types = member_info.member.get_instance_types(self.db.upcast(), types);
                            let func = Instance::new(func.into(), types, methods.collect());
                            let sig = self.db.func_signature(func.clone());
                            let repr = Repr::Func(Box::new(sig.clone()), false);
                            let res = if sig.params.is_empty() {
                                self.store_in_repr(&mut None, repr)
                            } else {
                                self.store_in_repr(store_in, repr)
                            };

                            self.builder.instance_ref(res.clone(), func);
                            (Operand::Move(res), sig)
                        },
                        | MethodSource::Record(_idx, _p) => todo!(),
                    }
                } else {
                    let types = infer.instances.get(&expr.0);
                    let func = if types.is_some() || methods.is_some() {
                        Instance::new(
                            func.into(),
                            types.cloned().unwrap_or_default(),
                            methods.map(|c| c.collect()).unwrap_or_default(),
                        )
                    } else {
                        Instance::mono(func.into())
                    };

                    let sig = self.db.func_signature(func.clone());
                    let repr = Repr::Func(Box::new(sig.clone()), false);
                    let res = if sig.params.is_empty() {
                        self.store_in_repr(&mut None, repr)
                    } else {
                        self.store_in_repr(store_in, repr)
                    };

                    self.builder.instance_ref(res.clone(), func);
                    (Operand::Move(res), sig)
                };

                tracing::debug!("{}: {} ({:?})", path, sig.display(self.db.upcast()), expr);

                if sig.params.is_empty() {
                    let place = self.store_in_repr(store_in, sig.ret);
                    self.builder.call(place.clone(), base, []);
                    base = Operand::Move(place);
                }

                base
            },
            | _ => todo!("{}", path),
        }
    }

    pub fn lower_infix_expr(
        &mut self,
        expr: hir::ExprId,
        exprs: &[hir::ExprId],
        ops: &[hir::Path],
        store_in: &mut Option<Place>,
    ) -> Operand {
        use std::iter::{once, Enumerate, Peekable};
        let exprs = exprs.iter().map(|e| Arg::ExprId(*e));
        let resolver = Resolver::for_expr(self.db.upcast(), self.builder.origin().def, expr);
        let db = self.db;
        let fixities = ops.iter().map(|op| {
            let (resolved, _, _) = resolver.resolve_value(db.upcast(), op).unwrap();

            match resolved {
                | ValueNs::Fixity(id) => match db.fixity_data(id).kind {
                    | hir::FixityKind::Infix { assoc, prec } => (id, assoc, prec),
                    | _ => unreachable!(),
                },
                | _ => unreachable!(),
            }
        });

        return go(
            self,
            fixities.peekable(),
            exprs,
            ops.iter().enumerate(),
            expr,
            &resolver,
            store_in,
            true,
        );

        fn go<'a>(
            ctx: &mut BodyLowerCtx,
            mut fixities: Peekable<impl Iterator<Item = (hir::id::FixityId, hir::Assoc, hir::Prec)>>,
            mut exprs: impl Iterator<Item = Arg>,
            mut ops: Enumerate<impl Iterator<Item = &'a hir::Path>>,
            id: hir::ExprId,
            resolver: &Resolver,
            store_in: &mut Option<Place>,
            top_level: bool,
        ) -> Operand {
            if let Some((fix, assoc, prec)) = fixities.next() {
                let (i, op) = ops.next().unwrap();
                let left = if let Some((fix2, _, prec2)) = fixities.peek() {
                    if fix == *fix2 {
                        match assoc {
                            | hir::Assoc::Left => true,
                            | hir::Assoc::Right => false,
                            | hir::Assoc::None => true,
                        }
                    } else {
                        prec >= *prec2
                    }
                } else {
                    let lhs = exprs.next().unwrap();
                    let rhs = exprs.next().unwrap();

                    if top_level {
                        return ctx.lower_path_app(resolver, id, (id, i), op, vec![lhs, rhs], store_in);
                    } else {
                        return ctx.lower_path_app(resolver, id, (id, i), op, vec![lhs, rhs], &mut None);
                    }
                };

                if left {
                    let lhs = exprs.next().unwrap();
                    let rhs = exprs.next().unwrap();
                    let exp = ctx.lower_path_app(resolver, id, (id, i), op, vec![lhs, rhs], &mut None);
                    let exprs = once(Arg::Op(exp)).chain(exprs).collect::<Vec<_>>();

                    go(ctx, fixities, exprs.into_iter(), ops, id, resolver, store_in, top_level)
                } else {
                    let lhs = exprs.next().unwrap();
                    let rhs = go(ctx, fixities, exprs, ops, id, resolver, store_in, false);

                    if top_level {
                        ctx.lower_path_app(resolver, id, (id, i), op, vec![lhs, Arg::Op(rhs)], store_in)
                    } else {
                        ctx.lower_path_app(resolver, id, (id, i), op, vec![lhs, Arg::Op(rhs)], &mut None)
                    }
                }
            } else {
                if top_level {
                    ctx.lower_arg(exprs.next().unwrap(), store_in)
                } else {
                    ctx.lower_arg(exprs.next().unwrap(), &mut None)
                }
            }
        }
    }

    pub(super) fn place_op(&mut self, op: Operand) -> Place {
        match op {
            | Operand::Move(p) | Operand::Copy(p) => p,
            | Operand::Const(c, repr) => {
                let local = self.builder.add_local(LocalKind::Tmp, repr.clone());

                self.builder.init(local);
                self.builder.assign(Place::new(local), Operand::Const(c, repr));
                Place::new(local)
            },
        }
    }

    pub(super) fn lower_arg(&mut self, arg: Arg, store_in: &mut Option<Place>) -> Operand {
        match arg {
            | Arg::ExprId(e) => self.lower_expr(e, store_in),
            | Arg::Op(op) => op,
        }
    }

    pub fn store_in(&mut self, place: &mut Option<Place>, ty: Ty) -> Place {
        match place.take() {
            | Some(place) => place,
            | None => {
                let repr = self.db.repr_of(ty);
                let res = self.builder.add_local(LocalKind::Tmp, repr);
                self.builder.init(res);
                Place::new(res)
            },
        }
    }

    pub fn store_in_repr(&mut self, place: &mut Option<Place>, repr: Repr) -> Place {
        match place.take() {
            | Some(place) => place,
            | None => {
                let res = self.builder.add_local(LocalKind::Tmp, repr);
                self.builder.init(res);
                Place::new(res)
            },
        }
    }

    pub fn get_discriminant(&mut self, place: Place) -> Operand {
        let repr = self.builder.place_repr(&place).discr();
        let local = self.builder.add_local(LocalKind::Tmp, repr);

        self.builder.discriminant(Place::new(local), place);

        Operand::Move(Place::new(local))
    }

    fn app_ret_ty(&self, base: &Operand, _params: usize) -> Repr {
        let base = self.builder.operand_repr(base);

        match base {
            | Repr::Func(sig, _) => sig.ret,
            | _ => unreachable!(),
        }
    }

    fn func_params(&self, id: hir::id::FuncId) -> usize {
        if self.db.func_data(id).has_body {
            let body = self.db.body(id.into());

            body.params().len()
        } else {
            use hir::id::Lookup;
            let infer = self.db.infer(id.into());
            let mut ty = infer.self_type.ty;

            loop {
                match ty.lookup(self.db.upcast()) {
                    | hir::ty::TyKind::ForAll(_, inner, _) => {
                        ty = inner;
                    },
                    | hir::ty::TyKind::Where(_, inner) => {
                        ty = inner;
                    },
                    | _ => break,
                }
            }

            let lib = id.lookup(self.db.upcast()).module(self.db.upcast()).lib;
            let func_ctor = self.db.lang_item(lib, "fn-type").unwrap().as_type_ctor().unwrap();
            let mut params = 0;

            while let Some([_, ret]) = ty.match_ctor(self.db.upcast(), func_ctor).as_deref() {
                params += 1;
                ty = *ret;
            }

            params
        }
    }
}

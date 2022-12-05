use hir::id::{AssocItemId, DefWithBodyId, HasModule};
use hir::{Expr, HasResolver, Literal, MethodSource, Resolver, ValueNs};

use super::*;
use crate::repr::Repr;

pub enum Arg {
    ExprId(hir::ExprId),
    Op(Operand),
}

impl BodyLowerCtx<'_> {
    pub fn lower_expr(&mut self, expr: hir::ExprId) -> Operand {
        let body = self.body.clone();

        match body[expr] {
            | Expr::Missing | Expr::Hole => unreachable!(),
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
            | Expr::Path { ref path } => self.lower_path(
                &Resolver::for_expr(self.db.upcast(), self.builder.origin().def, expr),
                (expr, 0),
                path,
            ),
            | Expr::Do { ref stmts } => self.lower_block(stmts),
            | Expr::App { mut base, arg } => {
                let mut args = vec![Arg::ExprId(arg)];

                while let Expr::App { base: b, arg } = body[base] {
                    base = b;
                    args.push(Arg::ExprId(arg));
                }

                args.reverse();
                self.lower_app(expr, Arg::ExprId(base), args)
            },
            | Expr::Infix { ref exprs, ref ops } => self.lower_infix_expr(expr, exprs, ops),
            | Expr::Case { pred, ref arms } => self.lower_case(expr, pred, arms),
            | ref e => todo!("{:?}", e),
        }
    }

    pub fn lower_block(&mut self, stmts: &[hir::Stmt]) -> Operand {
        for (i, stmt) in stmts.iter().enumerate() {
            match *stmt {
                | hir::Stmt::Expr { expr } if i == stmts.len() - 1 => {
                    return self.lower_expr(expr);
                },
                | hir::Stmt::Expr { expr } => {
                    self.lower_expr(expr);
                },
                | hir::Stmt::Let { pat, val } => {
                    let ty = self.infer.type_of_pat[pat];
                    let repr = self.db.repr_of(ty);
                    let local = self.builder.add_local(LocalKind::Var, repr);
                    let op = self.lower_expr(val);

                    self.builder.assign(Place::new(local), op);
                    self.define_pat(pat, Place::new(local));
                },
                | hir::Stmt::Bind { .. } => unreachable!(),
            }
        }

        Operand::Const(Const::Unit, Repr::unit())
    }

    pub fn lower_app(&mut self, expr: hir::ExprId, base: Arg, args: Vec<Arg>) -> Operand {
        if let Arg::ExprId(base) = base {
            let body = self.body.clone();

            if let Expr::Path { ref path } = body[base] {
                let resolver = Resolver::for_expr(self.db.upcast(), self.builder.origin().def, base);

                return self.lower_path_app(&resolver, (expr, 0), path, args);
            }
        }

        let _base = self.lower_arg(base);

        todo!()
    }

    pub fn lower_path_app(
        &mut self,
        resolver: &Resolver,
        expr: (hir::ExprId, usize),
        path: &hir::Path,
        mut args: Vec<Arg>,
    ) -> Operand {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();
        let (mut base, params) = match resolved {
            | ValueNs::Local(id) => (Operand::Copy(self.locals[id].clone()), 1),
            | ValueNs::Fixity(id) => {
                let resolver = id.resolver(self.db.upcast());
                let data = self.db.fixity_data(id);

                return self.lower_path_app(&resolver, expr, &data.func, args);
            },
            | ValueNs::Func(id) => {
                let func = hir::Func::from(id);

                if func.is_intrinsic(self.db.upcast()) {
                    return self.lower_intrinsic(&func.name(self.db.upcast()).to_string(), args);
                }

                (self.lower_path(resolver, expr, path), self.func_params(id))
            },
            | ValueNs::Ctor(id) => {
                let ty = self.infer.type_of_expr[expr.0];
                let repr = self.db.repr_of(ty);
                let res = self.builder.add_local(LocalKind::Tmp, repr);
                self.builder.init(res);
                let downcast = if self.db.type_ctor_data(id.parent).ctors.len() == 1 {
                    Place::new(res)
                } else {
                    self.builder.set_discriminant(Place::new(res), id.into());
                    Place::new(res).downcast(id.into())
                };

                for (i, arg) in args.into_iter().enumerate() {
                    let arg = self.lower_arg(arg);
                    self.builder.assign(downcast.clone().field(i), arg);
                }

                return Operand::Move(Place::new(res));
            },
            | _ => (self.lower_path(resolver, expr, path), args.len()),
        };

        if params > 0 {
            let args2 = args.drain(..params).map(|a| self.lower_arg(a)).collect::<Vec<_>>();
            let ty = self.infer.type_of_expr[expr.0];
            let repr = self.db.repr_of(ty);
            let res = self.builder.add_local(LocalKind::Tmp, repr);

            self.builder.call(Place::new(res), base, args2);
            base = Operand::Move(Place::new(res));
        }

        if !args.is_empty() {
            self.lower_app(expr.0, Arg::Op(base), args)
        } else {
            base
        }
    }

    pub fn lower_path(&mut self, resolver: &Resolver, expr: (hir::ExprId, usize), path: &hir::Path) -> Operand {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();

        match resolved {
            | ValueNs::Local(id) => Operand::Copy(self.locals[id].clone()),
            | ValueNs::Func(id) => {
                let func = hir::Func::from(id);
                let infer = self.infer.clone();
                let is_method = func.as_assoc_item(self.db.upcast()).is_some();
                let mut methods = infer.methods.get(&expr).map(|m| m.iter().copied());
                let base = if is_method {
                    let methods = methods.as_mut().unwrap();

                    match methods.next().unwrap() {
                        | MethodSource::Member(id) => {
                            let member = self.db.member_data(id);
                            let item = match member.item(path.segments().last().unwrap()).unwrap() {
                                | AssocItemId::FuncId(id) => DefWithBodyId::FuncId(id),
                                | AssocItemId::StaticId(id) => DefWithBodyId::StaticId(id),
                            };

                            let ty = self.db.value_ty(item.into()).ty;
                            let repr = self.db.repr_of(ty);
                            let res = self.builder.add_local(LocalKind::Tmp, repr);

                            self.builder.def_ref(Place::new(res), item.into());

                            Operand::Move(Place::new(res))
                        },
                        | MethodSource::Record(_idx, _p) => todo!(),
                    }
                } else {
                    let ty = self.db.value_ty(id.into()).ty;
                    let repr = self.db.repr_of(ty);
                    let res = self.builder.add_local(LocalKind::Tmp, repr);

                    self.builder.def_ref(Place::new(res), func.into());

                    Operand::Move(Place::new(res))
                };

                base
            },
            | _ => todo!("{}", path),
        }
    }

    pub fn lower_infix_expr(&mut self, expr: hir::ExprId, exprs: &[hir::ExprId], ops: &[hir::Path]) -> Operand {
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
        );

        fn go<'a>(
            ctx: &mut BodyLowerCtx,
            mut fixities: Peekable<impl Iterator<Item = (hir::id::FixityId, hir::Assoc, hir::Prec)>>,
            mut exprs: impl Iterator<Item = Arg>,
            mut ops: Enumerate<impl Iterator<Item = &'a hir::Path>>,
            id: hir::ExprId,
            resolver: &Resolver,
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

                    return ctx.lower_path_app(resolver, (id, i), op, vec![lhs, rhs]);
                };

                if left {
                    let lhs = exprs.next().unwrap();
                    let rhs = exprs.next().unwrap();
                    let exp = ctx.lower_path_app(resolver, (id, i), op, vec![lhs, rhs]);
                    let exprs = once(Arg::Op(exp)).chain(exprs).collect::<Vec<_>>();

                    go(ctx, fixities, exprs.into_iter(), ops, id, resolver)
                } else {
                    let lhs = exprs.next().unwrap();
                    let rhs = go(ctx, fixities, exprs, ops, id, resolver);

                    ctx.lower_path_app(resolver, (id, i), op, vec![lhs, Arg::Op(rhs)])
                }
            } else {
                ctx.lower_arg(exprs.next().unwrap())
            }
        }
    }

    pub(super) fn place_op(&mut self, op: Operand) -> Place {
        match op {
            | Operand::Move(p) | Operand::Copy(p) => p,
            | Operand::Const(c, repr) => {
                let local = self.builder.add_local(LocalKind::Tmp, repr.clone());

                self.builder.assign(Place::new(local), Operand::Const(c, repr));
                Place::new(local)
            },
        }
    }

    pub(super) fn lower_arg(&mut self, arg: Arg) -> Operand {
        match arg {
            | Arg::ExprId(e) => self.lower_expr(e),
            | Arg::Op(op) => op,
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

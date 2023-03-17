use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::ValueDefId;
use hir_def::pat::{DecisionTree, PatId, VariantTag};
use salsa::AsId;

use super::*;
use crate::instance::Instance;
use crate::repr::Repr;

pub enum Arg {
    ExprId(ExprId),
    Op(Operand),
}

impl Ctx<'_> {
    pub fn lower_expr(&mut self, id: ExprId, store_in: &mut Option<Place>) -> Operand {
        let op = self.lower_expr_inner(id, store_in);

        if let Some(store_in) = store_in.take() {
            self.builder.assign(store_in, op.clone());
        }

        op
    }

    fn lower_expr_inner(&mut self, id: ExprId, store_in: &mut Option<Place>) -> Operand {
        let body = self.body.clone();

        match body[id] {
            | Expr::Missing => unreachable!(),
            | Expr::Hole(_) => unreachable!(),
            | Expr::Unit => (Const::Unit, Arc::new(Repr::unit())).into(),
            | Expr::Lit { ref lit } => self.lower_lit(id, lit),
            | Expr::Path { def: Some(def), .. } => self.lower_path(id, def, store_in),
            | Expr::App { base, ref args } => self.lower_app(id, base, args, store_in),
            | Expr::Block { ref stmts, expr } => self.lower_block(stmts, expr, store_in),
            | Expr::Lambda {
                ref env,
                ref params,
                body,
            } => self.lower_lambda(id, env, params, body, store_in),
            | Expr::Match {
                expr,
                ref branches,
                ref decision_tree,
            } => self.lower_match(id, expr, branches, decision_tree, store_in),
            | ref e => todo!("{e:?}"),
        }
    }

    fn lower_lit(&mut self, id: ExprId, lit: &Literal) -> Operand {
        let repr = repr_of(self.db, self.infer.type_of_expr[id]);

        match *lit {
            | Literal::Int(l) => (Const::Int(l), repr).into(),
            | Literal::Float(l) => (Const::Float(l), repr).into(),
            | Literal::Char(l) => (Const::Char(l), repr).into(),
            | Literal::String(ref l) => (Const::String(l.clone()), repr).into(),
        }
    }

    fn lower_block(&mut self, stmts: &[Stmt], expr: Option<ExprId>, store_in: &mut Option<Place>) -> Operand {
        for &stmt in stmts {
            self.lower_stmt(stmt);
        }

        match expr {
            | Some(expr) => self.lower_expr(expr, store_in),
            | None => (Const::Unit, Arc::new(Repr::unit())).into(),
        }
    }

    fn lower_stmt(&mut self, stmt: Stmt) {
        match stmt {
            | Stmt::Let(pat, expr) => {
                let repr = repr_of(self.db, self.infer.type_of_pat[pat]);
                let local = self.builder.add_local(LocalKind::Var, repr);
                self.lower_expr(expr, &mut Some(Place::new(local)));
                self.locals.insert(pat, Place::new(local));
            },
            | Stmt::Expr(expr) => {
                self.lower_expr_inner(expr, &mut None);
            },
        }
    }

    fn lower_path(&mut self, id: ExprId, def: ValueDefId, _store_in: &mut Option<Place>) -> Operand {
        let repr = repr_of(self.db, self.infer.type_of_expr[id]);
        let mir_id = match def {
            | ValueDefId::PatId(id) => return self.locals[id].clone().into(),
            | ValueDefId::ValueId(id) => MirValueId::ValueId(id),
            | ValueDefId::CtorId(id) => {
                if Ctor::from(id).types(self.db).is_empty() {
                    return (Const::Ctor(id), repr).into();
                }

                MirValueId::CtorId(id)
            },
            | _ => todo!("{def:?}"),
        };

        let instance = Instance::new(self.db, mir_id, None);

        (Const::Instance(instance), repr).into()
    }

    fn lower_app(&mut self, id: ExprId, base: ExprId, args: &[ExprId], store_in: &mut Option<Place>) -> Operand {
        let args = args.iter().copied().map(Arg::ExprId).collect();
        if let Expr::Path { def: Some(def), .. } = self.body[base] {
            return self.lower_path_app(id, base, def, args, store_in);
        }

        let func = self.lower_expr(base, &mut None);
        self.make_app(id, func, args, store_in)
    }

    fn lower_path_app(
        &mut self,
        expr: ExprId,
        base_expr: ExprId,
        def: ValueDefId,
        args: Vec<Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        match def {
            | ValueDefId::PatId(pat) => {
                let place = self.locals[pat].clone();
                self.make_app(expr, place.into(), args, store_in)
            },
            | ValueDefId::ValueId(id) => {
                if Value::from(id).is_intrinsic(self.db) {
                    return self.lower_intrinsic(expr, Value::from(id).name(self.db), args, store_in);
                }

                let func = self.lower_path(base_expr, def, store_in);
                self.make_app(expr, func, args, store_in)
            },
            | ValueDefId::CtorId(id) => {
                let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                let ret = self.store_in(store_in, ret_repr);
                self.builder.init(ret.local);
                let downcast = if Ctor::from(id).type_ctor(self.db).ctors(self.db).len() == 1 {
                    ret.clone()
                } else {
                    ret.clone().downcast(id)
                };

                for (i, arg) in args.into_iter().enumerate() {
                    let arg = self.lower_arg(arg);
                    self.builder.assign(downcast.clone().field(i), arg);
                }

                ret.into()
            },
            | ValueDefId::FixityId(id) => {
                let data = hir_def::data::fixity_data(self.db, id);
                if let Some(def) = data.def(self.db).and_then(|d| d.left()) {
                    self.lower_path_app(expr, base_expr, def, args, store_in)
                } else {
                    let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                    (Const::Undefined, ret_repr).into()
                }
            },
            | _ => todo!("{def:?}"),
        }
    }

    fn make_app(&mut self, id: ExprId, func: Operand, args: Vec<Arg>, store_in: &mut Option<Place>) -> Operand {
        let args = args.into_iter().map(|a| self.lower_arg(a)).collect::<Vec<_>>();
        let ret_repr = repr_of(self.db, self.infer.type_of_expr[id]);
        let ret = self.store_in(store_in, ret_repr);

        self.builder.call(ret.clone(), func, args);
        Operand::Copy(ret)
    }

    fn lower_lambda(
        &mut self,
        expr: ExprId,
        env: &[PatId],
        params: &[PatId],
        body: ExprId,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let mut ctx = self.for_lambda(expr);
        let env_ty = match self.infer.type_of_expr[expr].kind(self.db) {
            | hir_ty::ty::TyKind::Func(func) => func.env,
            | _ => unreachable!(),
        };

        let env_repr = repr_of(self.db, env_ty);
        let env_repr = Arc::new(Repr::Box(env_repr));
        let env_param = ctx.builder.add_local(LocalKind::Arg, env_repr.clone());
        let entry = ctx.builder.create_block();

        ctx.builder.switch_block(entry);
        ctx.builder.add_block_param(entry, env_param);

        for &param in params.iter() {
            let param_repr = repr_of(self.db, self.infer.type_of_pat[param]);
            let local = ctx.builder.add_local(LocalKind::Arg, param_repr);
            ctx.builder.add_block_param(entry, local);
            ctx.locals.insert(param, Place::new(local));
        }

        let env_place = Place::new(env_param).deref();

        if env.len() == 1 {
            ctx.locals.insert(env[0], env_place);
        } else {
            for (i, &pat) in env.iter().enumerate() {
                ctx.locals.insert(pat, env_place.clone().field(i));
            }
        }

        let res = ctx.lower_expr(body, &mut None);
        ctx.builder.ret(res);
        let body = ctx.builder.build(self.db);
        self.lambdas.push((expr, body));
        self.lambdas.append(&mut ctx.lambdas);
        let instance = Instance::new(self.db, MirValueId::Body(body), None);
        let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr]);
        let func_repr = match &*ret_repr {
            | Repr::Func(_, None) => return (Const::Instance(instance), ret_repr).into(),
            | Repr::Func(sig, _) => Arc::new(Repr::Func(sig.clone(), None)),
            | _ => unreachable!(),
        };

        let var = self.store_in(store_in, ret_repr);
        let func = (Const::Instance(instance), func_repr);
        let env_field = var.clone().field(1).deref();

        self.builder.init(var.local);
        self.builder.assign(var.clone().field(0), func);

        if env.len() == 1 {
            self.builder.assign(env_field, self.locals[env[0]].clone());
        } else {
            for (i, &pat) in env.iter().enumerate() {
                let val = self.locals[pat].clone();
                self.builder.assign(env_field.clone().field(i), val);
            }
        }

        var.into()
    }

    fn lower_match(
        &mut self,
        expr: ExprId,
        value: ExprId,
        branches: &[(PatId, ExprId)],
        tree: &DecisionTree,
        _store_in: &mut Option<Place>,
    ) -> Operand {
        let pred = self.lower_expr(value, &mut None);
        let pred = self.place_op(pred);
        let blocks = branches.iter().map(|_| self.builder.create_block()).collect::<Vec<_>>();
        self.lower_decision_tree(pred, &blocks, tree);
        let exit_block = self.builder.create_block();

        for (&(_, branch), block) in branches.iter().zip(blocks) {
            self.builder.switch_block(block);
            let op = self.lower_expr(branch, &mut None);
            self.builder.jump((exit_block, [op]));
        }

        let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr]);
        let ret = self.builder.add_local(LocalKind::Arg, ret_repr);
        self.builder.add_block_param(exit_block, ret);
        self.builder.switch_block(exit_block);
        Place::new(ret).into()
    }

    fn lower_decision_tree(&mut self, pred: Place, blocks: &[Block], tree: &DecisionTree) {
        match tree {
            | DecisionTree::Fail => {
                self.builder.abort();
            },
            | DecisionTree::Leaf(i) => {
                self.builder.jump(blocks[*i]);
            },
            | DecisionTree::Guard(_, _) => todo!(),
            | DecisionTree::Switch(_pat, cases) => {
                let mut default_branch = None;
                let mut switch = self.builder.switch();
                let block = self.builder.current_block();
                let discr = pred.clone();

                for case in cases {
                    let Some(tag) = &case.tag else {
                        assert!(case.fields.len() <= 1);
                        for pats in case.fields.iter() {
                            for &pat in pats {
                                self.locals.insert(pat, pred.clone());
                            }
                        }

                        default_branch = Some(self.lower_case_branch(blocks, &case.branch));
                        break;
                    };

                    let value = match tag {
                        | VariantTag::Literal(lit) => match lit {
                            | Literal::Int(l) => *l,
                            | _ => todo!(),
                        },
                        | VariantTag::Ctor(id) => {
                            let downcast = if Ctor::from(*id).type_ctor(self.db).ctors(self.db).len() == 1 {
                                pred.clone()
                            } else {
                                pred.clone().downcast(*id)
                            };

                            for (i, pats) in case.fields.iter().enumerate() {
                                for &pat in pats {
                                    self.locals.insert(pat, downcast.clone().field(i));
                                }
                            }

                            id.as_id().as_u32() as i128
                        },
                    };

                    let branch = self.lower_case_branch(blocks, &case.branch);

                    switch.branch(value, branch);
                }

                let default_branch = default_branch.unwrap_or_else(|| {
                    let block = self.builder.create_block();
                    let old = self.builder.switch_block(block).unwrap();
                    self.builder.unreachable();
                    self.builder.switch_block(old);
                    block
                });

                self.builder.switch_block(block);
                switch.build(&mut self.builder, discr, default_branch);
            },
        }
    }

    fn lower_case_branch(&mut self, blocks: &[Block], tree: &DecisionTree) -> Block {
        match tree {
            | DecisionTree::Leaf(i) => blocks[*i],
            | _ => {
                let block = self.builder.create_block();
                self.builder.switch_block(block);
                block
            },
        }
    }

    pub(super) fn lower_arg(&mut self, arg: Arg) -> Operand {
        match arg {
            | Arg::ExprId(id) => self.lower_expr(id, &mut None),
            | Arg::Op(op) => op,
        }
    }

    pub(super) fn store_in(&mut self, store_in: &mut Option<Place>, repr: Arc<Repr>) -> Place {
        match store_in.take() {
            | Some(place) => place,
            | None => {
                let local = self.builder.add_local(LocalKind::Tmp, repr);
                Place::new(local)
            },
        }
    }

    pub(super) fn place_op(&mut self, op: Operand) -> Place {
        match op {
            | Operand::Copy(place) | Operand::Move(place) => place,
            | Operand::Const(_, ref repr) => {
                let res = self.builder.add_local(LocalKind::Tmp, repr.clone());
                self.builder.assign(Place::new(res), op);
                Place::new(res)
            },
        }
    }
}

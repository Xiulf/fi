use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::{ContainerId, ValueDefId};
use hir_def::pat::{DecisionTree, Pat, PatId, VariantTag};
use hir_ty::ty::{InstanceImpl, Ty, TyKind};

use super::*;
use crate::instance::{ImplInstance, ImplSource, Instance, InstanceId, Subst};
use crate::repr::{Repr, ReprKind};

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
            | Expr::Path { def: None, .. } => unreachable!(),
            | Expr::Recur => self.lower_recur(id, store_in),
            | Expr::Typed { expr, ty: _ } => self.lower_expr(expr, store_in),
            | Expr::Unit => (Const::Unit, Repr::unit(self.db)).into(),
            | Expr::Path { def: Some(def), .. } => self.lower_path(id, def, store_in),
            | Expr::Lit { ref lit } => self.lower_lit(id, lit),
            | Expr::Array { ref exprs } => self.lower_array(id, exprs, store_in),
            | Expr::App { base, ref args } => self.lower_app(id, base, args, store_in),
            | Expr::Block { ref stmts, expr } => self.lower_block(stmts, expr, store_in),
            | Expr::Lambda {
                ref env,
                ref params,
                body,
            } => self.lower_lambda(id, env, params, body, store_in),
            | Expr::If {
                cond,
                then,
                else_: None,
            } => self.lower_if(cond, then),
            | Expr::If {
                cond,
                then,
                else_: Some(else_),
            } => self.lower_if_else(id, cond, then, else_, store_in),
            | Expr::Match {
                expr,
                ref branches,
                ref decision_tree,
            } => self.lower_match(id, expr, branches, decision_tree, store_in),
            | Expr::Return { expr } => self.lower_return(expr),
        }
    }

    fn lower_lit(&mut self, id: ExprId, lit: &Literal) -> Operand {
        let repr = repr_of(self.db, self.infer.type_of_expr[id], ReprPos::Argument);

        match *lit {
            | Literal::Int(l) => (Const::Int(l), repr).into(),
            | Literal::Float(l) => (Const::Float(l), repr).into(),
            | Literal::Char(l) => (Const::Char(l), repr).into(),
            | Literal::String(ref l) => (Const::String(l.clone()), repr).into(),
        }
    }

    fn lower_recur(&mut self, id: ExprId, _store_in: &mut Option<Place>) -> Operand {
        let repr = repr_of(self.db, self.infer.type_of_expr[id], ReprPos::Argument);
        let ty_inst = self.infer.instances.get(id).cloned().unwrap_or_default();
        let mir_id = InstanceId::MirValueId(self.id);
        let mut impls_iter = ty_inst.impls.into_iter();
        let mut impls = Vec::new();
        let types = ty_inst.types;

        while let Some(src) = impls_iter.next() {
            impls.push(self.lower_impl_source(src, &mut impls_iter, &types));
        }

        let subst = Subst { types, impls };
        let instance = Instance::new(self.db, mir_id, Some(subst).filter(|s| !s.is_empty()));

        (Const::Instance(instance), repr).into()
    }

    fn lower_array(&mut self, id: ExprId, exprs: &[ExprId], store_in: &mut Option<Place>) -> Operand {
        let repr = repr_of(self.db, self.infer.type_of_expr[id], ReprPos::Argument);
        let place = self.store_in(store_in, repr);
        let usize = Repr::usize(self.db);

        for (i, &expr) in exprs.iter().enumerate() {
            let index = Operand::Const(Const::Int(i as i128), usize);
            let place = place.clone().index(index);
            self.lower_expr(expr, &mut Some(place));
        }

        place.into()
    }

    fn lower_block(&mut self, stmts: &[Stmt], expr: Option<ExprId>, store_in: &mut Option<Place>) -> Operand {
        for &stmt in stmts {
            self.lower_stmt(stmt);
        }

        match expr {
            | Some(expr) => self.lower_expr(expr, store_in),
            | None => (Const::Unit, Repr::unit(self.db)).into(),
        }
    }

    fn lower_stmt(&mut self, stmt: Stmt) {
        match stmt {
            | Stmt::Let(pat, expr) => {
                let op = self.lower_expr(expr, &mut None);
                let place = self.place_op(op);
                self.bind_pat(pat, place);
            },
            | Stmt::Expr(expr) => {
                self.lower_expr_inner(expr, &mut None);
            },
        }
    }

    pub(super) fn bind_pat(&mut self, pat: PatId, place: Place) {
        let body = self.body.clone();
        match body[pat] {
            | Pat::Missing => unreachable!(),
            | Pat::Ctor { ctor: None, .. } => unreachable!(),
            | Pat::Wildcard | Pat::Bind { subpat: None, .. } | Pat::Lit { .. } => {},
            | Pat::Typed { pat, .. } => self.bind_pat(pat, place.clone()),
            | Pat::Bind {
                subpat: Some(subpat), ..
            } => self.bind_pat(subpat, place.clone()),
            | Pat::Ctor {
                ctor: Some(ctor),
                ref args,
                ..
            } => {
                let place = if Ctor::from(ctor).type_ctor(self.db).is_boxed(self.db) {
                    place.clone().deref().field(1)
                } else {
                    place.clone()
                };

                for (i, &arg) in args.iter().enumerate() {
                    self.bind_pat(arg, place.clone().field(i));
                }
            },
        }

        self.locals.insert(pat, place);
    }

    fn lower_path(&mut self, expr: ExprId, def: ValueDefId, store_in: &mut Option<Place>) -> Operand {
        let pos = match def {
            | ValueDefId::PatId(_) => ReprPos::Argument,
            | _ => ReprPos::TopLevel,
        };
        let repr = repr_of(self.db, self.infer.type_of_expr[expr], pos);
        let (mir_id, ty_inst) = match def {
            | ValueDefId::PatId(id) => return self.locals[id].clone().into(),
            | ValueDefId::ValueId(id) => match self.infer.methods.get(expr) {
                | Some(&method) => {
                    let impl_id = match method.container(self.db) {
                        | ContainerId::ImplId(id) => id,
                        | _ => unreachable!(),
                    };
                    let inst = self.infer.instances[expr].adjust_for_impl(self.db, impl_id);
                    (InstanceId::MirValueId(MirValueId::ValueId(method)), inst)
                },
                | None => match self.infer.instances.get(expr).and_then(|i| i.impls.get(0)) {
                    | Some(&InstanceImpl::Param(idx)) => match id.container(self.db) {
                        | ContainerId::TraitId(_) => {
                            let constraint = &self.builder.constraints()[idx];
                            let trait_id = constraint.trait_id;
                            let methods = hir::Trait::from(trait_id).items(self.db);
                            let method = methods.iter().position(|m| m.id() == id).unwrap();
                            let mut inst = self.infer.instances[expr].clone();
                            inst.types.drain(..constraint.args.len());
                            inst.impls.remove(0);

                            (InstanceId::VtableMethod(self.id, idx, method), inst)
                        },
                        | _ => (MirValueId::ValueId(id).into(), self.infer.instances[expr].clone()),
                    },
                    | _ => match id.container(self.db) {
                        | ContainerId::TraitId(_) => panic!("cannot create instance of a trait method"),
                        | _ if hir::Value::from(id).is_intrinsic(self.db) => {
                            return self.lower_intrinsic(expr, Value::from(id).name(self.db), Vec::new(), store_in);
                        },
                        | _ => (
                            MirValueId::ValueId(id).into(),
                            self.infer.instances.get(expr).cloned().unwrap_or_default(),
                        ),
                    },
                },
            },
            | ValueDefId::CtorId(id) => {
                if Ctor::from(id).types(self.db).is_empty() {
                    if !Ctor::from(id).type_ctor(self.db).is_boxed(self.db) {
                        return (Const::Ctor(id), repr).into();
                    }

                    let res = self.builder.add_local(LocalKind::Tmp, repr);

                    self.builder.init(res);
                    self.builder.set_discriminant(Place::new(res).deref().field(1), id);

                    return Place::new(res).into();
                }

                (MirValueId::CtorId(id).into(), self.infer.instances[expr].clone())
            },
            | _ => todo!("{def:?}"),
        };

        let mut impls_iter = ty_inst.impls.into_iter();
        let mut impls = Vec::new();
        let types = ty_inst.types;

        while let Some(src) = impls_iter.next() {
            impls.push(self.lower_impl_source(src, &mut impls_iter, &types));
        }

        let subst = Subst { types, impls };
        let instance = Instance::new(self.db, mir_id, Some(subst).filter(|s| !s.is_empty()));

        (Const::Instance(instance), repr).into()
    }

    fn lower_impl_source(
        &mut self,
        imp: InstanceImpl,
        impls_iter: &mut impl Iterator<Item = InstanceImpl>,
        types: &[Ty],
    ) -> ImplSource {
        match imp {
            | InstanceImpl::ImplId(id) => {
                let imp = hir::Impl::from(id);
                let vars = imp.bind_vars(self.db, types);
                let impls = imp
                    .constraints(self.db)
                    .iter()
                    .map(|c| {
                        let types = c
                            .args
                            .iter()
                            .map(|t| t.replace_vars(self.db, &vars))
                            .collect::<Vec<_>>();
                        self.lower_impl_source(impls_iter.next().unwrap(), impls_iter, &types)
                    })
                    .collect();

                let subst = Subst {
                    types: imp.type_vars(self.db).into_iter().map(|v| vars[&v.id()]).collect(),
                    impls,
                };

                ImplSource::Instance(ImplInstance::new(self.db, id, Some(subst).filter(|s| !s.is_empty())))
            },
            | InstanceImpl::Param(idx) => ImplSource::Param(idx),
        }
    }

    fn lower_app(&mut self, id: ExprId, base: ExprId, args: &[ExprId], store_in: &mut Option<Place>) -> Operand {
        let args = args.iter().copied().map(Arg::ExprId).collect();
        if let Expr::Path { def: Some(def), .. } = self.body[base] {
            return self.lower_path_app(id, base, def, args, store_in);
        }

        let func = self.lower_expr(base, &mut None);
        self.make_app(id, base, func, args, store_in)
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
                self.make_app(expr, base_expr, place.into(), args, store_in)
            },
            | ValueDefId::ValueId(id) => {
                if Value::from(id).is_intrinsic(self.db) {
                    return self.lower_intrinsic(expr, Value::from(id).name(self.db), args, store_in);
                }

                let func = self.lower_path(base_expr, def, store_in);
                let ret = self.make_app(expr, base_expr, func, args, store_in);

                if Value::from(id).attrs(self.db).by_key("deref").exists() {
                    let place = self.place_op(ret);
                    place.deref().into()
                } else {
                    ret
                }
            },
            | ValueDefId::CtorId(id) => {
                let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr], ReprPos::Argument);
                let ret = self.store_in(store_in, ret_repr);
                let single_variant = Ctor::from(id).type_ctor(self.db).ctors(self.db).len() == 1;
                let is_boxed = Ctor::from(id).type_ctor(self.db).is_boxed(self.db);
                let deref = if is_boxed {
                    self.builder.init(ret.local);
                    ret.clone().deref().field(1)
                } else {
                    ret.clone()
                };

                let downcast = if single_variant {
                    deref.clone()
                } else {
                    deref.clone().downcast(id)
                };

                let args = args.into_iter().map(|a| self.lower_arg(a)).collect::<Vec<_>>();

                for (i, arg) in args.into_iter().enumerate() {
                    self.builder.assign(downcast.clone().field(i), arg);
                }

                if !single_variant {
                    self.builder.set_discriminant(deref, id);
                }

                ret.into()
            },
            | ValueDefId::FixityId(id) => {
                let data = hir_def::data::fixity_data(self.db, id);
                if let Some(def) = data.def(self.db).and_then(|d| d.left()) {
                    self.lower_path_app(expr, base_expr, def, args, store_in)
                } else {
                    let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr], ReprPos::Argument);
                    (Const::Undefined, ret_repr).into()
                }
            },
            | _ => todo!("{def:?}"),
        }
    }

    fn make_app(
        &mut self,
        _id: ExprId,
        base_expr: ExprId,
        mut func: Operand,
        mut args: Vec<Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let mut ty = self.infer.type_of_expr[base_expr];
        let mut calls = Vec::new();

        while let TyKind::Func(func) = ty.kind(self.db) {
            calls.push((func.params.len(), func.ret));
            ty = func.ret;
        }

        for (arg_count, ret_ty) in calls {
            let args2 = args.drain(..arg_count).map(|a| self.lower_arg(a)).collect::<Vec<_>>();
            let ret_repr = repr_of(self.db, ret_ty, ReprPos::Argument);
            let ret = if args.is_empty() {
                self.store_in(store_in, ret_repr)
            } else {
                self.store_in(&mut None, ret_repr)
            };

            self.builder.call(ret.clone(), func, args2);
            func = ret.into();
        }

        func
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
        ctx.lower_lambda_body(expr, env, params, body);
        let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr], ReprPos::Argument);
        let body = ctx.builder.build(self.db, ctx.id, ret_repr.clone());

        self.lambdas.push((expr, body));
        self.lambdas.append(&mut ctx.lambdas);

        let types = match &self.infer.ty {
            | hir_ty::ty::Generalized::Mono(_) => &[],
            | hir_ty::ty::Generalized::Poly(vars, _) => &**vars,
        };

        let types = types.iter().map(|&v| Ty::new(self.db, TyKind::Var(v))).collect();
        let impls = (0..self.builder.constraints().len()).map(ImplSource::Param).collect();
        let subst = Subst { types, impls };
        let instance = Instance::new(self.db, InstanceId::Body(body), Some(subst).filter(|s| !s.is_empty()));
        let func_repr = match ret_repr.kind(self.db) {
            | ReprKind::Func(_, false) => return (Const::Instance(instance), ret_repr).into(),
            | ReprKind::Func(sig, true) => Repr::new(self.db, ReprKind::Func(sig.clone(), false)),
            | _ => unreachable!(),
        };

        let var = self.store_in(store_in, ret_repr);
        let func = (Const::Instance(instance), func_repr);
        let env_repr = env
            .iter()
            .map(|e| repr_of(self.db, self.infer.type_of_pat[*e], ReprPos::Argument))
            .collect();
        let env_repr = Repr::new(self.db, ReprKind::Struct(env_repr));
        let env_repr = Repr::new(self.db, ReprKind::Box(env_repr));
        let env_var = if env.is_empty() {
            (Const::Zeroed, env_repr).into()
        } else {
            let env_var = self.store_in(&mut None, env_repr);
            self.builder.init(env_var.local);
            let env_deref = env_var.clone().deref().field(1);

            for (i, &pat) in env.iter().enumerate() {
                let val = self.locals[pat].clone();
                self.builder.assign(env_deref.clone().field(i), val);
            }

            Operand::Move(env_var)
        };

        self.builder.assign(var.clone().field(0), func);
        self.builder.assign(var.clone().field(1), env_var);
        var.into()
    }

    fn lower_lambda_body(&mut self, _expr: ExprId, env: &[PatId], params: &[PatId], body: ExprId) {
        for &v in self.infer.ty.type_vars() {
            self.builder.add_type_var(v.into());
        }

        for c in self.infer.constraints.iter() {
            self.builder.add_constraint(c.clone());
        }

        let env_repr = env
            .iter()
            .map(|e| repr_of(self.db, self.infer.type_of_pat[*e], ReprPos::Argument))
            .collect();
        let env_repr = Repr::new(self.db, ReprKind::Struct(env_repr));
        let env_repr = Repr::new(self.db, ReprKind::Box(env_repr));
        let env_param = self.builder.add_local(LocalKind::Arg, env_repr.clone());
        let entry = self.builder.create_block();
        self.builder.switch_block(entry);
        self.builder.add_block_param(entry, env_param);

        for &param in params.iter() {
            let param_repr = repr_of(self.db, self.infer.type_of_pat[param], ReprPos::Argument);
            let local = self.builder.add_local(LocalKind::Arg, param_repr);
            self.builder.add_block_param(entry, local);
            self.bind_pat(param, Place::new(local));
        }

        let env_place = Place::new(env_param).deref().field(1);

        for (i, &pat) in env.iter().enumerate() {
            self.locals.insert(pat, env_place.clone().field(i));
        }

        let res = self.lower_expr(body, &mut None);

        self.builder.ret(res);
    }

    fn lower_if(&mut self, cond: ExprId, then: ExprId) -> Operand {
        let cond = self.lower_expr(cond, &mut None);
        let then_block = self.builder.create_block();
        let exit_block = self.builder.create_block();
        let mut switch = self.builder.switch();

        switch.branch(0, exit_block);
        switch.build(&mut self.builder, cond, then_block);
        self.builder.switch_block(then_block);
        self.lower_expr(then, &mut None);
        self.builder.jump(exit_block);
        self.builder.switch_block(exit_block);

        Operand::Const(Const::Unit, Repr::unit(self.db))
    }

    fn lower_if_else(
        &mut self,
        expr: ExprId,
        cond: ExprId,
        then: ExprId,
        else_: ExprId,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let cond = self.lower_expr(cond, &mut None);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let exit_block = self.builder.create_block();
        let mut switch = self.builder.switch();

        switch.branch(0, else_block);
        switch.build(&mut self.builder, cond, then_block);

        self.builder.switch_block(then_block);
        let then = self.lower_expr(then, &mut store_in.clone());
        self.builder.jump((exit_block, [then]));
        self.builder.switch_block(else_block);
        let else_ = self.lower_expr(else_, &mut store_in.clone());
        self.builder.jump((exit_block, [else_]));

        if let Some(place) = store_in.take() {
            self.builder.switch_block(exit_block);
            place.into()
        } else {
            let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr], ReprPos::Argument);
            let ret = self.builder.add_local(LocalKind::Arg, ret_repr);
            self.builder.add_block_param(exit_block, ret);
            self.builder.switch_block(exit_block);
            Place::new(ret).into()
        }
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
        if let DecisionTree::Switch(pat, _) = tree {
            self.bind_pat(*pat, pred);
        }
        self.lower_decision_tree(&blocks, tree);
        let exit_block = self.builder.create_block();

        for (&(_, branch), block) in branches.iter().zip(blocks) {
            self.builder.switch_block(block);
            let op = self.lower_expr(branch, &mut None);
            self.builder.jump((exit_block, [op]));
        }

        let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr], ReprPos::Argument);
        let ret = self.builder.add_local(LocalKind::Arg, ret_repr);
        self.builder.add_block_param(exit_block, ret);
        self.builder.switch_block(exit_block);
        Place::new(ret).into()
    }

    fn lower_decision_tree(&mut self, blocks: &[Block], tree: &DecisionTree) {
        match tree {
            | DecisionTree::Fail => {
                self.builder.abort();
            },
            | DecisionTree::Leaf(i) => {
                self.builder.jump(blocks[*i]);
            },
            | DecisionTree::Guard(_, _) => todo!(),
            | DecisionTree::Switch(pat, cases) => {
                let mut default_branch = None;
                let mut switch = self.builder.switch();
                let block = self.builder.current_block();
                let pred = self.locals[*pat].clone();
                let mut discr = pred.clone();

                if !cases.is_empty() && let Some(VariantTag::Ctor(id)) = cases[0].tag {
                    let type_ctor = Ctor::from(id).type_ctor(self.db);
                    let mut discr_repr = self.builder.place_repr(self.db, &discr);

                    if type_ctor.is_boxed(self.db) {
                        discr = discr.deref().field(1);
                        discr_repr = match discr_repr.kind(self.db) {
                            ReprKind::Box(repr) => *repr,
                            _ => unreachable!(),
                        };
                    }

                    if type_ctor.ctors(self.db).len() == 1 {
                        for (i, pats) in cases[0].fields.iter().enumerate() {
                            let field = discr.clone().field(i);
                            for &pat in pats {
                                self.bind_pat(pat, field.clone());
                            }
                        }

                        let branch = self.lower_case_branch(blocks, &cases[0].branch);
                        self.builder.switch_block(block);
                        self.builder.jump(branch);
                        return;
                    }

                    let discr_repr = Repr::new(self.db, ReprKind::Discr(discr_repr));
                    let discr_local = self.builder.add_local(LocalKind::Tmp, discr_repr);

                    self.builder.get_discriminant(Place::new(discr_local), discr);
                    discr = Place::new(discr_local);
                }

                for (i, case) in cases.iter().enumerate() {
                    let Some(tag) = &case.tag else {
                        assert!(case.fields.len() <= 1);
                        for pats in case.fields.iter() {
                            for &pat in pats {
                                self.bind_pat(pat, pred.clone());
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
                            let type_ctor = Ctor::from(*id).type_ctor(self.db);
                            let deref = if type_ctor.is_boxed(self.db) {
                                pred.clone().deref().field(1)
                            } else {
                                pred.clone()
                            };

                            let downcast = if type_ctor.ctors(self.db).len() == 1 {
                                deref
                            } else {
                                deref.downcast(*id)
                            };

                            for (i, pats) in case.fields.iter().enumerate() {
                                let field = downcast.clone().field(i);
                                for &pat in pats {
                                    self.bind_pat(pat, field.clone());
                                }
                            }

                            type_ctor.ctors(self.db).iter().position(|c| c.id() == *id).unwrap() as i128
                        },
                    };

                    let branch = self.lower_case_branch(blocks, &case.branch);

                    if i == cases.len() - 1 {
                        default_branch = Some(branch);
                    } else {
                        switch.branch(value, branch);
                    }
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
                self.lower_decision_tree(blocks, tree);
                block
            },
        }
    }

    fn lower_return(&mut self, expr: ExprId) -> Operand {
        let op = self.lower_expr(expr, &mut None);
        self.builder.ret(op);
        Operand::Const(Const::Undefined, Repr::new(self.db, ReprKind::Uninhabited))
    }

    pub(super) fn lower_arg(&mut self, arg: Arg) -> Operand {
        match arg {
            | Arg::ExprId(id) => self.lower_expr(id, &mut None),
            | Arg::Op(op) => op,
        }
    }

    pub(super) fn store_in(&mut self, store_in: &mut Option<Place>, repr: Repr) -> Place {
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
            | Operand::Move(place) | Operand::Copy(place) => place,
            | Operand::Const(_, ref repr) => {
                let res = self.builder.add_local(LocalKind::Tmp, repr.clone());
                self.builder.assign(Place::new(res), op);
                Place::new(res)
            },
        }
    }
}

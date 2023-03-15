use hir_def::expr::{Expr, ExprId, Literal, Stmt};
use hir_def::id::ValueDefId;

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
        mut args: Vec<Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        match def {
            | ValueDefId::PatId(pat) => {
                let place = self.locals[pat].clone();
                let repr = self.builder.place_repr(self.db, &place);

                if let Repr::Func(_, true) = &*repr {
                    let func = place.clone().field(0);
                    let env = place.field(1);
                    args.insert(0, Arg::Op(env.into()));
                    return self.make_app(expr, func.into(), args, store_in);
                }

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
                let func = self.lower_path(base_expr, def, store_in);
                self.make_app(expr, func, args, store_in)
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

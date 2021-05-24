use crate::db::MirDatabase;
use crate::ir::*;
use crate::layout::Layout;
use hir::display::HirDisplay as _;
use hir::id::HasModule as _;
use hir::ty::{Ty, TyKind};
use hir_def::resolver::{HasResolver, Resolver, ValueNs};
use rustc_hash::FxHashMap;
use std::sync::Arc;

impl Body {
    pub(crate) fn body_mir_query(db: &dyn MirDatabase, def: hir::id::DefWithBodyId) -> Arc<Self> {
        let mut ty = match def {
            | hir::id::DefWithBodyId::FuncId(id) => db.value_ty(id.into()),
            | hir::id::DefWithBodyId::StaticId(id) => db.value_ty(id.into()),
            | hir::id::DefWithBodyId::ConstId(id) => db.value_ty(id.into()),
        };

        let mut args = Vec::new();
        let func_id = db.lang_item(def.module(db.upcast()).lib, "fn-type".into()).unwrap();
        let func_id = func_id.as_type_ctor().unwrap();

        while let Some([arg, ret]) = ty.match_ctor(db.upcast(), func_id) {
            args.push(arg);
            ty = ret;
        }

        let mut builder = Builder::new(db, def, ty);

        builder.lower();

        Arc::new(builder.body)
    }
}

struct Builder<'a> {
    db: &'a dyn MirDatabase,
    def: hir::id::DefWithBodyId,
    hir: Arc<hir::Body>,
    infer: Arc<hir::InferenceResult>,
    body: Body,
    ret: LocalId,
    block: Option<BlockId>,
    binders: FxHashMap<hir::PatId, Place>,
}

impl<'a> Builder<'a> {
    fn new(db: &'a dyn MirDatabase, def: hir::id::DefWithBodyId, ret_ty: Ty) -> Self {
        let mut body = Body::default();
        let ret = body.locals.alloc(Local {
            layout: db.layout_of(ret_ty),
            kind: LocalKind::Ret,
        });

        Self {
            db,
            def,
            body,
            ret,
            hir: db.body(def),
            infer: db.infer(def),
            block: None,
            binders: FxHashMap::default(),
        }
    }

    fn lower(&mut self) {
        let entry = self.create_block();

        self.block = Some(entry);

        for param in self.hir.params().to_vec() {
            let ty = self.infer.type_of_pat[param];
            let arg = self.create_arg(ty);
            let place = Place::new(arg);

            self.lower_pat(param, place);
        }

        let res = self.lower_expr(self.hir.body_expr());
        let ret = Place::new(self.ret);

        self.assign(ret, RValue::Use(res));
        self.ret();
    }

    fn lower_pat(&mut self, id: hir::PatId, place: Place) {
        let body = Arc::clone(&self.hir);
        let ty = self.infer.type_of_pat[id];

        match body[id] {
            | hir::Pat::Missing => {},
            | hir::Pat::Wildcard => {},
            | hir::Pat::Typed { pat, .. } => self.lower_pat(pat, place),
            | hir::Pat::Bind { subpat, .. } => {
                self.binders.insert(id, place.clone());

                if let Some(subpat) = subpat {
                    self.lower_pat(subpat, place);
                }
            },
            | hir::Pat::Tuple { ref pats } => {
                for (i, &pat) in pats.iter().enumerate() {
                    self.lower_pat(pat, place.clone().field(i));
                }
            },
            | _ => unimplemented!(),
        }
    }

    fn lower_expr(&mut self, id: hir::ExprId) -> Operand {
        let body = Arc::clone(&self.hir);
        let ty = self.infer.type_of_expr[id];

        match body[id] {
            | hir::Expr::Missing => Operand::Const(Const::Undefined(self.db.layout_of(ty))),
            | hir::Expr::Typed { expr, .. } => self.lower_expr(expr),
            | hir::Expr::Path { ref path } => self.lower_path(id, path, ty),
            | hir::Expr::Lit { ref lit } => match *lit {
                | hir::Literal::Int(i) => Operand::Const(Const::Scalar(i as u128, self.db.layout_of(ty))),
                | hir::Literal::Float(i) => Operand::Const(Const::Scalar(i as u128, self.db.layout_of(ty))),
                | hir::Literal::Char(i) => Operand::Const(Const::Scalar(i as u128, self.db.layout_of(ty))),
                | hir::Literal::String(ref s) => Operand::Const(Const::String(s.clone())),
            },
            | hir::Expr::Infix { ref op, lhs, rhs } => {
                let resolver = Resolver::for_expr(self.db.upcast(), self.def, id);

                if let Some(ValueNs::Fixity(id)) = resolver.resolve_value_fully(self.db.upcast(), op) {
                    let fixity = self.db.fixity_data(id);
                    let resolver = id.resolver(self.db.upcast());

                    match resolver.resolve_value_fully(self.db.upcast(), &fixity.func) {
                        | Some(ValueNs::Func(id)) => self.lower_func_app(id, vec![lhs, rhs], ty),
                        | Some(ValueNs::Ctor(id)) => self.lower_ctor_app(id, vec![lhs, rhs], ty),
                        | _ => Operand::Const(Const::Undefined(self.db.layout_of(ty))),
                    }
                } else {
                    Operand::Const(Const::Undefined(self.db.layout_of(ty)))
                }
            },
            | hir::Expr::App { .. } => self.lower_app(id, ty),
            | hir::Expr::Do { ref stmts } => {
                let last = stmts.len() - 1;

                for (i, &stmt) in stmts.iter().enumerate() {
                    match stmt {
                        | hir::Stmt::Expr { expr } => {
                            let op = self.lower_expr(expr);

                            if i == last {
                                return op;
                            }
                        },
                        | hir::Stmt::Bind { pat, val } | hir::Stmt::Let { pat, val } => {
                            let place = match self.lower_expr(val) {
                                | Operand::Place(p) => p,
                                | Operand::Const(c) => {
                                    let ty = self.infer.type_of_expr[val];
                                    let p = self.create_tmp(ty);
                                    let p = Place::new(p);

                                    self.assign(p.clone(), RValue::Use(Operand::Const(c)));
                                    p
                                },
                            };

                            self.lower_pat(pat, place);
                        },
                    }
                }

                Operand::Const(Const::Tuple(Vec::new()))
            },
            | ref e => unimplemented!("{:?}", e),
        }
    }

    fn lower_path(&mut self, expr: hir::ExprId, path: &hir::Path, mut ty: Ty) -> Operand {
        let resolver = Resolver::for_expr(self.db.upcast(), self.def, expr);

        match resolver.resolve_value_fully(self.db.upcast(), path) {
            | Some(ValueNs::Func(id)) => {
                while let TyKind::ForAll(_, t) = ty.lookup(self.db.upcast()) {
                    ty = t;
                }

                while let TyKind::Ctnt(_, t) = ty.lookup(self.db.upcast()) {
                    ty = t;
                }

                if let TyKind::App(..) = ty.lookup(self.db.upcast()) {
                    Operand::Const(Const::FuncAddr(id.into()))
                } else {
                    let ret = self.create_tmp(ty);
                    let ret = Place::new(ret);
                    let func = Operand::Const(Const::FuncAddr(id.into()));

                    self.call(ret.clone(), func, Vec::new());

                    Operand::Place(ret)
                }
            },
            | Some(ValueNs::Local(pat)) => Operand::Place(self.binders[&pat].clone()),
            | Some(ValueNs::Const(_)) => Operand::Const(Const::Undefined(self.db.layout_of(ty))),
            | r => unimplemented!("{:?}", r),
        }
    }

    fn lower_app(&mut self, mut base: hir::ExprId, ret_ty: Ty) -> Operand {
        let body = Arc::clone(&self.hir);
        let mut args = vec![];

        while let hir::Expr::App { base: base2, arg } = body[base] {
            args.insert(0, arg);
            base = base2;
        }

        if let hir::Expr::Path { path } = &body[base] {
            let resolver = Resolver::for_expr(self.db.upcast(), self.def, base);

            match resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some(ValueNs::Func(id)) => return self.lower_func_app(id, args, ret_ty),
                | Some(ValueNs::Ctor(id)) => return self.lower_ctor_app(id, args, ret_ty),
                | _ => {},
            }
        }

        let ret = self.create_tmp(ret_ty);
        let ret = Place::new(ret);
        let func = self.lower_expr(base);
        let args = args.into_iter().map(|a| self.lower_expr(a)).collect();

        self.call(ret.clone(), func, args);

        Operand::Place(ret)
    }

    fn lower_func_app(&mut self, func: hir::id::FuncId, args: Vec<hir::ExprId>, ret_ty: Ty) -> Operand {
        let ret = self.create_tmp(ret_ty);
        let ret = Place::new(ret);
        let args = args.into_iter().map(|a| self.lower_expr(a)).collect();

        if self.db.attrs(func.into()).by_key("intrinsic").exists() {
            let name = self.db.func_data(func).name.to_string();

            self.assign(ret.clone(), RValue::Intrinsic(name, args));

            return Operand::Place(ret);
        }

        let func = Operand::Const(Const::FuncAddr(func.into()));

        self.call(ret.clone(), func, args);

        Operand::Place(ret)
    }

    fn lower_ctor_app(&mut self, id: hir::id::CtorId, args: Vec<hir::ExprId>, ret_ty: Ty) -> Operand {
        let data = self.db.type_ctor_data(id.parent);
        let ret = self.create_tmp(ret_ty);
        let ret = Place::new(ret);

        if data.ctors.len() == 1 {
            for (i, arg) in args.into_iter().enumerate() {
                let arg = self.lower_expr(arg);

                self.assign(ret.clone().field(i), RValue::Use(arg));
            }
        } else {
            let idx: u32 = id.local_id.into_raw().into();
            let mut p = ret.clone().downcast(idx as usize);

            for (i, arg) in args.into_iter().enumerate() {
                let arg = self.lower_expr(arg);

                self.assign(p.clone().field(i), RValue::Use(arg));
            }

            self.set_discr(p, idx as u128);
        }

        Operand::Place(ret)
    }

    fn block(&mut self) -> &mut Block {
        &mut self.body.blocks[self.block.unwrap()]
    }

    fn ret(&mut self) {
        self.block().term = Term::Return;
    }

    fn jmp(&mut self, to: BlockId) {
        self.block().term = Term::Jump(to);
    }

    fn assign(&mut self, p: Place, val: RValue) {
        self.block().stmts.push(Stmt::Assign(p, val));
    }

    fn call(&mut self, p: Place, val: Operand, args: Vec<Operand>) {
        self.block().stmts.push(Stmt::Call(p, val, args));
    }

    fn set_discr(&mut self, p: Place, discr: u128) {
        self.block().stmts.push(Stmt::SetDiscr(p, discr));
    }

    fn create_block(&mut self) -> BlockId {
        self.body.blocks.alloc(Block {
            stmts: Vec::new(),
            term: Term::Abort,
        })
    }

    fn create_var(&mut self, ty: Ty) -> LocalId {
        let layout = self.db.layout_of(ty);

        self.body.locals.alloc(Local {
            layout,
            kind: LocalKind::Var,
        })
    }

    fn create_tmp(&mut self, ty: Ty) -> LocalId {
        let layout = self.db.layout_of(ty);

        self.body.locals.alloc(Local {
            layout,
            kind: LocalKind::Tmp,
        })
    }

    fn create_arg(&mut self, ty: Ty) -> LocalId {
        let layout = self.db.layout_of(ty);

        self.body.locals.alloc(Local {
            layout,
            kind: LocalKind::Arg,
        })
    }
}

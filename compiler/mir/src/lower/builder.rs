use crate::db::MirDatabase;
use crate::ir::*;
use crate::layout::{Abi, Layout};
use hir::ty::{Ty, TyKind};
use std::sync::Arc;

#[derive(Default)]
pub struct Builder {
    body: Body,
    block: Option<BlockId>,
    unit_tmp: Option<LocalId>,
}

impl Builder {
    pub fn finish(self) -> Body {
        self.body
    }

    pub fn create_block(&mut self) -> BlockId {
        self.body.blocks.alloc(Block {
            stmts: Vec::new(),
            term: Term::Abort,
        })
    }

    pub fn create_ret(&mut self, db: &dyn MirDatabase, ty: Ty) -> LocalId {
        if let TyKind::TypeVar(_) = ty.lookup(db.upcast()) {
            self.body.locals.alloc(Local {
                layout: Arc::new(Layout::default()),
                kind: LocalKind::Ret,
                is_ssa: false,
            });

            let id = self.body.locals.alloc(Local {
                layout: db.layout_of(ty),
                kind: LocalKind::Arg,
                is_ssa: true,
            });

            self.body.ret = Some(id);
            id
        } else {
            let layout = db.layout_of(ty);
            let id = self.body.locals.alloc(Local {
                is_ssa: match &layout.abi {
                    | Abi::Scalar(_) | Abi::ScalarPair(_, _) => true,
                    | _ => false,
                },
                kind: LocalKind::Ret,
                layout,
            });

            self.body.ret = Some(id);
            id
        }
    }

    pub fn create_arg(&mut self, layout: Arc<Layout>) -> LocalId {
        self.body.locals.alloc(Local {
            is_ssa: match &layout.abi {
                | Abi::Scalar(_) | Abi::ScalarPair(_, _) => true,
                | _ => false,
            },
            kind: LocalKind::Arg,
            layout,
        })
    }

    pub fn create_var(&mut self, layout: Arc<Layout>) -> LocalId {
        if *layout == Layout::UNIT {
            let val = self
                .body
                .locals
                .iter()
                .find_map(|(id, l)| if *l.layout == Layout::UNIT { Some(id) } else { None });

            if let Some(id) = val {
                id
            } else {
                self.body.locals.alloc(Local {
                    layout,
                    kind: LocalKind::Var,
                    is_ssa: false,
                })
            }
        } else {
            self.body.locals.alloc(Local {
                is_ssa: match &layout.abi {
                    | Abi::Scalar(_) | Abi::ScalarPair(_, _) => true,
                    | _ => false,
                },
                kind: LocalKind::Var,
                layout,
            })
        }
    }

    pub fn set_block(&mut self, block: BlockId) {
        self.block = Some(block);

        if let None = self.body.entry {
            self.body.entry = Some(block);
        }
    }

    pub fn get_ret(&self) -> LocalId {
        self.body.ret.unwrap()
    }

    pub fn location(&self) -> Location {
        let block = self.block.unwrap();

        Location {
            block,
            stmt: self.body.blocks[block].stmts.len(),
        }
    }

    pub fn placed(&mut self, op: Operand) -> Place {
        match op {
            | Operand::Place(p) => p,
            | Operand::Const(c, lyt) => {
                let var = self.create_var(lyt.clone());
                let ret = Place::new(var);

                self.use_op(ret.clone(), Operand::Const(c, lyt));
                ret
            },
        }
    }

    pub fn memcpy(&mut self, to: Place, from: Operand, size: Place) {
        let ret = self.unit_tmp.unwrap_or_else(|| self.create_var(Arc::new(Layout::UNIT)));
        let ret = Place::new(ret);

        self.intrinsic(ret, "memcpy", vec![Operand::Place(to), from, Operand::Place(size)]);
    }

    pub fn abort(&mut self) {
        self.block().term = Term::Abort;
    }

    pub fn ret(&mut self) {
        self.block().term = Term::Return;
    }

    pub fn jump(&mut self, to: BlockId) {
        self.block().term = Term::Jump(to);
    }

    pub fn switch(&mut self, op: Operand, vals: Vec<u128>, blocks: Vec<BlockId>) {
        assert_eq!(vals.len() + 1, blocks.len());
        self.block().term = Term::Switch(op, vals, blocks);
    }

    pub fn use_op(&mut self, ret: Place, op: Operand) {
        self.stmt(Stmt::Assign(ret, RValue::Use(op)));
    }

    pub fn addr_of(&mut self, ret: Place, place: Place) {
        self.body.locals[ret.local].is_ssa = false;
        self.stmt(Stmt::Assign(ret, RValue::AddrOf(place)));
    }

    pub fn intrinsic(&mut self, ret: Place, name: impl Into<String>, args: Vec<Operand>) {
        self.stmt(Stmt::Assign(ret, RValue::Intrinsic(name.into(), args)));
    }

    pub fn get_discr(&mut self, ret: Place, place: Place) {
        self.stmt(Stmt::Assign(ret, RValue::GetDiscr(place)));
    }

    pub fn set_discr(&mut self, ret: Place, discr: u128) {
        self.stmt(Stmt::SetDiscr(ret, discr));
    }

    pub fn call(&mut self, ret: Place, func: Operand, args: Vec<Operand>) {
        if let Abi::Aggregate { .. } = self.body.locals[ret.local].layout.abi {
            self.body.locals[ret.local].is_ssa = false;
        }

        self.stmt(Stmt::Call(ret, func, args));
    }

    fn stmt(&mut self, stmt: Stmt) {
        self.block().stmts.push(stmt);
    }

    fn block(&mut self) -> &mut Block {
        &mut self.body.blocks[self.block.unwrap()]
    }
}

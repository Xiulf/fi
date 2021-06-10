use crate::db::MirDatabase;
use crate::ir::*;
use crate::ty::{Type, TypeKind};
use hir::ty::{Ty, TyKind};
use std::sync::Arc;

#[derive(Default)]
pub struct Builder<'a> {
    bodies: &'a mut Bodies,
    body: LocalBodyId,
    block: Option<BlockId>,
    unit_tmp: Option<LocalId>,
}

impl Bodies {
    pub fn add(&mut self) -> Builder {
        let id = self.bodies.alloc(Body::default());

        Builder {
            bodies: self,
            body: id,
            block: None,
            unit_tmp: None,
        }
    }
}

impl<'a> Builder<'a> {
    pub fn create_block(&mut self) -> BlockId {
        self.body.blocks.alloc(Block {
            stmts: Vec::new(),
            term: Term::Abort,
        })
    }

    pub fn create_ret(&mut self, db: &dyn MirDatabase, ty: Ty) -> LocalId {
        let id = self.body.locals.alloc(Local {
            kind: LocalKind::Ret,
            ty: db.mir_type(ty),
        });

        self.body.ret = Some(id);
        id
    }

    pub fn create_arg(&mut self, ty: Arc<Type>) -> LocalId {
        self.body.locals.alloc(Local {
            kind: LocalKind::Arg,
            ty,
        })
    }

    pub fn create_var(&mut self, ty: Arc<Type>) -> LocalId {
        if *ty == Type::UNIT {
            let val = self
                .body
                .locals
                .iter()
                .find_map(|(id, l)| if *l.ty == Type::UNIT { Some(id) } else { None });

            if let Some(id) = val {
                id
            } else {
                self.body.locals.alloc(Local {
                    ty,
                    kind: LocalKind::Var,
                })
            }
        } else {
            self.body.locals.alloc(Local {
                kind: LocalKind::Var,
                ty,
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

    pub fn place_type(&self, db: &dyn MirDatabase, place: &Place) -> Arc<Type> {
        let mut ty = self.body.locals[place.local].ty.clone();

        for elem in &place.elems {
            match elem {
                | PlaceElem::Deref => {
                    if let TypeKind::Ptr(elem) = &ty.kind {
                        ty = elem.clone();
                    }
                },
                | PlaceElem::Field(i) => {
                    if let TypeKind::And(fields) = &ty.kind {
                        ty = fields[*i].clone();
                    }
                },
                | PlaceElem::Offset(_) => {},
                | PlaceElem::Index(_) => {
                    if let TypeKind::Array(elem, _) = &ty.kind {
                        ty = elem.clone();
                    }
                },
                | PlaceElem::Downcast(i) => {
                    if let TypeKind::Or(variants, true) = &ty.kind {
                        ty = variants[*i].clone();
                    }
                },
            }
        }

        ty
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
        self.stmt(Stmt::Call(ret, func, args));
    }

    fn stmt(&mut self, stmt: Stmt) {
        self.block().stmts.push(stmt);
    }

    fn block(&mut self) -> &mut Block {
        &mut self.body.blocks[self.block.unwrap()]
    }
}

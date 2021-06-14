use crate::db::MirDatabase;
use crate::instance_record::InstanceRecord;
use crate::ir::*;
use crate::ty::{Type, TypeKind, TypeVarKind};
use hir::ty::{Ty, TyKind};
use std::sync::Arc;

pub struct Builder<'a> {
    bodies: &'a mut Bodies,
    body: LocalBodyId,
    block: Option<BlockId>,
    unit_tmp: Option<LocalId>,
}

impl Bodies {
    pub fn add(&mut self, type_vars: Vec<Option<TypeVarKind>>, records: Vec<Arc<InstanceRecord>>) -> LocalBodyId {
        self.bodies.alloc(Body {
            type_vars,
            records,
            ..Body::default()
        })
    }

    pub fn set_arity(&mut self, id: LocalBodyId, arity: usize) {
        self.arities.insert(arity, id);
    }

    pub fn builder(&mut self, id: LocalBodyId) -> Builder {
        Builder {
            bodies: self,
            body: id,
            block: None,
            unit_tmp: None,
        }
    }
}

impl<'a> std::ops::Deref for Builder<'a> {
    type Target = Bodies;

    fn deref(&self) -> &Self::Target {
        self.bodies
    }
}

impl<'a> std::ops::DerefMut for Builder<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.bodies
    }
}

impl<'a> Builder<'a> {
    pub fn body(&self) -> &Body {
        &self.bodies[self.body]
    }

    pub fn body_mut(&mut self) -> &mut Body {
        &mut self.bodies[self.body]
    }

    pub fn create_block(&mut self) -> BlockId {
        self.body_mut().blocks.alloc(Block {
            stmts: Vec::new(),
            term: Term::Abort,
        })
    }

    pub fn create_ret(&mut self, ty: Arc<Type>) -> LocalId {
        let id = self.body_mut().locals.alloc(Local {
            kind: LocalKind::Ret,
            ty,
        });

        self.body_mut().ret = Some(id);
        id
    }

    pub fn create_arg(&mut self, ty: Arc<Type>) -> LocalId {
        self.body_mut().locals.alloc(Local {
            kind: LocalKind::Arg,
            ty,
        })
    }

    pub fn create_var(&mut self, ty: Arc<Type>) -> LocalId {
        if *ty == Type::UNIT {
            let val = self
                .body()
                .locals
                .iter()
                .find_map(|(id, l)| if *l.ty == Type::UNIT { Some(id) } else { None });

            if let Some(id) = val {
                id
            } else {
                self.body_mut().locals.alloc(Local {
                    ty,
                    kind: LocalKind::Var,
                })
            }
        } else {
            self.body_mut().locals.alloc(Local {
                kind: LocalKind::Var,
                ty,
            })
        }
    }

    pub fn set_block(&mut self, block: BlockId) {
        self.block = Some(block);

        if let None = self.body().entry {
            self.body_mut().entry = Some(block);
        }
    }

    pub fn get_ret(&self) -> LocalId {
        self.body().ret.unwrap()
    }

    pub fn location(&self) -> Location {
        let block = self.block.unwrap();

        Location {
            block,
            stmt: self.body().blocks[block].stmts.len(),
        }
    }

    pub fn placed(&mut self, op: Operand) -> Place {
        match op {
            | Operand::Record(..) => unimplemented!(),
            | Operand::Place(p) => p,
            | Operand::Const(c, lyt) => {
                let var = self.create_var(lyt.clone());
                let ret = Place::new(var);

                self.use_op(ret.clone(), Operand::Const(c, lyt));
                ret
            },
        }
    }

    pub fn place_type(&self, place: &Place) -> Arc<Type> {
        self.body().place_type(place)
    }

    pub fn alloc(&mut self, db: &dyn MirDatabase, ret: Place, ty: Arc<Type>) {
        let uint_ty = Type::ptr_sized_int(db, false);
        let layout = db.layout_of(ty);
        let size = Const::Scalar(layout.size.bytes() as u128);
        let size = Operand::Const(size, uint_ty);

        self.intrinsic(ret, "alloc", vec![size]);
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
        let block = self.block.unwrap();
        &mut self.body_mut().blocks[block]
    }
}

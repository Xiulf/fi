#![feature(drain_filter)]

pub mod constant;
pub mod convert;
pub mod optimize;
mod printing;
pub mod visit;

pub use check::ty::{Ident, Param, Ty, Type};
pub use hir::{AttrKind, Attribute, Id, ItemId};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct Package<'tcx> {
    pub items: BTreeMap<Id, Item<'tcx>>,
}

#[derive(Debug, Clone)]
pub struct Item<'tcx> {
    pub id: Id,
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub kind: ItemKind<'tcx>,
}

#[derive(Debug, Clone)]
pub enum ItemKind<'tcx> {
    Extern(Ty<'tcx>),
    Global(Ty<'tcx>, Const<'tcx>),
    Body(Body<'tcx>),
}

#[derive(Debug, Clone)]
pub struct Body<'tcx> {
    pub locals: BTreeMap<LocalId, Local<'tcx>>,
    pub blocks: BTreeMap<BlockId, Block<'tcx>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local<'tcx> {
    pub id: LocalId,
    pub kind: LocalKind,
    pub ty: Ty<'tcx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LocalKind {
    Ret,
    Arg,
    Var,
    Tmp,
}

#[derive(Debug, Clone)]
pub struct Block<'tcx> {
    pub id: BlockId,
    pub stmts: Vec<Stmt<'tcx>>,
    pub term: Term<'tcx>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'tcx> {
    Nop,
    Assign(Place<'tcx>, RValue<'tcx>),
}

#[derive(Debug, Clone)]
pub enum Term<'tcx> {
    Unset,
    Abort,
    Return,
    Jump(BlockId),
    Switch(Operand<'tcx>, Vec<u128>, Vec<BlockId>),
    Call(Place<'tcx>, Operand<'tcx>, Vec<Operand<'tcx>>, BlockId),
}

#[derive(Debug, Clone)]
pub struct Place<'tcx> {
    pub base: PlaceBase,
    pub elems: Vec<PlaceElem<'tcx>>,
}

#[derive(Debug, Clone)]
pub enum PlaceBase {
    Local(LocalId),
    Global(Id),
}

#[derive(Debug, Clone)]
pub enum PlaceElem<'tcx> {
    Deref,
    Field(usize),
    Index(Operand<'tcx>),
    Slice(Operand<'tcx>, Operand<'tcx>),
}

#[derive(Debug, Clone)]
pub enum Operand<'tcx> {
    Place(Place<'tcx>),
    Const(Const<'tcx>),
}

#[derive(Debug, Clone)]
pub enum Const<'tcx> {
    Undefined,
    Ref(Box<Const<'tcx>>),
    Tuple(Vec<Const<'tcx>>),
    Array(Vec<Const<'tcx>>),
    Scalar(u128, Ty<'tcx>),
    FuncAddr(Id),
    Bytes(Box<[u8]>),
    Type(Ty<'tcx>),
}

#[derive(Debug, Clone)]
pub enum RValue<'tcx> {
    Use(Operand<'tcx>),
    Ref(Place<'tcx>),
    Cast(Ty<'tcx>, Operand<'tcx>),
    BinOp(BinOp, Operand<'tcx>, Operand<'tcx>),
    UnOp(UnOp, Operand<'tcx>),
    Init(Ty<'tcx>, usize, Vec<Operand<'tcx>>),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BitAnd,
    BitOr,
    BitXOr,
    Shl,
    Shr,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

impl<'tcx> Package<'tcx> {
    pub fn new() -> Self {
        Package {
            items: BTreeMap::new(),
        }
    }

    pub fn declare_extern(&mut self, id: Id, attrs: Vec<Attribute>, name: Ident, ty: Ty<'tcx>) {
        self.items.insert(
            id,
            Item {
                id,
                attrs,
                name,
                kind: ItemKind::Extern(ty),
            },
        );
    }

    pub fn declare_global(&mut self, id: Id, attrs: Vec<Attribute>, name: Ident, ty: Ty<'tcx>) {
        self.items.insert(
            id,
            Item {
                id,
                attrs,
                name,
                kind: ItemKind::Global(ty, Const::Undefined),
            },
        );
    }

    pub fn declare_body(
        &mut self,
        id: Id,
        attrs: Vec<Attribute>,
        name: Ident,
        params: &[Param<'tcx>],
        ret: Ty<'tcx>,
    ) {
        let mut locals = BTreeMap::new();

        locals.insert(
            LocalId(0),
            Local {
                id: LocalId(0),
                kind: LocalKind::Ret,
                ty: ret,
            },
        );

        for param in params {
            let id = LocalId(locals.len());

            locals.insert(
                id,
                Local {
                    id,
                    kind: LocalKind::Arg,
                    ty: param.ty,
                },
            );
        }

        self.items.insert(
            id,
            Item {
                id,
                attrs,
                name,
                kind: ItemKind::Body(Body {
                    locals,
                    blocks: BTreeMap::new(),
                }),
            },
        );
    }

    pub fn define_global(
        &mut self,
        tcx: &check::tcx::Tcx<'tcx>,
        id: Id,
        f: impl FnOnce(Builder<'_, 'tcx>),
    ) {
        let ty = match self.items.get(&id).unwrap().kind {
            ItemKind::Global(ty, _) => ty,
            _ => unreachable!(),
        };

        let mut body = Body {
            locals: BTreeMap::new(),
            blocks: BTreeMap::new(),
        };

        body.locals.insert(
            LocalId::RET,
            Local {
                id: LocalId::RET,
                ty,
                kind: LocalKind::Ret,
            },
        );

        let builder = Builder {
            body: &mut body,
            current_block: None,
        };

        f(builder);

        let value = crate::constant::eval(tcx, body, self);

        if let ItemKind::Global(_, val) = &mut self.items.get_mut(&id).unwrap().kind {
            *val = value;
        }
    }

    pub fn define_body<'a>(&'a mut self, id: Id) -> Builder<'a, 'tcx> {
        if let ItemKind::Body(body) = &mut self.items.get_mut(&id).unwrap().kind {
            Builder {
                body,
                current_block: None,
            }
        } else {
            panic!("item is not a body");
        }
    }
}

impl<'tcx> Item<'tcx> {
    pub fn no_mangle(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(&attr.kind, AttrKind::NoMangle))
    }
}

impl<'tcx> Body<'tcx> {
    pub fn params(&self) -> impl Iterator<Item = &Local<'tcx>> {
        self.locals.values().filter(|v| v.kind == LocalKind::Arg)
    }
}

impl LocalId {
    pub const RET: Self = LocalId(0);

    pub const fn as_u32(self) -> u32 {
        self.0 as u32
    }
}

impl<'tcx> Place<'tcx> {
    pub fn local(id: LocalId) -> Self {
        Place {
            base: PlaceBase::Local(id),
            elems: Vec::new(),
        }
    }

    pub fn global(id: Id) -> Self {
        Place {
            base: PlaceBase::Global(id),
            elems: Vec::new(),
        }
    }

    pub fn deref(mut self) -> Self {
        self.elems.push(PlaceElem::Deref);
        self
    }

    pub fn field(mut self, idx: usize) -> Self {
        self.elems.push(PlaceElem::Field(idx));
        self
    }

    pub fn index(mut self, idx: Operand<'tcx>) -> Self {
        self.elems.push(PlaceElem::Index(idx));
        self
    }

    pub fn slice(mut self, low: Operand<'tcx>, high: Operand<'tcx>) -> Self {
        self.elems.push(PlaceElem::Slice(low, high));
        self
    }
}

pub struct Builder<'a, 'tcx> {
    pub body: &'a mut Body<'tcx>,
    current_block: Option<BlockId>,
}

impl<'a, 'tcx> Builder<'a, 'tcx> {
    fn block(&mut self) -> &mut Block<'tcx> {
        self.body
            .blocks
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
    }

    pub fn current_block(&self) -> BlockId {
        self.current_block.unwrap()
    }

    pub fn create_var(&mut self, ty: Ty<'tcx>) -> LocalId {
        let id = LocalId(self.body.locals.len());

        self.body.locals.insert(
            id,
            Local {
                id,
                kind: LocalKind::Var,
                ty,
            },
        );

        id
    }

    pub fn create_tmp(&mut self, ty: Ty<'tcx>) -> LocalId {
        let id = LocalId(self.body.locals.len());

        self.body.locals.insert(
            id,
            Local {
                id,
                kind: LocalKind::Tmp,
                ty,
            },
        );

        id
    }

    pub fn create_block(&mut self) -> BlockId {
        let id = BlockId(self.body.blocks.len());

        self.body.blocks.insert(
            id,
            Block {
                id,
                stmts: Vec::new(),
                term: Term::Unset,
            },
        );

        id
    }

    pub fn placed(&mut self, op: Operand<'tcx>, ty: Ty<'tcx>) -> Place<'tcx> {
        match op {
            Operand::Place(place) => place,
            _ => {
                let tmp = self.create_tmp(ty);
                let place = Place::local(tmp);

                self.use_(place.clone(), op);
                place
            }
        }
    }

    pub fn use_block(&mut self, id: BlockId) {
        self.current_block = Some(id);
    }

    pub fn use_(&mut self, place: Place<'tcx>, op: Operand<'tcx>) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Use(op)));
    }

    pub fn ref_(&mut self, place: Place<'tcx>, to: Place<'tcx>) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Ref(to)));
    }

    pub fn cast(&mut self, place: Place<'tcx>, ty: Ty<'tcx>, op: Operand<'tcx>) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Cast(ty, op)));
    }

    pub fn binop(&mut self, place: Place<'tcx>, op: BinOp, lhs: Operand<'tcx>, rhs: Operand<'tcx>) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::BinOp(op, lhs, rhs)));
    }

    pub fn unop(&mut self, place: Place<'tcx>, op: UnOp, rhs: Operand<'tcx>) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::UnOp(op, rhs)));
    }

    pub fn init(
        &mut self,
        place: Place<'tcx>,
        ty: Ty<'tcx>,
        variant: usize,
        ops: Vec<Operand<'tcx>>,
    ) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Init(ty, variant, ops)));
    }

    pub fn abort(&mut self) {
        if let Term::Unset = self.block().term {
            self.block().term = Term::Abort;
        }
    }

    pub fn return_(&mut self) {
        if let Term::Unset = self.block().term {
            self.block().term = Term::Return;
        }
    }

    pub fn jump(&mut self, target: BlockId) {
        if let Term::Unset = self.block().term {
            self.block().term = Term::Jump(target);
        }
    }

    pub fn switch(&mut self, op: Operand<'tcx>, vals: Vec<u128>, targets: Vec<BlockId>) {
        if let Term::Unset = self.block().term {
            self.block().term = Term::Switch(op, vals, targets);
        }
    }

    pub fn call(
        &mut self,
        place: Place<'tcx>,
        func: Operand<'tcx>,
        args: Vec<Operand<'tcx>>,
        target: BlockId,
    ) {
        if let Term::Unset = self.block().term {
            self.block().term = Term::Call(place, func, args, target);
        }
    }
}

impl<'tcx> Const<'tcx> {
    pub fn to_bytes(&self, stride: usize) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(stride);

        match self {
            Const::Undefined => {}
            Const::Array(vals) => {
                for val in vals {
                    bytes.append(&mut val.to_bytes(stride / vals.len()));
                }
            }
            Const::Scalar(val, ty) => match ty {
                Type::Int(0) => match stride {
                    2 => bytes.extend(&(*val as i16).to_le_bytes()),
                    4 => bytes.extend(&(*val as i32).to_le_bytes()),
                    8 => bytes.extend(&(*val as i64).to_le_bytes()),
                    _ => unreachable!(),
                },
                Type::UInt(0) => match stride {
                    2 => bytes.extend(&(*val as u16).to_le_bytes()),
                    4 => bytes.extend(&(*val as u32).to_le_bytes()),
                    8 => bytes.extend(&(*val as u64).to_le_bytes()),
                    _ => unreachable!(),
                },
                Type::Int(8) => bytes.extend(&(*val as i8).to_le_bytes()),
                Type::Int(16) => bytes.extend(&(*val as i16).to_le_bytes()),
                Type::Int(32) => bytes.extend(&(*val as i32).to_le_bytes()),
                Type::Int(64) => bytes.extend(&(*val as i64).to_le_bytes()),
                Type::Int(128) => bytes.extend(&(*val as i128).to_le_bytes()),
                Type::UInt(8) => bytes.extend(&(*val as u8).to_le_bytes()),
                Type::UInt(16) => bytes.extend(&(*val as u16).to_le_bytes()),
                Type::UInt(32) => bytes.extend(&(*val as u32).to_le_bytes()),
                Type::UInt(64) => bytes.extend(&(*val as u64).to_le_bytes()),
                Type::UInt(128) => bytes.extend(&(*val as u128).to_le_bytes()),
                Type::Float(32) => bytes.extend(&(*val as u32).to_be_bytes()),
                Type::Float(64) => bytes.extend(&(*val as u32).to_be_bytes()),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }

        bytes.resize(stride, 0);
        bytes
    }
}

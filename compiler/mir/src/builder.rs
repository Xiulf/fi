use arena::Arena;
use hir::TypeVar;
use hir_def::id::CtorId;
use hir_ty::ty::Constraint;

use crate::ir::{
    BasicBlocks, BinOp, Block, BlockData, Body, CastKind, Const, JumpTarget, Local, LocalData, LocalKind, MirValueId,
    NullOp, Operand, Place, Projection, RValue, Statement, Terminator,
};
use crate::repr::{Repr, ReprKind};
use crate::Db;

mod copying;

#[derive(Default)]
pub struct Builder {
    type_vars: Vec<TypeVar>,
    constraints: Vec<Constraint>,
    locals: Arena<LocalData>,
    blocks: BasicBlocks,
    block: Option<Block>,
}

#[derive(Default)]
pub struct SwitchBuilder {
    values: Vec<i128>,
    targets: Vec<JumpTarget>,
}

impl Builder {
    pub fn build(mut self, db: &dyn Db, id: MirValueId, repr: Repr) -> Body {
        copying::run_copy_analyzer(&mut self, db);
        Body::new(db, id, repr, self.type_vars, self.constraints, self.locals, self.blocks)
    }

    pub fn current_block(&self) -> Block {
        self.block.unwrap()
    }

    pub fn type_vars(&self) -> &[TypeVar] {
        &self.type_vars
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    pub fn set_type_vars(&mut self, type_vars: Vec<TypeVar>) {
        self.type_vars = type_vars;
    }

    pub fn add_type_var(&mut self, type_var: TypeVar) {
        self.type_vars.push(type_var);
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    pub fn create_block(&mut self) -> Block {
        Block(self.blocks.arena.alloc(BlockData {
            params: Vec::new(),
            statements: Vec::new(),
            terminator: Terminator::None,
        }))
    }

    pub fn switch_block(&mut self, block: Block) -> Option<Block> {
        self.block.replace(block)
    }

    pub fn add_block_param(&mut self, block: Block, param: Local) {
        self.blocks.arena[block.0].params.push(param);
    }

    pub fn add_local(&mut self, kind: LocalKind, repr: Repr) -> Local {
        Local(self.locals.alloc(LocalData { kind, repr }))
    }

    fn block_mut(&mut self) -> &mut BlockData {
        &mut self.blocks.arena[self.block.unwrap().0]
    }

    fn stmt(&mut self, stmt: Statement) {
        self.block_mut().statements.push(stmt);
    }

    fn term(&mut self, term: Terminator) {
        let block = self.block_mut();

        if let Terminator::None = block.terminator {
            block.terminator = term;
        }
    }

    pub fn unreachable(&mut self) {
        self.term(Terminator::Unreachable);
    }

    pub fn abort(&mut self) {
        self.term(Terminator::Abort);
    }

    pub fn ret(&mut self, op: impl Into<Operand>) {
        self.term(Terminator::Return(op.into()));
    }

    pub fn jump(&mut self, target: impl Into<JumpTarget>) {
        self.term(Terminator::Jump(target.into()));
    }

    pub fn switch(&mut self) -> SwitchBuilder {
        SwitchBuilder::default()
    }

    pub fn init(&mut self, local: Local) {
        self.stmt(Statement::Init(local));
    }

    pub fn drop(&mut self, place: Place) {
        self.stmt(Statement::Drop(place));
    }

    pub fn set_discriminant(&mut self, place: Place, ctor: CtorId) {
        self.stmt(Statement::SetDiscriminant(place, ctor));
    }

    pub fn intrinsic(&mut self, place: Place, name: String, args: impl Into<Vec<Operand>>) {
        let args = args.into();
        self.stmt(Statement::Intrinsic { place, name, args });
    }

    pub fn call(&mut self, place: Place, func: impl Into<Operand>, args: impl Into<Vec<Operand>>) {
        let func = func.into();
        let args = args.into();
        self.stmt(Statement::Call { place, func, args });
    }

    pub fn assign(&mut self, res: Place, op: impl Into<Operand>) {
        self.stmt(Statement::Assign(res, RValue::Use(op.into())));
    }

    pub fn addrof(&mut self, res: Place, place: Place) {
        self.stmt(Statement::Assign(res, RValue::AddrOf(place)));
    }

    pub fn cast(&mut self, res: Place, kind: CastKind, op: impl Into<Operand>) {
        self.stmt(Statement::Assign(res, RValue::Cast(kind, op.into())));
    }

    pub fn binop(&mut self, res: Place, op: BinOp, lhs: impl Into<Operand>, rhs: impl Into<Operand>) {
        self.stmt(Statement::Assign(res, RValue::BinOp(op, lhs.into(), rhs.into())));
    }

    pub fn nullop(&mut self, res: Place, op: NullOp, repr: Repr) {
        self.stmt(Statement::Assign(res, RValue::NullOp(op, repr)));
    }

    pub fn get_discriminant(&mut self, res: Place, place: Place) {
        self.stmt(Statement::Assign(res, RValue::Discriminant(place)));
    }
}

impl Builder {
    pub fn place_repr(&self, db: &dyn Db, place: &Place) -> Repr {
        let mut repr = self.locals[place.local.0].repr.clone();

        for proj in &place.projection {
            if let ReprKind::ReprOf(ty) = repr.kind(db) {
                repr = crate::repr::repr_of(db, *ty);
            }

            match proj {
                | Projection::Deref => match repr.kind(db) {
                    | ReprKind::Ptr(pointee, _, _) => repr = *pointee,
                    | ReprKind::Box(elem) => repr = Repr::new(db, ReprKind::Struct(Box::new([Repr::usize(db), *elem]))),
                    | _ => unreachable!(),
                },
                | Projection::Field(field) => match repr.kind(db) {
                    | ReprKind::Struct(fields) => repr = fields[*field],
                    | _ => unreachable!(),
                },
                | Projection::Index(_) => match repr.kind(db) {
                    | ReprKind::Array(_, elem) => repr = *elem,
                    | _ => unreachable!(),
                },
                | Projection::Slice(_, _) => match repr.kind(db) {
                    | ReprKind::Array(_, elem) => repr = Repr::new(db, ReprKind::Ptr(*elem, true, true)),
                    | ReprKind::Ptr(_, true, _) => {},
                    | _ => unreachable!(),
                },
                | Projection::Downcast(ctor) => match repr.kind(db) {
                    | ReprKind::Enum(variants) => {
                        repr = variants[u32::from(ctor.local_id(db).into_raw()) as usize];
                    },
                    | _ => unreachable!(),
                },
            }
        }

        repr
    }

    pub fn operand_repr(&self, db: &dyn Db, operand: &Operand) -> Repr {
        match operand {
            | Operand::Copy(place) | Operand::Move(place) => self.place_repr(db, place),
            | Operand::Const(_, repr) => repr.clone(),
        }
    }
}

impl SwitchBuilder {
    pub fn build(mut self, builder: &mut Builder, discr: impl Into<Operand>, default_branch: impl Into<JumpTarget>) {
        self.targets.push(default_branch.into());
        builder.term(Terminator::Switch {
            discr: discr.into(),
            values: self.values,
            targets: self.targets,
        });
    }

    pub fn branch(&mut self, value: i128, target: impl Into<JumpTarget>) {
        self.values.push(value);
        self.targets.push(target.into());
    }
}

impl Place {
    pub fn new(local: Local) -> Self {
        Self {
            local,
            projection: Vec::new(),
        }
    }

    pub fn deref(mut self) -> Self {
        self.projection.push(Projection::Deref);
        self
    }

    pub fn field(mut self, index: usize) -> Self {
        self.projection.push(Projection::Field(index));
        self
    }

    pub fn index(mut self, index: impl Into<Operand>) -> Self {
        self.projection.push(Projection::Index(index.into()));
        self
    }

    pub fn slice(mut self, lo: impl Into<Operand>, hi: impl Into<Operand>) -> Self {
        self.projection.push(Projection::Slice(lo.into(), hi.into()));
        self
    }

    pub fn downcast(mut self, ctor: CtorId) -> Self {
        self.projection.push(Projection::Downcast(ctor));
        self
    }
}

impl From<Place> for Operand {
    fn from(value: Place) -> Self {
        Self::Copy(value)
    }
}

impl From<(Const, Repr)> for Operand {
    fn from(value: (Const, Repr)) -> Self {
        Self::Const(value.0, value.1)
    }
}

impl From<Block> for JumpTarget {
    fn from(block: Block) -> Self {
        Self {
            block,
            args: Vec::new(),
        }
    }
}

impl<T: Into<Vec<Operand>>> From<(Block, T)> for JumpTarget {
    fn from((block, args): (Block, T)) -> Self {
        Self {
            block,
            args: args.into(),
        }
    }
}

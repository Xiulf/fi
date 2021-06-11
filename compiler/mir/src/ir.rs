use crate::layout::Layout;
use crate::ty::{Signature, Type};
use hir::arena::{Arena, Idx};
use hir::display::{self, Write as _};
use hir::id::DefWithBodyId;
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Bodies {
    pub(crate) bodies: Arena<Body>,
    pub(crate) arities: FxHashMap<usize, LocalBodyId>,
}

pub type LocalBodyId = Idx<Body>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyId {
    pub def: DefWithBodyId,
    pub local_id: LocalBodyId,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub locals: Arena<Local>,
    pub blocks: Arena<Block>,
    pub entry: Option<BlockId>,
    pub ret: Option<LocalId>,
}

pub type LocalId = Idx<Local>;
pub type BlockId = Idx<Block>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local {
    pub ty: Arc<Type>,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LocalKind {
    Ret,
    Arg,
    Var,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
    pub block: BlockId,
    pub stmt: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Assign(Place, RValue),
    SetDiscr(Place, u128),
    Call(Place, Operand, Vec<Operand>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Abort,
    Return,
    Jump(BlockId),
    Switch(Operand, Vec<u128>, Vec<BlockId>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RValue {
    Use(Operand),
    AddrOf(Place),
    GetDiscr(Place),
    Intrinsic(String, Vec<Operand>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Place(Place),
    Const(Const, Arc<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Place {
    pub local: LocalId,
    pub elems: Vec<PlaceElem>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PlaceElem {
    Deref,
    Field(usize),
    Index(Operand),
    Offset(Operand),
    Downcast(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    Undefined,
    Scalar(u128),
    Addr(BodyId),
    String(String),
    Tuple(Vec<Const>),
    Ref(Box<Const>),
    Ctor(hir::Ctor, Vec<Const>),
}

impl Bodies {
    pub fn main_id(&self, def: DefWithBodyId) -> BodyId {
        let local_id = self.bodies.iter().next().unwrap().0;

        BodyId { def, local_id }
    }

    pub fn arity(&self, mut arity: usize) -> (&Body, usize) {
        loop {
            match self.arities.get(&arity) {
                | Some(id) => return (&self.bodies[*id], arity),
                | None => arity -= 1,
            }
        }

        unreachable!()
    }

    pub fn signature(&self, local_id: LocalBodyId) -> Arc<Type> {
        let body = &self.bodies[local_id];
        let args = body.args().iter().map(|&l| body.locals[l].ty.clone()).collect();
        let ret = body.locals[body.ret.unwrap()].ty.clone();

        Type::func(args, ret)
    }
}

impl std::ops::Index<LocalBodyId> for Bodies {
    type Output = Body;

    fn index(&self, id: LocalBodyId) -> &Self::Output {
        &self.bodies[id]
    }
}

impl std::ops::IndexMut<LocalBodyId> for Bodies {
    fn index_mut(&mut self, id: LocalBodyId) -> &mut Self::Output {
        &mut self.bodies[id]
    }
}

impl Body {
    pub fn args(&self) -> Vec<LocalId> {
        self.locals
            .iter()
            .filter_map(|(id, l)| if l.kind == LocalKind::Arg { Some(id) } else { None })
            .collect()
    }
}

impl Place {
    pub fn new(local: LocalId) -> Self {
        Self {
            local,
            elems: Vec::new(),
        }
    }

    pub fn deref(mut self) -> Self {
        self.elems.push(PlaceElem::Deref);
        self
    }

    pub fn field(mut self, field: usize) -> Self {
        self.elems.push(PlaceElem::Field(field));
        self
    }

    pub fn index(mut self, idx: Operand) -> Self {
        self.elems.push(PlaceElem::Index(idx));
        self
    }

    pub fn offset(mut self, offset: Operand) -> Self {
        self.elems.push(PlaceElem::Offset(offset));
        self
    }

    pub fn downcast(mut self, variant: usize) -> Self {
        self.elems.push(PlaceElem::Downcast(variant));
        self
    }
}

impl Const {
    pub fn type_info(lyt: &Layout) -> Self {
        let size = Const::Scalar(lyt.size.bytes() as u128);
        let tuple = Const::Tuple(vec![size]);

        Const::Ref(Box::new(tuple))
    }
}

impl display::HirDisplay for Bodies {
    fn hir_fmt(&self, f: &mut display::HirFormatter) -> display::Result {
        for (id, body) in self.bodies.iter() {
            let id: u32 = id.into_raw().into();

            write!(f, "^{} :: ", id)?;
            body.hir_fmt(f)?;
            writeln!(f)?;
        }

        Ok(())
    }
}

impl display::HirDisplay for BodyId {
    fn hir_fmt(&self, f: &mut display::HirFormatter) -> display::Result {
        let path = match self.def {
            | DefWithBodyId::FuncId(d) => hir::Func::from(d).path(f.db),
            | DefWithBodyId::StaticId(d) => hir::Static::from(d).path(f.db),
            | DefWithBodyId::ConstId(d) => hir::Const::from(d).path(f.db),
        };

        let local: u32 = self.local_id.into_raw().into();

        write!(f, "{}^{}", path, local)
    }
}

impl display::HirDisplay for Body {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        writeln!(f, "body {{")?;

        for (id, local) in self.locals.iter() {
            let id: u32 = id.into_raw().into();

            write!(display::indent(f), "_{}: ", id)?;
            local.hir_fmt(f)?;
            writeln!(f)?;
        }

        writeln!(f)?;

        for (id, block) in self.blocks.iter() {
            let id: u32 = id.into_raw().into();
            let db = f.db;

            writeln!(f, "%{}:", id)?;
            write!(display::indent(f), "{}", block.display(db))?;
        }

        write!(f, "}}")
    }
}

impl display::HirDisplay for Local {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        match self.kind {
            | LocalKind::Ret => write!(f, "ret"),
            | LocalKind::Arg => write!(f, "arg"),
            | LocalKind::Var => write!(f, "var"),
        }?;

        write!(f, " {}", self.ty)
    }
}

impl display::HirDisplay for Block {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        for stmt in &self.stmts {
            stmt.hir_fmt(f)?;
            writeln!(f)?;
        }

        self.term.hir_fmt(f)?;
        writeln!(f)
    }
}

impl display::HirDisplay for Stmt {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        match self {
            | Stmt::Assign(p, val) => {
                p.hir_fmt(f)?;
                write!(f, " = ")?;
                val.hir_fmt(f)
            },
            | Stmt::SetDiscr(p, d) => {
                write!(f, "discr ")?;
                p.hir_fmt(f)?;
                write!(f, " = {}", d)
            },
            | Stmt::Call(ret, op, args) => {
                ret.hir_fmt(f)?;
                write!(f, " = ")?;
                op.hir_fmt(f)?;
                write!(f, "(")?;

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    arg.hir_fmt(f)?;
                }

                write!(f, ")")
            },
        }
    }
}

impl display::HirDisplay for Term {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        match self {
            | Term::Abort => write!(f, "abort"),
            | Term::Return => write!(f, "return"),
            | Term::Jump(to) => {
                let to: u32 = to.into_raw().into();

                write!(f, "jump %{}", to)
            },
            | Term::Switch(op, vals, blocks) => {
                write!(f, "switch ")?;
                op.hir_fmt(f)?;
                write!(f, " [")?;

                for (val, block) in vals.iter().zip(blocks) {
                    let block: u32 = block.into_raw().into();

                    write!(f, "{}: %{}, ", val, block)?;
                }

                let block: u32 = blocks.last().unwrap().into_raw().into();

                write!(f, "otherwise %{}]", block)
            },
        }
    }
}

impl display::HirDisplay for RValue {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        match self {
            | RValue::Use(op) => op.hir_fmt(f),
            | RValue::AddrOf(p) => {
                write!(f, "addr ")?;
                p.hir_fmt(f)
            },
            | RValue::GetDiscr(p) => {
                write!(f, "discr ")?;
                p.hir_fmt(f)
            },
            | RValue::Intrinsic(name, args) => {
                write!(f, "@{}(", name)?;

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    arg.hir_fmt(f)?;
                }

                write!(f, ")")
            },
        }
    }
}

impl display::HirDisplay for Operand {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        match self {
            | Operand::Place(p) => p.hir_fmt(f),
            | Operand::Const(c, _) => c.hir_fmt(f),
        }
    }
}

impl display::HirDisplay for Place {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        let local: u32 = self.local.into_raw().into();

        write!(f, "_{}", local)?;

        for elem in &self.elems {
            match elem {
                | PlaceElem::Deref => write!(f, ".*")?,
                | PlaceElem::Field(i) => write!(f, ".{}", i)?,
                | PlaceElem::Index(op) => {
                    write!(f, "[")?;
                    op.hir_fmt(f)?;
                    write!(f, "]")?;
                },
                | PlaceElem::Offset(op) => {
                    write!(f, "{{")?;
                    op.hir_fmt(f)?;
                    write!(f, "}}")?;
                },
                | PlaceElem::Downcast(v) => write!(f, "({})", v)?,
            }
        }

        Ok(())
    }
}

impl display::HirDisplay for Const {
    fn hir_fmt(&self, f: &mut display::HirFormatter<'_>) -> display::Result {
        match self {
            | Const::Undefined => write!(f, "undefined"),
            | Const::Scalar(int) => write!(f, "{}", int),
            | Const::Addr(id) => {
                write!(f, "addr(")?;
                id.hir_fmt(f)?;
                write!(f, ")")
            },
            | Const::String(s) => write!(f, "{:?}", s),
            | Const::Tuple(cs) => {
                write!(f, "(")?;

                for (i, c) in cs.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    c.hir_fmt(f)?;
                }

                write!(f, ")")
            },
            | Const::Ref(to) => {
                write!(f, "ref(")?;
                to.hir_fmt(f)?;
                write!(f, ")")
            },
            | Const::Ctor(ctor, cs) => {
                write!(f, "ctor({}", ctor.path(f.db))?;

                for c in cs.iter() {
                    write!(f, ", ")?;
                    c.hir_fmt(f)?;
                }

                write!(f, ")")
            },
        }
    }
}

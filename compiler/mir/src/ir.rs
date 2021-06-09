use crate::layout::Layout;
use crate::ty::Type;
use hir::arena::{Arena, Idx};
use hir::display::{self, Write as _};
use std::sync::Arc;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
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
    pub is_ssa: bool,
    pub is_by_ref: bool,
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
    FuncAddr(hir::Func),
    StaticAddr(hir::Static),
    String(String),
    Tuple(Vec<Const>),
    Ref(Box<Const>),
    Ctor(hir::Ctor, Vec<Const>),
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
            writeln!(display::indent(f), "{}", block.display(db))?;
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

        if self.is_ssa {
            write!(f, " ssa")?;
        }

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
            | Const::FuncAddr(func) => write!(f, "func({})", func.path(f.db)),
            | Const::StaticAddr(stat) => write!(f, "static({})", stat.path(f.db)),
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

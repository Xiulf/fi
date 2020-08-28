use hir::Symbol;
pub use hir::{Ident, Span};
use std::fmt;

pub type Ty<'tcx> = &'tcx Type<'tcx>;
pub type Layout<'tcx> = crate::layout::TyLayout<'tcx, Ty<'tcx>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'tcx> {
    Error,
    TypeOf(hir::Id),
    Var(TypeVar),
    Never,
    Bool,
    Str,
    TypeId,
    VInt(TypeVar),
    VUInt(TypeVar),
    VFloat(TypeVar),
    Int(u8),
    UInt(u8),
    Float(u8),
    Ref(bool, Ty<'tcx>),
    Array(Ty<'tcx>, usize),
    Slice(Ty<'tcx>),
    Tuple(&'tcx [Ty<'tcx>]),
    Struct(hir::Id, &'tcx [Field<'tcx>]),
    Enum(hir::Id, &'tcx [Variant<'tcx>]),
    Func(&'tcx [Param<'tcx>], Ty<'tcx>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(pub(crate) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Param<'tcx> {
    pub span: Span,
    pub name: Ident,
    pub ty: Ty<'tcx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field<'tcx> {
    pub span: Span,
    pub name: Ident,
    pub ty: Ty<'tcx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<'tcx> {
    pub span: Span,
    pub name: Ident,
    pub fields: &'tcx [Field<'tcx>],
}

impl<'tcx> Type<'tcx> {
    pub fn func(&self) -> Option<(&'tcx [Param<'tcx>], Ty<'tcx>)> {
        match self {
            Type::Func(params, ret) => Some((params, ret)),
            _ => None,
        }
    }

    pub fn fields(&self, tcx: &crate::tcx::Tcx<'tcx>) -> Vec<(Symbol, Ty<'tcx>)> {
        match self {
            Type::Str => vec![
                (Symbol::new("ptr"), tcx.builtin.ref_u8),
                (Symbol::new("len"), tcx.builtin.usize),
            ],
            Type::TypeId => vec![
                (Symbol::new("size"), tcx.builtin.usize),
                (Symbol::new("align"), tcx.builtin.usize),
            ],
            Type::Slice(of) => vec![
                (Symbol::new("ptr"), tcx.intern_ty(Type::Ref(false, of))),
                (Symbol::new("len"), tcx.builtin.usize),
            ],
            Type::Tuple(tys) => tys
                .iter()
                .enumerate()
                .map(|(i, ty)| (Symbol::new(i.to_string()), *ty))
                .collect(),
            Type::Struct(_, fields) => fields.iter().map(|f| (f.name.symbol, f.ty)).collect(),
            _ => Vec::new(),
        }
    }

    pub fn pointee(&self) -> Ty<'tcx> {
        match self {
            Type::Ref(_, to) => to,
            _ => panic!("type is not a reference"),
        }
    }

    pub fn idx(&self, tcx: &crate::tcx::Tcx<'tcx>) -> Ty<'tcx> {
        match self {
            Type::Str => tcx.builtin.u8,
            Type::Array(of, _) => of,
            Type::Slice(of) => of,
            _ => panic!("type can't be indexed"),
        }
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Error => write!(f, "[type error]"),
            Type::TypeOf(id) => write!(f, "{}.type", id),
            Type::Var(var) => var.fmt(f),
            Type::Never => write!(f, "never"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::TypeId => write!(f, "type"),
            Type::VInt(_) => write!(f, "int"),
            Type::VUInt(_) => write!(f, "uint"),
            Type::VFloat(_) => write!(f, "float"),
            Type::Int(0) => write!(f, "isize"),
            Type::UInt(0) => write!(f, "usize"),
            Type::Int(bits) => write!(f, "i{}", bits),
            Type::UInt(bits) => write!(f, "u{}", bits),
            Type::Float(bits) => write!(f, "f{}", bits),
            Type::Ref(true, to) => write!(f, "ref mut {}", to),
            Type::Ref(false, to) => write!(f, "ref {}", to),
            Type::Array(of, len) => write!(f, "[{}; {}]", of, len),
            Type::Slice(of) => write!(f, "[{}]", of),
            Type::Tuple(tys) => write!(
                f,
                "({})",
                tys.iter()
                    .map(|t| format!("{},", t))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Type::Struct(id, _) => write!(f, "struct {}", id),
            Type::Enum(id, _) => write!(f, "enum {}", id),
            Type::Func(params, ret) => write!(
                f,
                "fn ({}) -> {}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret
            ),
        }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl fmt::Display for Param<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl fmt::Display for Field<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl fmt::Display for Variant<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.fields.is_empty() {
            write!(f, "{}", self.name)
        } else {
            write!(
                f,
                "{}({})",
                self.name,
                self.fields
                    .iter()
                    .map(|f| f.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}

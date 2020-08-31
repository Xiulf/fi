use hir::Symbol;
pub use hir::{Ident, Span};
use std::fmt;

pub type Ty<'tcx> = &'tcx Type<'tcx>;
pub type Layout<'tcx> = crate::layout::TyLayout<'tcx, Ty<'tcx>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'tcx> {
    Error,
    TypeOf(hir::Id),
    Param(hir::Id),
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
    Forall(&'tcx [hir::Id], Ty<'tcx>),
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

    pub fn mono(&'tcx self, tcx: &crate::tcx::Tcx<'tcx>, args: Vec<Ty<'tcx>>) -> Ty<'tcx> {
        if let Type::Forall(params, ty) = self {
            let mut args = args.into_iter();
            let args = params
                .iter()
                .map(|id| {
                    if let Some(ty) = args.next() {
                        (*id, ty)
                    } else {
                        (*id, tcx.new_var())
                    }
                })
                .collect();

            let new_ty = ty.replace(&args, tcx);

            tcx.substs.borrow_mut().insert(new_ty, args);
            new_ty
        } else {
            self
        }
    }

    pub fn replace(
        &'tcx self,
        args: &std::collections::HashMap<hir::Id, Ty<'tcx>>,
        tcx: &crate::tcx::Tcx<'tcx>,
    ) -> Ty<'tcx> {
        match self {
            Type::Param(id) if args.contains_key(id) => args[id],
            Type::Ref(mut_, to) => {
                let new_to = to.replace(args, tcx);

                if new_to != *to {
                    tcx.intern_ty(Type::Ref(*mut_, new_to))
                } else {
                    self
                }
            }
            Type::Array(of, len) => {
                let new_of = of.replace(args, tcx);

                if new_of != *of {
                    tcx.intern_ty(Type::Array(new_of, *len))
                } else {
                    self
                }
            }
            Type::Slice(of) => {
                let new_of = of.replace(args, tcx);

                if new_of != *of {
                    tcx.intern_ty(Type::Slice(new_of))
                } else {
                    self
                }
            }
            Type::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.replace(args, tcx));
                let tys = tcx.arena.alloc_slice_fill_iter(tys);

                tcx.intern_ty(Type::Tuple(tys))
            }
            Type::Struct(id, fields) => {
                let fields = fields.iter().map(|f| Field {
                    span: f.span,
                    name: f.name,
                    ty: f.ty.replace(args, tcx),
                });

                let fields = tcx.arena.alloc_slice_fill_iter(fields);

                tcx.intern_ty(Type::Struct(*id, fields))
            }
            Type::Enum(id, variants) => {
                let variants = variants.iter().map(|v| {
                    let fields = v.fields.iter().map(|f| Field {
                        span: f.span,
                        name: f.name,
                        ty: f.ty.replace(args, tcx),
                    });

                    let fields = tcx.arena.alloc_slice_fill_iter(fields);

                    Variant {
                        span: v.span,
                        name: v.name,
                        fields,
                    }
                });

                let variants = tcx.arena.alloc_slice_fill_iter(variants);

                tcx.intern_ty(Type::Enum(*id, variants))
            }
            Type::Func(params, ret) => {
                let params = params.iter().map(|p| Param {
                    span: p.span,
                    name: p.name,
                    ty: p.ty.replace(args, tcx),
                });

                let params = tcx.arena.alloc_slice_fill_iter(params);
                let ret = ret.replace(args, tcx);

                tcx.intern_ty(Type::Func(params, ret))
            }
            Type::Forall(a, ty) => {
                let new_ty = ty.replace(args, tcx);

                if new_ty != *ty {
                    tcx.intern_ty(Type::Forall(*a, new_ty))
                } else {
                    self
                }
            }
            _ => self,
        }
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Error => write!(f, "[type error]"),
            Type::TypeOf(id) => write!(f, "{}.type", id),
            Type::Param(id) => write!(f, "{}", id),
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
            Type::Forall(args, ty) => write!(
                f,
                "for {}. {}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ty
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

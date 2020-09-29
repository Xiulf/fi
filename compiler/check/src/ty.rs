pub use crate::list::List;
use crate::tcx::Tcx;
use hir::Symbol;
pub use hir::{Ident, Span};
use std::collections::HashMap;
use std::fmt;

pub type Ty<'tcx> = &'tcx Type<'tcx>;
pub type Layout<'tcx> = crate::layout::TyLayout<'tcx, Ty<'tcx>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'tcx> {
    Error,
    TypeOf(hir::Id, &'tcx List<Ty<'tcx>>),
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
    Tuple(&'tcx List<Ty<'tcx>>),
    Struct(hir::Id, &'tcx List<Field<'tcx>>),
    Enum(hir::Id, &'tcx List<Variant<'tcx>>),
    Func(Option<hir::Id>, &'tcx List<Param<'tcx>>, Ty<'tcx>),
    Forall(&'tcx List<hir::Id>, Ty<'tcx>),
    Object,
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
    pub fields: &'tcx List<Field<'tcx>>,
}

pub struct TypeMap<'tcx>(pub(crate) HashMap<hir::Id, Ty<'tcx>>);

pub struct TyDisplay<'a, 'tcx> {
    ty: &'a Type<'tcx>,
    tcx: &'a Tcx<'tcx>,
}

impl<'tcx> Type<'tcx> {
    pub fn display<'a>(&'a self, tcx: &'a Tcx<'tcx>) -> TyDisplay<'a, 'tcx> {
        TyDisplay { ty: self, tcx }
    }

    pub fn func(&self) -> Option<(Option<&hir::Id>, &'tcx List<Param<'tcx>>, Ty<'tcx>)> {
        match self {
            Type::Func(id, params, ret) => Some((id.as_ref(), params, ret)),
            _ => None,
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            Type::Param(_) => true,
            Type::Object => true,
            _ => false,
        }
    }

    pub fn fields(&self, tcx: &Tcx<'tcx>) -> Vec<(Symbol, Ty<'tcx>)> {
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
                .map(|(i, ty)| (Symbol::new(i.to_string()), ty))
                .collect(),
            Type::Struct(_, fields) => fields.iter().map(|f| (f.name.symbol, f.ty)).collect(),
            _ => Vec::new(),
        }
    }

    pub fn pointee(&self, tcx: &Tcx<'tcx>) -> Ty<'tcx> {
        match self {
            Type::Ref(_, to) => to,
            Type::TypeId => tcx.builtin.type_layout,
            _ => panic!("type is not a reference"),
        }
    }

    pub fn idx(&self, tcx: &Tcx<'tcx>) -> Ty<'tcx> {
        match self {
            Type::Str => tcx.builtin.u8,
            Type::Array(of, _) => of,
            Type::Slice(of) => of,
            _ => panic!("type can't be indexed"),
        }
    }

    pub fn mono(&'tcx self, tcx: &Tcx<'tcx>, args: Vec<Ty<'tcx>>) -> Ty<'tcx> {
        if let Type::Forall(params, ty) = self {
            let mut args = args.into_iter();
            let args = params
                .iter()
                .map(|id| {
                    if let Some(ty) = args.next() {
                        (id, ty)
                    } else {
                        (id, tcx.new_var())
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

    pub fn replace(&'tcx self, args: &HashMap<hir::Id, Ty<'tcx>>, tcx: &Tcx<'tcx>) -> Ty<'tcx> {
        match self {
            Type::TypeOf(id, tys) => {
                let tys = tys
                    .iter()
                    .map(|ty| ty.replace(args, tcx))
                    .collect::<Vec<_>>();

                let tys = tcx.intern.intern_ty_list(&tys);

                tcx.intern_ty(Type::TypeOf(*id, tys))
            }
            Type::Param(id) if args.contains_key(id) => args[id],
            Type::Ref(mut_, to) => {
                let new_to = to.replace(args, tcx);

                tcx.intern_ty(Type::Ref(*mut_, new_to))
            }
            Type::Array(of, len) => {
                let new_of = of.replace(args, tcx);

                tcx.intern_ty(Type::Array(new_of, *len))
            }
            Type::Slice(of) => {
                let new_of = of.replace(args, tcx);

                tcx.intern_ty(Type::Slice(new_of))
            }
            Type::Tuple(tys) => {
                let tys = tys.iter().map(|t| t.replace(args, tcx)).collect::<Vec<_>>();
                let tys = tcx.intern.intern_ty_list(&tys);

                tcx.intern_ty(Type::Tuple(tys))
            }
            Type::Struct(id, fields) => {
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        span: f.span,
                        name: f.name,
                        ty: f.ty.replace(args, tcx),
                    })
                    .collect::<Vec<_>>();

                let fields = tcx.intern.intern_field_list(&fields);

                tcx.intern_ty(Type::Struct(*id, fields))
            }
            Type::Enum(id, variants) => {
                let variants = variants
                    .iter()
                    .map(|v| {
                        let fields = v
                            .fields
                            .iter()
                            .map(|f| Field {
                                span: f.span,
                                name: f.name,
                                ty: f.ty.replace(args, tcx),
                            })
                            .collect::<Vec<_>>();

                        let fields = tcx.intern.intern_field_list(&fields);

                        Variant {
                            span: v.span,
                            name: v.name,
                            fields,
                        }
                    })
                    .collect::<Vec<_>>();

                let variants = tcx.intern.intern_variant_list(&variants);

                tcx.intern_ty(Type::Enum(*id, variants))
            }
            Type::Func(id, params, ret) => {
                let params = params
                    .iter()
                    .map(|p| Param {
                        span: p.span,
                        name: p.name,
                        ty: p.ty.replace(args, tcx),
                    })
                    .collect::<Vec<_>>();

                let params = tcx.intern.intern_param_list(&params);
                let ret = ret.replace(args, tcx);

                tcx.intern_ty(Type::Func(*id, params, ret))
            }
            Type::Forall(a, ty) => {
                let new_ty = ty.replace(args, tcx);

                tcx.intern_ty(Type::Forall(*a, new_ty))
            }
            _ => self,
        }
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Error => write!(f, "[type error]"),
            Type::TypeOf(id, args) => write!(
                f,
                "{}<{}>",
                id,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
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
            Type::Ref(true, to) => write!(f, "*mut {}", to),
            Type::Ref(false, to) => write!(f, "*{}", to),
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
            // Type::Struct(id, _) => write!(f, "struct {}", id),
            Type::Enum(id, _) => write!(f, "enum {}", id),
            Type::Struct(_, fields) => write!(
                f,
                "struct {{ {} }}",
                fields
                    .iter()
                    .map(|f| f.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Func(Some(id), params, ret) => write!(
                f,
                "fn ({}) -> {} {{{}}}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret,
                id,
            ),
            Type::Func(None, params, ret) => write!(
                f,
                "fn ({}) -> {}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret,
            ),
            Type::Forall(args, ty) => write!(
                f,
                "forall {}. {}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ty
            ),
            Type::Object => write!(f, "[object]"),
        }
    }
}

impl fmt::Display for TyDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ty {
            Type::Error => write!(f, "[type error]"),
            Type::TypeOf(id, args) => write!(
                f,
                "{}<{}>",
                self.tcx.get_full_name(id, true),
                args.iter()
                    .map(|a| a.display(self.tcx).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Param(id) => write!(f, "T{}", id.local().saturating_sub(2) + 1),
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
            Type::Ref(true, to) => write!(f, "*gc {}", to.display(self.tcx)),
            Type::Ref(false, to) => write!(f, "*{}", to.display(self.tcx)),
            Type::Array(of, len) => write!(f, "[{}; {}]", of.display(self.tcx), len),
            Type::Slice(of) => write!(f, "[{}]", of.display(self.tcx)),
            Type::Tuple(tys) => write!(
                f,
                "({})",
                tys.iter()
                    .map(|t| format!("{},", t.display(self.tcx)))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Type::Struct(id, _) => write!(f, "{}", self.tcx.get_full_name(id, true)),
            Type::Enum(id, _) => write!(f, "{}", self.tcx.get_full_name(id, true)),
            Type::Func(Some(id), params, ret) => write!(
                f,
                "fn ({}) -> {} {{{}}}",
                params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, p.ty.display(self.tcx)))
                    .collect::<Vec<_>>()
                    .join(", "),
                ret.display(self.tcx),
                self.tcx.get_full_name(id, true),
            ),
            Type::Func(None, params, ret) => write!(
                f,
                "fn ({}) -> {}",
                params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, p.ty.display(self.tcx)))
                    .collect::<Vec<_>>()
                    .join(", "),
                ret.display(self.tcx),
            ),
            Type::Forall(args, ty) => write!(
                f,
                "for<{}> {}",
                args.iter()
                    .map(|a| format!("T{}", a.local().saturating_sub(2) + 1))
                    .collect::<Vec<_>>()
                    .join(", "),
                ty.display(self.tcx),
            ),
            Type::Object => write!(f, "[object]"),
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

pub(crate) mod ser {
    use super::*;

    #[derive(Debug, serde::Serialize, serde::Deserialize)]
    struct Index(Vec<SType>);

    #[derive(Debug, serde::Serialize, serde::Deserialize)]
    struct Map(HashMap<hir::Id, usize>);

    pub struct Deser<'a, 'tcx>(pub(crate) &'a crate::tcx::TcxIntern<'tcx>);

    #[derive(Debug, serde::Serialize, serde::Deserialize)]
    enum SType {
        TypeOf(hir::Id, Vec<usize>),
        Param(hir::Id),
        Never,
        Bool,
        Str,
        TypeId,
        Int(u8),
        UInt(u8),
        Float(u8),
        Ref(bool, usize),
        Array(usize, usize),
        Slice(usize),
        Tuple(Vec<usize>),
        Struct(hir::Id, Vec<(Span, Ident, usize)>),
        Enum(hir::Id, Vec<(Span, Ident, Vec<(Span, Ident, usize)>)>),
        Func(Option<hir::Id>, Vec<(Span, Ident, usize)>, usize),
        Forall(Vec<hir::Id>, usize),
        Object,
    }

    impl<'tcx> serde::Serialize for TypeMap<'tcx> {
        fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
            let mut index = Index(Vec::new());
            let mut index2 = Vec::new();
            let mut map = Map(HashMap::new());

            fn convert<'tcx>(index: &mut Index, idx2: &mut Vec<Ty<'tcx>>, ty: Ty<'tcx>) -> usize {
                if let Some(idx) = idx2.iter().position(|t| *t == ty) {
                    idx
                } else {
                    let sty = match ty {
                        Type::Error
                        | Type::Var(_)
                        | Type::VInt(_)
                        | Type::VUInt(_)
                        | Type::VFloat(_) => unreachable!(),
                        Type::TypeOf(id, args) => SType::TypeOf(
                            *id,
                            args.iter().map(|ty| convert(index, idx2, ty)).collect(),
                        ),
                        Type::Param(id) => SType::Param(*id),
                        Type::Never => SType::Never,
                        Type::Bool => SType::Bool,
                        Type::Str => SType::Str,
                        Type::TypeId => SType::TypeId,
                        Type::Int(bits) => SType::Int(*bits),
                        Type::UInt(bits) => SType::UInt(*bits),
                        Type::Float(bits) => SType::Float(*bits),
                        Type::Ref(b, ty) => SType::Ref(*b, convert(index, idx2, ty)),
                        Type::Array(ty, len) => SType::Array(convert(index, idx2, ty), *len),
                        Type::Slice(ty) => SType::Slice(convert(index, idx2, ty)),
                        Type::Tuple(tys) => {
                            SType::Tuple(tys.iter().map(|ty| convert(index, idx2, ty)).collect())
                        }
                        Type::Struct(id, fields) => SType::Struct(
                            *id,
                            fields
                                .iter()
                                .map(|f| (f.span, f.name, convert(index, idx2, f.ty)))
                                .collect(),
                        ),
                        Type::Enum(id, variants) => SType::Enum(
                            *id,
                            variants
                                .iter()
                                .map(|v| {
                                    (
                                        v.span,
                                        v.name,
                                        v.fields
                                            .iter()
                                            .map(|f| (f.span, f.name, convert(index, idx2, f.ty)))
                                            .collect(),
                                    )
                                })
                                .collect(),
                        ),
                        Type::Func(id, params, ret) => SType::Func(
                            *id,
                            params
                                .iter()
                                .map(|p| (p.span, p.name, convert(index, idx2, p.ty)))
                                .collect(),
                            convert(index, idx2, ret),
                        ),
                        Type::Forall(params, ret) => {
                            SType::Forall(params.to_vec(), convert(index, idx2, ret))
                        }
                        Type::Object => SType::Object,
                    };

                    index.0.push(sty);
                    idx2.push(ty);
                    index.0.len() - 1
                }
            }

            for (id, ty) in &self.0 {
                map.0.insert(*id, convert(&mut index, &mut index2, ty));
            }

            <(Map, Index)>::serialize(&(map, index), s)
        }
    }

    impl<'de, 'a, 'tcx> serde::de::DeserializeSeed<'de> for Deser<'a, 'tcx> {
        type Value = TypeMap<'tcx>;

        fn deserialize<D: serde::Deserializer<'de>>(self, d: D) -> Result<Self::Value, D::Error> {
            use serde::de::Deserialize;
            let (map, index) = <(Map, Index)>::deserialize(d)?;
            let mut index2 = HashMap::with_capacity(index.0.len());
            let mut tmap = TypeMap(HashMap::with_capacity(map.0.len()));
            let arena = self.0;

            fn convert<'a, 'tcx>(
                arena: &'a crate::tcx::TcxIntern<'tcx>,
                index: &Index,
                index2: &mut HashMap<usize, Ty<'tcx>>,
                idx: usize,
            ) -> Ty<'tcx> {
                if let Some(ty) = index2.get(&idx) {
                    *ty
                } else {
                    let sty = &index.0[idx];
                    let ty = match sty {
                        SType::TypeOf(id, args) => Type::TypeOf(
                            *id,
                            arena.intern_ty_list(
                                &args
                                    .into_iter()
                                    .map(|ty| convert(arena, index, index2, *ty))
                                    .collect::<Vec<_>>(),
                            ),
                        ),
                        SType::Param(id) => Type::Param(*id),
                        SType::Never => Type::Never,
                        SType::Bool => Type::Bool,
                        SType::Str => Type::Str,
                        SType::TypeId => Type::TypeId,
                        SType::Int(bits) => Type::Int(*bits),
                        SType::UInt(bits) => Type::UInt(*bits),
                        SType::Float(bits) => Type::Float(*bits),
                        SType::Ref(b, ty) => Type::Ref(*b, convert(arena, index, index2, *ty)),
                        SType::Array(ty, len) => {
                            Type::Array(convert(arena, index, index2, *ty), *len)
                        }
                        SType::Slice(ty) => Type::Slice(convert(arena, index, index2, *ty)),
                        SType::Tuple(tys) => Type::Tuple(
                            arena.intern_ty_list(
                                &tys.iter()
                                    .map(|ty| convert(arena, index, index2, *ty))
                                    .collect::<Vec<_>>(),
                            ),
                        ),
                        SType::Struct(id, fields) => Type::Struct(
                            *id,
                            arena.intern_field_list(
                                &fields
                                    .iter()
                                    .map(|(span, name, ty)| Field {
                                        span: *span,
                                        name: *name,
                                        ty: convert(arena, index, index2, *ty),
                                    })
                                    .collect::<Vec<_>>(),
                            ),
                        ),
                        SType::Enum(id, variants) => Type::Enum(
                            *id,
                            arena.intern_variant_list(
                                &variants
                                    .iter()
                                    .map(|(span, name, fields)| Variant {
                                        span: *span,
                                        name: *name,
                                        fields: arena.intern_field_list(
                                            &fields
                                                .iter()
                                                .map(|(span, name, ty)| Field {
                                                    span: *span,
                                                    name: *name,
                                                    ty: convert(arena, index, index2, *ty),
                                                })
                                                .collect::<Vec<_>>(),
                                        ),
                                    })
                                    .collect::<Vec<_>>(),
                            ),
                        ),
                        SType::Func(id, params, ret) => Type::Func(
                            *id,
                            arena.intern_param_list(
                                &params
                                    .iter()
                                    .map(|(span, name, ty)| Param {
                                        span: *span,
                                        name: *name,
                                        ty: convert(arena, index, index2, *ty),
                                    })
                                    .collect::<Vec<_>>(),
                            ),
                            convert(arena, index, index2, *ret),
                        ),
                        SType::Forall(params, ret) => Type::Forall(
                            arena.intern_id_list(params.as_ref()),
                            convert(arena, index, index2, *ret),
                        ),
                        SType::Object => Type::Object,
                    };

                    let ty = arena.intern_ty(ty);

                    index2.insert(idx, ty);
                    ty
                }
            }

            for (id, i) in map.0.into_iter() {
                let ty = convert(&arena, &index, &mut index2, i);

                tmap.0.insert(id, ty);
            }

            Ok(tmap)
        }
    }
}

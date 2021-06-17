use crate::db::MirDatabase;
use crate::layout::{Integer, Primitive};
use hir::attrs::{AttrInput, AttrInputGroup};
use hir::display::HirDisplay;
use hir::ty::{Ty, TyKind, TypeVar};
use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub repr: ReprOptions,
    pub kind: TypeKind,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReprOptions {
    pub uninhabited: bool,
    pub scalar: Option<Primitive>,
    pub valid_range_start: Option<u128>,
    pub valid_range_end: Option<u128>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Recurse(Ty),
    Unit,
    Var(TypeVar),
    Ptr(Arc<Type>),
    Array(Arc<Type>, usize),
    And(Arc<[Arc<Type>]>),
    Or(Arc<[Arc<Type>]>, bool),
    Func(Signature),
    Clos(Signature, Option<Arc<Type>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Arc<[Arc<Type>]>,
    pub ret: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeVarKind {
    Type,
    Figure,
    Symbol,
}

impl Type {
    pub const UNIT: Self = Self {
        repr: ReprOptions {
            uninhabited: false,
            scalar: None,
            valid_range_start: None,
            valid_range_end: None,
        },
        kind: TypeKind::Unit,
    };

    pub fn mir_type_query(db: &dyn MirDatabase, mut ty: Ty) -> Arc<Type> {
        let mut args = Vec::new();

        while let TyKind::App(a, b) = ty.lookup(db.upcast()) {
            args.push(b);
            ty = a;
        }

        args.reverse();

        let kind = match ty.lookup(db.upcast()) {
            | TyKind::Error => TypeKind::Unit,
            | TyKind::Unknown(_)
            | TyKind::Skolem(_, _)
            | TyKind::Row(_, _)
            | TyKind::Figure(_)
            | TyKind::Symbol(_)
            | TyKind::App(_, _) => unreachable!("{}", ty.display(db.upcast())),
            | TyKind::ForAll(_, ty) => return db.mir_type(ty),
            | TyKind::Ctnt(_, ty) => return db.mir_type(ty),
            | TyKind::TypeVar(tv) => TypeKind::Var(tv),
            | TyKind::Tuple(ts) if ts.is_empty() => TypeKind::Unit,
            | TyKind::Tuple(ts) => {
                let ts = ts.iter().map(|&t| db.mir_type(t)).collect();

                TypeKind::And(ts)
            },
            | TyKind::Ctor(id) => {
                let attrs = db.attrs(id.into());
                let mut attrs = attrs.by_key("repr").attrs();

                if let Some(attr) = attrs.next().and_then(|a| a.group()) {
                    return Self::from_repr(db, attr, &args);
                } else {
                    let data = db.type_ctor_data(id);

                    if data.ctors.is_empty() {
                        TypeKind::Unit
                    } else if data.ctors.len() == 1 {
                        let (local_id, ctor) = data.ctors.iter().next().unwrap();

                        return Self::variant_type(db, local_id, ctor, id, &args);
                    } else {
                        let variants = data
                            .ctors
                            .iter()
                            .map(|(local_id, ctor)| Self::variant_type(db, local_id, ctor, id, &args))
                            .collect();

                        TypeKind::Or(variants, true)
                    }
                }
            },
        };

        Arc::new(Type {
            repr: ReprOptions::default(),
            kind,
        })
    }

    fn variant_type(
        db: &dyn MirDatabase,
        local_id: hir::id::LocalCtorId,
        ctor: &hir_def::data::CtorData,
        id: hir::id::TypeCtorId,
        args: &[Ty],
    ) -> Arc<Type> {
        let lower = db.ctor_ty(hir::id::CtorId { local_id, parent: id });
        let fields = ctor
            .types
            .iter()
            .map(|&t| {
                let ty = lower.types[t];
                let ty = args.iter().fold(ty, |r, t| r.replace_var(db.upcast(), *t));

                db.mir_type(ty)
            })
            .collect();

        Arc::new(Type {
            repr: ReprOptions::default(),
            kind: TypeKind::And(fields),
        })
    }

    fn from_repr(db: &dyn MirDatabase, group: &AttrInputGroup, args: &[Ty]) -> Arc<Type> {
        let mut repr = ReprOptions::default();
        let mut kind = TypeKind::Unit;

        if group.ident("uninhabited") {
            repr.uninhabited = true;
        }

        if let Some(val) = group.field("scalar").and_then(AttrInput::string) {
            repr.scalar = Some(Self::scalar_from_repr(db, val));
        }

        if let Some(val) = group.field("valid_range_start").and_then(AttrInput::int) {
            repr.valid_range_start = Some(val as u128);
        }

        if let Some(val) = group.field("valid_range_end").and_then(AttrInput::int) {
            repr.valid_range_end = Some(val as u128);
        }

        if let Some(ptr) = group.field("ptr").and_then(AttrInput::group) {
            if let Some(elem) = ptr.field("elem") {
                let elem = if let Some(idx) = elem.int() {
                    db.mir_type(args[idx as usize])
                } else {
                    Self::from_repr(db, elem.group().unwrap(), args)
                };

                kind = TypeKind::Ptr(elem);
            }
        }

        if let Some(array) = group.field("array").and_then(AttrInput::group) {
            if let Some(elem) = array.field("elem") {
                if let Some(len) = array.field("len") {
                    let elem = if let Some(idx) = elem.int() {
                        db.mir_type(args[idx as usize])
                    } else {
                        Self::from_repr(db, elem.group().unwrap(), args)
                    };

                    let len = if let Some(len) = len.int() {
                        len as usize
                    } else {
                        let len = len.string().unwrap();

                        len.parse().unwrap()
                    };

                    kind = TypeKind::Array(elem, len);
                }
            }
        }

        if let Some(func) = group.field("func").and_then(AttrInput::group) {
            if let Some(arg) = func.field("arg") {
                if let Some(ret) = func.field("ret") {
                    let arg = if let Some(idx) = arg.int() {
                        db.mir_type(args[idx as usize])
                    } else {
                        Self::from_repr(db, arg.group().unwrap(), args)
                    };

                    let ret = if let Some(idx) = ret.int() {
                        db.mir_type(args[idx as usize])
                    } else {
                        Self::from_repr(db, ret.group().unwrap(), args)
                    };

                    kind = TypeKind::Clos(
                        Signature {
                            params: Arc::new([arg]),
                            ret,
                        },
                        None,
                    );
                }
            }
        }

        if let Some(record) = group.field("record").and_then(AttrInput::group) {
            if let Some(fields) = record.field("fields").and_then(AttrInput::int) {
                if let TyKind::Row(fields, _tail) = args[fields as usize].lookup(db.upcast()) {
                    let fields = fields.iter().map(|f| db.mir_type(f.ty)).collect();

                    kind = TypeKind::And(fields);
                }
            }
        }

        if let Some(count) = group.field("fields").and_then(AttrInput::int) {
            let mut fields = Vec::with_capacity(count as usize);

            for i in 0..count {
                if let Some(field) = group.field(&format!("f{}", i)).and_then(AttrInput::group) {
                    fields.push(Self::from_repr(db, field, args));
                }
            }

            kind = TypeKind::And(fields.into());
        }

        Arc::new(Type { repr, kind })
    }

    fn scalar_from_repr(db: &dyn MirDatabase, repr: &str) -> Primitive {
        match repr {
            | "u8" => Primitive::Int(Integer::I8, false),
            | "u16" => Primitive::Int(Integer::I16, false),
            | "u32" => Primitive::Int(Integer::I32, false),
            | "u64" => Primitive::Int(Integer::I64, false),
            | "u128" => Primitive::Int(Integer::I128, false),
            | "i8" => Primitive::Int(Integer::I8, true),
            | "i16" => Primitive::Int(Integer::I16, true),
            | "i32" => Primitive::Int(Integer::I32, true),
            | "i64" => Primitive::Int(Integer::I64, true),
            | "i128" => Primitive::Int(Integer::I128, true),
            | "f32" => Primitive::F32,
            | "f64" => Primitive::F64,
            | "ptr" => Primitive::Pointer,
            | "ptr_sized_uint" => match db.target_triple().pointer_width() {
                | Ok(target_lexicon::PointerWidth::U16) => Primitive::Int(Integer::I16, false),
                | Ok(target_lexicon::PointerWidth::U32) => Primitive::Int(Integer::I32, false),
                | Ok(target_lexicon::PointerWidth::U64) => Primitive::Int(Integer::I64, false),
                | Err(_) => Primitive::Int(Integer::I32, false),
            },
            | "ptr_sized_int" => match db.target_triple().pointer_width() {
                | Ok(target_lexicon::PointerWidth::U16) => Primitive::Int(Integer::I16, true),
                | Ok(target_lexicon::PointerWidth::U32) => Primitive::Int(Integer::I32, true),
                | Ok(target_lexicon::PointerWidth::U64) => Primitive::Int(Integer::I64, true),
                | Err(_) => Primitive::Int(Integer::I32, true),
            },
            | _ => panic!("invalid scalar repr"),
        }
    }

    pub fn discriminant(db: &dyn MirDatabase, ty: Arc<Type>) -> Arc<Type> {
        use crate::layout::*;
        let layout = db.layout_of(ty);

        match &layout.variants {
            | Variants::Single { .. } => Arc::new(Type::UNIT),
            | Variants::Multiple { tag, .. } => Arc::new(Type {
                repr: ReprOptions {
                    scalar: Some(tag.value),
                    valid_range_start: Some(*tag.valid_range.start()),
                    valid_range_end: Some(*tag.valid_range.end()),
                    ..Default::default()
                },
                kind: TypeKind::Unit,
            }),
        }
    }

    pub fn ref_(to: Arc<Type>) -> Arc<Type> {
        Arc::new(Type {
            repr: ReprOptions {
                valid_range_start: Some(1),
                ..ReprOptions::default()
            },
            kind: TypeKind::Ptr(to),
        })
    }

    pub fn unit_func(ret: Arc<Type>) -> Arc<Type> {
        Arc::new(Type {
            repr: ReprOptions::default(),
            kind: TypeKind::Func(Signature {
                params: Arc::new([]),
                ret,
            }),
        })
    }

    pub fn func(params: Arc<[Arc<Type>]>, ret: Arc<Type>) -> Arc<Type> {
        Arc::new(Type {
            repr: ReprOptions::default(),
            kind: TypeKind::Func(Signature { params, ret }),
        })
    }

    pub fn closure(arg: Arc<Type>, ret: Arc<Type>, env: Option<Arc<Type>>) -> Arc<Type> {
        Arc::new(Type {
            repr: ReprOptions::default(),
            kind: TypeKind::Clos(
                Signature {
                    params: Arc::new([arg]),
                    ret,
                },
                env,
            ),
        })
    }

    pub fn and(i: impl IntoIterator<Item = Arc<Type>>) -> Arc<Type> {
        Arc::new(Type {
            repr: ReprOptions::default(),
            kind: TypeKind::And(i.into_iter().collect()),
        })
    }

    pub fn ptr(valid_range_start: Option<u128>) -> Arc<Type> {
        Arc::new(Type {
            repr: ReprOptions {
                valid_range_start,
                scalar: Some(Primitive::Pointer),
                ..ReprOptions::default()
            },
            kind: TypeKind::Unit,
        })
    }

    pub fn ptr_sized_int(db: &dyn MirDatabase, sign: bool) -> Arc<Type> {
        Arc::new(Type {
            kind: TypeKind::Unit,
            repr: ReprOptions {
                scalar: Some(match db.target_triple().pointer_width() {
                    | Ok(target_lexicon::PointerWidth::U16) => Primitive::Int(Integer::I16, sign),
                    | Ok(target_lexicon::PointerWidth::U32) => Primitive::Int(Integer::I32, sign),
                    | Ok(target_lexicon::PointerWidth::U64) => Primitive::Int(Integer::I64, sign),
                    | Err(_) => Primitive::Int(Integer::I32, sign),
                }),
                ..ReprOptions::default()
            },
        })
    }

    pub fn type_info(db: &dyn MirDatabase) -> Arc<Type> {
        let uint = Type::ptr_sized_int(db, false);
        let struc = Type::and([uint]);

        Type::ref_(struc)
    }

    pub fn str_slice(db: &dyn MirDatabase) -> Arc<Type> {
        let byte = Arc::new(Type {
            kind: TypeKind::Unit,
            repr: ReprOptions {
                scalar: Some(Primitive::Int(Integer::I8, false)),
                ..ReprOptions::default()
            },
        });

        let ptr = Type::ref_(byte);
        let uint = Type::ptr_sized_int(db, false);

        Type::and([ptr, uint])
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            | TypeKind::Recurse(t) => write!(f, "recurse({:?})", t),
            | TypeKind::Unit => match &self.repr.scalar {
                | Some(s) => match s {
                    | Primitive::Int(Integer::I8, false) => write!(f, "u8"),
                    | Primitive::Int(Integer::I16, false) => write!(f, "u16"),
                    | Primitive::Int(Integer::I32, false) => write!(f, "u32"),
                    | Primitive::Int(Integer::I64, false) => write!(f, "u64"),
                    | Primitive::Int(Integer::I128, false) => write!(f, "u128"),
                    | Primitive::Int(Integer::I8, true) => write!(f, "i8"),
                    | Primitive::Int(Integer::I16, true) => write!(f, "i16"),
                    | Primitive::Int(Integer::I32, true) => write!(f, "i32"),
                    | Primitive::Int(Integer::I64, true) => write!(f, "i64"),
                    | Primitive::Int(Integer::I128, true) => write!(f, "i128"),
                    | Primitive::F32 => write!(f, "f32"),
                    | Primitive::F64 => write!(f, "f64"),
                    | Primitive::Pointer => write!(f, "ptr"),
                },
                | None => write!(f, "()"),
            },
            | TypeKind::Var(var) => var.fmt(f),
            | TypeKind::Ptr(to) => write!(f, "*{}", to),
            | TypeKind::Array(of, len) => write!(f, "[{}]{}", len, of),
            | TypeKind::And(tys) => {
                write!(f, "(")?;

                for (i, ty) in tys.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    ty.fmt(f)?;
                }

                write!(f, ")")
            },
            | TypeKind::Or(tys, tagged) => {
                write!(f, "(")?;

                for (i, ty) in tys.iter().enumerate() {
                    if i != 0 {
                        if *tagged {
                            write!(f, " / ")?;
                        } else {
                            write!(f, " | ")?;
                        }
                    }

                    ty.fmt(f)?;
                }

                write!(f, ")")
            },
            | TypeKind::Func(sig) => sig.fmt(f),
            | TypeKind::Clos(sig, _) => {
                write!(f, "|")?;

                for (i, ty) in sig.params.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    ty.fmt(f)?;
                }

                write!(f, "| -> {}", sig.ret)
            },
        }
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        for (i, ty) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }

            ty.fmt(f)?;
        }

        write!(f, ") -> {}", self.ret)
    }
}

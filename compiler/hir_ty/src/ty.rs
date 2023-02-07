use hir_def::id::TypeCtorId;

#[salsa::interned]
pub struct Ty {
    #[return_ref]
    pub kind: TyKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unknown(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Error,
    Unknown(Unknown),
    Ctor(TypeCtorId),
    App(Ty, Box<[Ty]>),
    Func(FuncType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub params: Box<[Ty]>,
    pub ret: Ty,
    pub env: Ty,
    pub is_varargs: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    Normal(usize),
    HigherOrder(Box<[Kind]>),
}

impl Kind {
    pub const TYPE: Self = Self::Normal(0);
}

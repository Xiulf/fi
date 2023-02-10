use hir_def::display::HirDisplay;
use hir_def::id::TypeCtorId;

#[salsa::interned]
pub struct Ty {
    #[return_ref]
    pub kind: TyKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unknown(pub(crate) u32);

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
    pub variadic: bool,
}

impl HirDisplay for Ty {
    type Db<'a> = dyn crate::Db + 'a;

    fn hir_fmt(&self, f: &mut hir_def::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        match self.kind(f.db) {
            | TyKind::Error => write!(f, "{{error}}"),
            | TyKind::Unknown(u) => write!(f, "?{}", u.0),
            | TyKind::Ctor(ctor) => {
                let it = ctor.it(f.db);
                let item_tree = hir_def::item_tree::query(f.db, it.file);
                write!(f, "{}", item_tree[it.value].name.display(f.db))
            },
            | TyKind::App(base, args) => {
                base.hir_fmt(f)?;
                for arg in args.iter() {
                    f.write_char(' ')?;
                    arg.hir_fmt(f)?;
                }
                Ok(())
            },
            | TyKind::Func(func) => {
                f.write_joined(func.params.iter(), ", ")?;
                if func.variadic {
                    f.write_str(", ..")?;
                }
                f.write_str(" -> ")?;
                func.ret.hir_fmt(f)
            },
        }
    }
}

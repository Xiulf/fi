use crate::db::DefDatabase;
use crate::id::{Intern, Lookup};
use crate::name::{AsName, Name};
use crate::path::{convert_path, Path};
use syntax::ast::{self, NameOwner};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeRefId(salsa::InternId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Placeholder,
    Kinded(Box<[TypeRef; 2]>),
    App(Box<[TypeRef; 2]>),
    Tuple(Vec<TypeRef>),
    Path(Path),
    Ptr(Box<TypeRef>, PtrLen),
    Slice(Box<TypeRef>),
    Array(Box<TypeRef>, usize),
    Func(Box<[TypeRef; 2]>),
    Forall(Name, Box<TypeRef>),
    Constraint(Constraint, Box<TypeRef>),
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PtrLen {
    Single,
    Multiple(Option<Sentinel>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sentinel(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {}

impl Intern for TypeRef {
    type ID = TypeRefId;

    fn intern(self, db: &dyn DefDatabase) -> Self::ID {
        db.intern_type_ref(self)
    }
}

impl Lookup for TypeRefId {
    type Data = TypeRef;

    fn lookup(&self, db: &dyn DefDatabase) -> Self::Data {
        db.lookup_intern_type_ref(*self)
    }
}

impl TypeRef {
    pub(crate) fn from_ast(node: ast::Type) -> Self {
        match node {
            | ast::Type::Kinded(inner) => TypeRef::Kinded(Box::new([
                TypeRef::from_ast_opt(inner.ty()),
                TypeRef::from_ast_opt(inner.kind()),
            ])),
            | ast::Type::App(inner) => TypeRef::App(Box::new([
                TypeRef::from_ast_opt(inner.base()),
                TypeRef::from_ast_opt(inner.arg()),
            ])),
            | ast::Type::Path(inner) => convert_path(inner.path()).map(TypeRef::Path).unwrap_or(TypeRef::Error),
            | ast::Type::Array(inner) => inner
                .len()
                .map(|l| TypeRef::Array(Box::new(TypeRef::from_ast_opt(inner.elem())), l))
                .unwrap_or(TypeRef::Error),
            | ast::Type::Slice(inner) => TypeRef::Slice(Box::new(TypeRef::from_ast_opt(inner.elem()))),
            | ast::Type::Ptr(inner) => TypeRef::Ptr(
                Box::new(TypeRef::from_ast_opt(inner.elem())),
                if inner.is_buf_ptr() {
                    PtrLen::Multiple(inner.sentinel().map(|s| Sentinel(s.value())))
                } else {
                    PtrLen::Single
                },
            ),
            | ast::Type::Fn(inner) => TypeRef::Func(Box::new([
                TypeRef::from_ast_opt(inner.param()),
                TypeRef::from_ast_opt(inner.ret()),
            ])),
            | ast::Type::Rec(_inner) => unimplemented!(),
            | ast::Type::Tuple(inner) => TypeRef::Tuple(inner.types().map(|t| TypeRef::from_ast(t)).collect()),
            | ast::Type::Parens(inner) => TypeRef::from_ast_opt(inner.ty()),
            | ast::Type::For(inner) => {
                if let Some(generics) = inner.generics() {
                    generics.vars().fold(
                        generics
                            .constraints()
                            .fold(TypeRef::from_ast_opt(inner.ty()), |ty, ctnt| {
                                unimplemented!();
                            }),
                        |ty, var| {
                            if let Some(name) = var.name() {
                                TypeRef::Forall(name.as_name(), Box::new(ty))
                            } else {
                                TypeRef::Error
                            }
                        },
                    )
                } else {
                    TypeRef::from_ast_opt(inner.ty())
                }
            },
        }
    }

    pub(crate) fn from_ast_opt(node: Option<ast::Type>) -> Self {
        node.map(Self::from_ast).unwrap_or(TypeRef::Error)
    }
}

impl salsa::InternKey for TypeRefId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

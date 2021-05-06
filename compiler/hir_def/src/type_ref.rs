use crate::data::TypeVarData;
use crate::db::DefDatabase;
use crate::id::{Intern, Lookup, TypeVarId};
use crate::name::{AsName, Name};
use crate::path::{convert_path, Path};
use syntax::ast::{self, NameOwner};
use syntax::AstPtr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeRefId(salsa::InternId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Error,
    Placeholder,
    Kinded(TypeRefId, TypeRefId),
    App(TypeRefId, TypeRefId),
    Tuple(Box<[TypeRefId]>),
    Path(Path),
    Ptr(TypeRefId, PtrLen),
    Slice(TypeRefId),
    Array(TypeRefId, usize),
    Func(TypeRefId, TypeRefId),
    Forall(Box<[TypeVarId]>, TypeRefId),
    Constraint(Constraint, TypeRefId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PtrLen {
    Single,
    Multiple(Option<Sentinel>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sentinel(pub i128);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub class: Path,
    pub types: Box<[TypeRefId]>,
}

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
    pub(crate) fn from_ast(node: ast::Type, db: &dyn DefDatabase) -> TypeRefId {
        match node {
            | ast::Type::Kinded(inner) => TypeRef::Kinded(
                TypeRef::from_ast_opt(inner.ty(), db),
                TypeRef::from_ast_opt(inner.kind(), db),
            )
            .intern(db),
            | ast::Type::App(inner) => TypeRef::App(
                TypeRef::from_ast_opt(inner.base(), db),
                TypeRef::from_ast_opt(inner.arg(), db),
            )
            .intern(db),
            | ast::Type::Path(inner) => convert_path(inner.path())
                .map(TypeRef::Path)
                .unwrap_or(TypeRef::Error)
                .intern(db),
            | ast::Type::Array(inner) => inner
                .len()
                .map(|l| TypeRef::Array(TypeRef::from_ast_opt(inner.elem(), db), l))
                .unwrap_or(TypeRef::Error)
                .intern(db),
            | ast::Type::Slice(inner) => TypeRef::Slice(TypeRef::from_ast_opt(inner.elem(), db)).intern(db),
            | ast::Type::Ptr(inner) => TypeRef::Ptr(
                TypeRef::from_ast_opt(inner.elem(), db),
                if inner.is_buf_ptr() {
                    PtrLen::Multiple(inner.sentinel().map(|s| Sentinel(s.value())))
                } else {
                    PtrLen::Single
                },
            )
            .intern(db),
            | ast::Type::Fn(inner) => TypeRef::Func(
                TypeRef::from_ast_opt(inner.param(), db),
                TypeRef::from_ast_opt(inner.ret(), db),
            )
            .intern(db),
            | ast::Type::Rec(_inner) => unimplemented!(),
            | ast::Type::Tuple(inner) => {
                TypeRef::Tuple(inner.types().map(|t| TypeRef::from_ast(t, db)).collect()).intern(db)
            },
            | ast::Type::Parens(inner) => TypeRef::from_ast_opt(inner.ty(), db),
            | ast::Type::For(inner) => {
                let vars = inner
                    .vars()
                    .filter_map(|var| {
                        let name = var.name()?.as_name();
                        let kind = var.kind().map(|k| Self::from_ast(k, db));
                        let data = TypeVarData {
                            ast_ptr: AstPtr::new(&var),
                            name,
                            kind,
                        };

                        Some(data.intern(db))
                    })
                    .collect();

                let ty = TypeRef::from_ast_opt(inner.ty(), db);

                TypeRef::Forall(vars, ty).intern(db)
            },
            | ast::Type::Ctnt(inner) => {
                let ty = Self::from_ast_opt(inner.ty(), db);

                if let Some(ctnt) = inner.ctnt() {
                    let ctnt = Constraint {
                        class: Path::lower(ctnt.class().unwrap()),
                        types: ctnt.types().map(|n| Self::from_ast(n, db)).collect(),
                    };

                    TypeRef::Constraint(ctnt, ty).intern(db)
                } else {
                    ty
                }
            },
        }
    }

    pub(crate) fn from_ast_opt(node: Option<ast::Type>, db: &dyn DefDatabase) -> TypeRefId {
        node.map(|n| Self::from_ast(n, db)).unwrap_or(TypeRef::Error.intern(db))
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

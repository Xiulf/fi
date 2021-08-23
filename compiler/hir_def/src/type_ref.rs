use crate::arena::{Arena, ArenaMap, Idx};
use crate::name::{AsName, Name};
use crate::path::{convert_path, Path};
use either::Either;
use rustc_hash::FxHashMap;
use syntax::ast::{self, NameOwner};
use syntax::AstPtr;

pub type LocalTypeRefId = Idx<TypeRef>;
pub type LocalTypeVarId = Idx<TypeVar>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Error,
    Placeholder,
    Figure(i128),
    Symbol(String),
    Kinded(LocalTypeRefId, LocalTypeRefId),
    App(LocalTypeRefId, LocalTypeRefId),
    Tuple(Box<[LocalTypeRefId]>),
    Path(Path),
    Ptr(LocalTypeRefId, PtrLen),
    Slice(LocalTypeRefId),
    Array(LocalTypeRefId, usize),
    Record(Box<[Field]>, Option<LocalTypeRefId>),
    Row(Box<[Field]>, Option<LocalTypeRefId>),
    Func(LocalTypeRefId, LocalTypeRefId),
    Forall(Box<[LocalTypeVarId]>, LocalTypeRefId),
    Constraint(Constraint, LocalTypeRefId),
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
    pub types: Box<[LocalTypeRefId]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub name: Name,
    pub kind: Option<LocalTypeRefId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: LocalTypeRefId,
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct TypeMap {
    type_refs: Arena<TypeRef>,
    type_vars: Arena<TypeVar>,
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct TypeSourceMap {
    type_ref_map: FxHashMap<AstPtr<ast::Type>, LocalTypeRefId>,
    type_ref_map_back: ArenaMap<LocalTypeRefId, AstPtr<ast::Type>>,

    type_var_map: FxHashMap<TypeVarPtr, LocalTypeVarId>,
    type_var_map_back: ArenaMap<LocalTypeVarId, TypeVarPtr>,
}

type TypeVarPtr = Either<AstPtr<ast::TypeVar>, AstPtr<ast::Type>>;

#[derive(Default)]
pub(crate) struct TypeMapBuilder {
    map: TypeMap,
    source_map: TypeSourceMap,
}

impl TypeRef {
    fn from_ast(node: ast::Type, map: &mut TypeMapBuilder) -> TypeRef {
        match node {
            | ast::Type::Kinded(inner) => {
                TypeRef::Kinded(map.alloc_type_ref_opt(inner.ty()), map.alloc_type_ref_opt(inner.kind()))
            },
            | ast::Type::Hole(_) => TypeRef::Placeholder,
            | ast::Type::Figure(inner) => {
                if let Some(int) = inner.int() {
                    if let Some(val) = int.value() {
                        return TypeRef::Figure(val);
                    }
                }

                TypeRef::Error
            },
            | ast::Type::Symbol(inner) => {
                if let Some(string) = inner.string() {
                    if let Some(val) = string.value() {
                        return TypeRef::Symbol(val);
                    }
                }

                TypeRef::Error
            },
            | ast::Type::App(inner) => TypeRef::App(
                map.alloc_type_ref_opt(inner.base()),
                map.alloc_type_ref_opt(inner.arg()),
            ),
            | ast::Type::Path(inner) => convert_path(inner.path()).map(TypeRef::Path).unwrap_or(TypeRef::Error),
            | ast::Type::Array(inner) => inner
                .len()
                .map(|l| TypeRef::Array(map.alloc_type_ref_opt(inner.elem()), l))
                .unwrap_or(TypeRef::Error),
            | ast::Type::Slice(inner) => TypeRef::Slice(map.alloc_type_ref_opt(inner.elem())),
            | ast::Type::Ptr(inner) => TypeRef::Ptr(
                map.alloc_type_ref_opt(inner.elem()),
                if inner.is_buf_ptr() {
                    PtrLen::Multiple(inner.sentinel().map(|s| Sentinel(s.value())))
                } else {
                    PtrLen::Single
                },
            ),
            | ast::Type::Fn(inner) => TypeRef::Func(
                map.alloc_type_ref_opt(inner.param()),
                map.alloc_type_ref_opt(inner.ret()),
            ),
            | ast::Type::Rec(inner) => {
                let fields = inner
                    .fields()
                    .filter_map(|f| {
                        Some(Field {
                            name: f.name()?.as_name(),
                            ty: map.alloc_type_ref_opt(f.ty()),
                        })
                    })
                    .collect();

                let tail = inner.tail().map(|t| map.alloc_type_ref(t));

                TypeRef::Record(fields, tail)
            },
            | ast::Type::Row(inner) => {
                let fields = inner
                    .fields()
                    .filter_map(|f| {
                        Some(Field {
                            name: f.name()?.as_name(),
                            ty: map.alloc_type_ref_opt(f.ty()),
                        })
                    })
                    .collect();

                let tail = inner.tail().map(|t| map.alloc_type_ref(t));

                TypeRef::Row(fields, tail)
            },
            | ast::Type::Tuple(inner) => TypeRef::Tuple(inner.types().map(|t| map.alloc_type_ref(t)).collect()),
            | ast::Type::Parens(inner) => Self::from_ast_opt(inner.ty(), map),
            | ast::Type::For(inner) => {
                let vars = inner.vars().filter_map(|var| map.alloc_type_var(var)).collect();
                let ty = map.alloc_type_ref_opt(inner.ty());

                TypeRef::Forall(vars, ty)
            },
            | ast::Type::Ctnt(inner) => {
                if let Some(ctnt) = inner.ctnt() {
                    let ctnt = Constraint {
                        class: Path::lower(ctnt.class().unwrap()),
                        types: ctnt.types().map(|n| map.alloc_type_ref(n)).collect(),
                    };

                    let ty = map.alloc_type_ref_opt(inner.ty());

                    TypeRef::Constraint(ctnt, ty)
                } else {
                    TypeRef::from_ast_opt(inner.ty(), map)
                }
            },
        }
    }

    fn from_ast_opt(node: Option<ast::Type>, map: &mut TypeMapBuilder) -> TypeRef {
        node.map(|n| Self::from_ast(n, map)).unwrap_or(TypeRef::Error)
    }
}

impl TypeMap {
    pub(crate) fn builder() -> TypeMapBuilder {
        TypeMapBuilder::default()
    }

    pub fn iter(&self) -> impl Iterator<Item = (LocalTypeRefId, &TypeRef)> {
        self.type_refs.iter()
    }
}

impl std::ops::Index<LocalTypeRefId> for TypeMap {
    type Output = TypeRef;

    fn index(&self, index: LocalTypeRefId) -> &Self::Output {
        &self.type_refs[index]
    }
}

impl std::ops::Index<LocalTypeVarId> for TypeMap {
    type Output = TypeVar;

    fn index(&self, index: LocalTypeVarId) -> &Self::Output {
        &self.type_vars[index]
    }
}

impl TypeSourceMap {
    pub fn type_ref_syntax(&self, id: LocalTypeRefId) -> Option<AstPtr<ast::Type>> {
        self.type_ref_map_back.get(id).cloned()
    }

    pub fn syntax_type_ref(&self, ptr: AstPtr<ast::Type>) -> Option<LocalTypeRefId> {
        self.type_ref_map.get(&ptr).copied()
    }

    pub fn type_var_syntax(&self, id: LocalTypeVarId) -> Option<TypeVarPtr> {
        self.type_var_map_back.get(id).cloned()
    }

    pub fn syntax_type_var(&self, ptr: TypeVarPtr) -> Option<LocalTypeVarId> {
        self.type_var_map.get(&ptr).copied()
    }
}

impl TypeMapBuilder {
    pub fn alloc_type_ref(&mut self, node: ast::Type) -> LocalTypeRefId {
        let ptr = AstPtr::new(&node);
        let type_ref = TypeRef::from_ast(node.clone(), self);

        self.alloc_type_ref_impl(type_ref, ptr)
    }

    pub fn alloc_type_ref_opt(&mut self, node: Option<ast::Type>) -> LocalTypeRefId {
        if let Some(node) = node {
            self.alloc_type_ref(node)
        } else {
            self.error()
        }
    }

    pub fn alloc_type_var(&mut self, node: ast::TypeVar) -> Option<LocalTypeVarId> {
        let ptr = AstPtr::new(&node);
        let type_var = TypeVar {
            name: node.name()?.as_name(),
            kind: node.kind().map(|t| self.alloc_type_ref(t)),
        };

        Some(self.alloc_type_var_impl(type_var, Either::Left(ptr)))
    }

    pub fn alloc_type_var_from_ty(&mut self, id: LocalTypeRefId, path: &Path) -> Option<LocalTypeVarId> {
        let ptr = self.source_map.type_ref_syntax(id)?;
        let type_var = TypeVar {
            name: path.segments().first()?.clone(),
            kind: None,
        };

        Some(self.alloc_type_var_impl(type_var, Either::Right(ptr)))
    }

    pub fn lower_constraint(&mut self, ctnt: ast::Constraint) -> Option<Constraint> {
        let class = Path::lower(ctnt.class()?);
        let types = ctnt.types().map(|t| self.alloc_type_ref(t)).collect();

        Some(Constraint { class, types })
    }

    fn alloc_type_ref_impl(&mut self, type_ref: TypeRef, ptr: AstPtr<ast::Type>) -> LocalTypeRefId {
        let id = self.map.type_refs.alloc(type_ref);

        self.source_map.type_ref_map.insert(ptr.clone(), id);
        self.source_map.type_ref_map_back.insert(id, ptr);

        id
    }

    fn alloc_type_var_impl(&mut self, type_var: TypeVar, ptr: TypeVarPtr) -> LocalTypeVarId {
        let id = self.map.type_vars.alloc(type_var);

        self.source_map.type_var_map.insert(ptr.clone(), id);
        self.source_map.type_var_map_back.insert(id, ptr);

        id
    }

    pub fn error(&mut self) -> LocalTypeRefId {
        self.map.type_refs.alloc(TypeRef::Error)
    }

    pub fn iter(&self) -> impl Iterator<Item = (LocalTypeRefId, &TypeRef)> {
        self.map.iter()
    }

    pub fn finish(self) -> (TypeMap, TypeSourceMap) {
        (self.map, self.source_map)
    }
}

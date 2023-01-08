use arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;
use syntax::{ast, AstPtr};

use crate::name::{AsName, Name};
use crate::path::{convert_path, Path};

pub type LocalTypeRefId = Idx<TypeRef>;
pub type LocalTypeVarId = Idx<TypeVar>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeVarSource {
    Type(AstPtr<ast::Type>),
    Name(AstPtr<ast::Name>),
    NameRef(AstPtr<ast::NameRef>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Error,
    Placeholder,
    Unit,
    Figure(i128),
    Symbol(String),
    Path(Path),
    App(LocalTypeRefId, LocalTypeRefId),
    Infix(Box<[LocalTypeRefId]>, Box<[Path]>),
    Record(Box<[Field]>, Option<LocalTypeRefId>),
    Row(Box<[Field]>, Option<LocalTypeRefId>),
    Forall(Box<[LocalTypeVarId]>, LocalTypeRefId),
    Where(WhereClause, LocalTypeRefId),
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause {
    pub constraints: Box<[Constraint]>,
    pub type_var_kinds: Box<[TypeVarKind]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub class: Path,
    pub types: Box<[LocalTypeRefId]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVarKind {
    pub type_var: LocalTypeRefId,
    pub kind: LocalTypeRefId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub name: Name,
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

    type_var_map: FxHashMap<TypeVarSource, LocalTypeVarId>,
    type_var_map_back: ArenaMap<LocalTypeVarId, TypeVarSource>,
}

#[derive(Default)]
pub(crate) struct TypeMapBuilder {
    map: TypeMap,
    source_map: TypeSourceMap,
}

impl TypeRef {
    fn from_ast(node: ast::Type, map: &mut TypeMapBuilder) -> Self {
        match node {
            | ast::Type::Hole(_) => Self::Placeholder,
            | ast::Type::Figure(inner) => {
                if let Some(int) = inner.int() {
                    if let Some(val) = int.value() {
                        return Self::Figure(val);
                    }
                }

                Self::Error
            },
            | ast::Type::Symbol(inner) => {
                if let Some(string) = inner.string() {
                    if let Some(val) = string.value() {
                        return Self::Symbol(val);
                    }
                }

                Self::Error
            },
            | ast::Type::Path(inner) => convert_path(inner.path()).map(Self::Path).unwrap_or(Self::Error),
            | ast::Type::App(inner) => Self::App(
                map.alloc_type_ref_opt(inner.base()),
                map.alloc_type_ref_opt(inner.arg()),
            ),
            | ast::Type::Infix(inner) => {
                let tys = inner.types().map(|t| map.alloc_type_ref(t)).collect();
                let ops = inner.ops().map(|op| Path::from(op.as_name())).collect();

                Self::Infix(tys, ops)
            },
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

                Self::Record(fields, tail)
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

                Self::Row(fields, tail)
            },
            | ast::Type::Unit(_) => Self::Unit,
            | ast::Type::Parens(inner) => Self::from_ast_opt(inner.ty(), map),
            | ast::Type::Forall(inner) => {
                if let Some(vars) = inner.vars() {
                    let vars = vars.iter().map(|v| map.alloc_type_var(v)).collect();
                    let inner = map.alloc_type_ref_opt(inner.ty());

                    Self::Forall(vars, inner)
                } else {
                    Self::from_ast_opt(inner.ty(), map)
                }
            },
            | ast::Type::Where(inner) => {
                let where_clause = map.lower_where_clause(inner.where_clause());
                let inner = map.alloc_type_ref_opt(inner.ty());

                Self::Where(where_clause, inner)
            },
        }
    }

    fn from_ast_opt(node: Option<ast::Type>, map: &mut TypeMapBuilder) -> Self {
        node.map(|n| Self::from_ast(n, map)).unwrap_or(Self::Error)
    }
}

impl TypeMap {
    pub(crate) fn builder() -> TypeMapBuilder {
        TypeMapBuilder::default()
    }

    pub fn iter(&self) -> impl Iterator<Item = (LocalTypeRefId, &TypeRef)> {
        self.type_refs.iter()
    }

    pub fn type_vars(&self) -> &Arena<TypeVar> {
        &self.type_vars
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

    pub fn type_var_syntax(&self, id: LocalTypeVarId) -> Option<TypeVarSource> {
        self.type_var_map_back.get(id).cloned()
    }

    pub fn syntax_type_var(&self, ptr: TypeVarSource) -> Option<LocalTypeVarId> {
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

    pub fn alloc_type_var_from_ty(&mut self, name: Name, id: LocalTypeRefId) -> LocalTypeVarId {
        let source = self.source_map.type_ref_map_back[id].clone();
        let var = TypeVar { name };

        self.alloc_type_var_impl(var, TypeVarSource::Type(source))
    }

    pub fn alloc_type_var(&mut self, node: ast::Name) -> LocalTypeVarId {
        let var = TypeVar { name: node.as_name() };
        let ptr = AstPtr::new(&node);

        self.alloc_type_var_impl(var, TypeVarSource::Name(ptr))
    }

    pub fn lower_where_clause(&mut self, where_clause: Option<ast::WhereClause>) -> WhereClause {
        let where_clause = match where_clause {
            | Some(w) => w,
            | None => return WhereClause::default(),
        };

        let constraints = where_clause
            .constraints()
            .filter_map(|c| self.lower_constraint(c))
            .collect();

        let type_var_kinds = where_clause
            .type_var_kinds()
            .filter_map(|c| self.lower_type_var_kind(c))
            .collect();

        WhereClause {
            constraints,
            type_var_kinds,
        }
    }

    pub fn lower_constraint(&mut self, ctnt: ast::Constraint) -> Option<Constraint> {
        let class = Path::lower(ctnt.class()?);
        let types = ctnt.types().map(|t| self.alloc_type_ref(t)).collect();

        Some(Constraint { class, types })
    }

    pub fn lower_type_var_kind(&mut self, tv_kind: ast::TypeVarKind) -> Option<TypeVarKind> {
        let type_var = self.alloc_type_ref_opt(tv_kind.type_var());
        let kind = self.alloc_type_ref_opt(tv_kind.kind());

        Some(TypeVarKind { type_var, kind })
    }

    fn alloc_type_ref_impl(&mut self, type_ref: TypeRef, ptr: AstPtr<ast::Type>) -> LocalTypeRefId {
        let id = self.map.type_refs.alloc(type_ref);

        self.source_map.type_ref_map.insert(ptr.clone(), id);
        self.source_map.type_ref_map_back.insert(id, ptr);

        id
    }

    fn alloc_type_var_impl(&mut self, type_var: TypeVar, ptr: TypeVarSource) -> LocalTypeVarId {
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

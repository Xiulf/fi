use arena::{Arena, ArenaMap, Idx};
use diagnostics::Diagnostics;
use either::Either;
use ra_ap_stdx::hash::NoHashHashMap;
use rustc_hash::FxHashMap;
use syntax::ast::{self, Assoc, AstNode, Prec};
use syntax::ptr::AstPtr;
use triomphe::Arc;
use vfs::File;

use crate::def_map::DefMap;
use crate::diagnostics::UnresolvedPath;
use crate::id::{HasModule, ITypedItemId, ItemId, TraitId, TypeDefId, TypeVarId, TypedItemId};
use crate::item_tree::FixityKind;
use crate::name::{AsName, Name};
use crate::path::Path;
use crate::per_ns::Namespace;
use crate::source::HasSource;
use crate::Db;

pub type TypeRefId = Idx<TypeRef>;
pub type LocalTypeVarId = Idx<TypeVar>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Missing,
    Hole,
    Path { path: Path, def: Option<TypeDefId> },
    App { base: TypeRefId, args: Box<[TypeRefId]> },
    Func { args: Box<[TypeRefId]>, ret: TypeRefId },
    Where { clause: WhereClause, ty: TypeRefId },
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereClause {
    pub constraints: Box<[Constraint]>,
    pub type_var_kinds: Box<[TypeVarKind]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub path: Path,
    pub trait_: Option<TraitId>,
    pub types: Box<[TypeRefId]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVarKind {
    pub type_var: LocalTypeVarId,
    pub kind: TypeRefId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub name: Name,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct TypeMap {
    type_refs: Arena<TypeRef>,
    type_vars: Arena<TypeVar>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeSourceMap {
    file: File,
    typ_to_src: ArenaMap<TypeRefId, TypeRefSrc>,
    src_to_typ: FxHashMap<AstPtr<ast::Type>, TypeRefId>,
    var_to_src: ArenaMap<LocalTypeVarId, AstPtr<ast::Name>>,
    src_to_var: FxHashMap<AstPtr<ast::Name>, LocalTypeVarId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeRefSrc {
    Single(AstPtr<ast::Type>),
    Infix(AstPtr<ast::Type>, AstPtr<ast::Type>),
    Operator(AstPtr<ast::Path>),
    Synthetic,
}

impl TypedItemId {
    #[inline]
    pub fn type_map(self, db: &dyn Db) -> (Arc<TypeMap>, Arc<TypeSourceMap>, Box<[TypeVarId]>) {
        query(db, ITypedItemId::new(db, self))
    }
}

#[salsa::tracked]
pub fn query(db: &dyn Db, item: ITypedItemId) -> (Arc<TypeMap>, Arc<TypeSourceMap>, Box<[TypeVarId]>) {
    let item = item.as_item_id(db);
    let src = match item {
        | TypedItemId::ValueId(id) => id.source(db).map(|v| v.syntax().clone()),
        | TypedItemId::CtorId(id) => id.source(db).map(|v| v.syntax().clone()),
        | TypedItemId::FieldId(id) => id.source(db).map(|v| v.syntax().clone()),
        | TypedItemId::TypeAliasId(id) => id.source(db).map(|v| v.syntax().clone()),
        | TypedItemId::TypeCtorId(id) => id.source(db).map(|v| v.syntax().clone()),
        | TypedItemId::TraitId(id) => id.source(db).map(|v| v.syntax().clone()),
        | TypedItemId::ImplId(id) => id.source(db).map(|v| v.syntax().clone()),
    };

    let mut ctx = Ctx::new(db, item, src.file);

    fn rec(ctx: &mut Ctx, node: &syntax::SyntaxNode) {
        if let Some(ty) = ast::Type::cast(node) {
            ctx.lower_type(ty);
            return;
        }

        for child in node.children() {
            rec(ctx, child);
        }
    }

    rec(&mut ctx, &src.value);

    (Arc::new(ctx.map), Arc::new(ctx.src), ctx.vars.into_values().collect())
}

impl std::ops::Index<TypeRefId> for TypeMap {
    type Output = TypeRef;

    fn index(&self, index: TypeRefId) -> &Self::Output {
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
    pub fn typ_for_src(&self, src: AstPtr<ast::Type>) -> Option<TypeRefId> {
        self.src_to_typ.get(&src).copied()
    }
}

struct Ctx<'a> {
    db: &'a dyn Db,
    owner: TypedItemId,
    def_map: Arc<DefMap>,
    map: TypeMap,
    src: TypeSourceMap,
    vars: NoHashHashMap<Name, TypeVarId>,
}

impl TypeRefSrc {
    pub fn as_typ_ptr(self, lhs: bool) -> AstPtr<ast::Type> {
        match self {
            | Self::Single(ptr) => ptr,
            | Self::Infix(ptr, _) if lhs => ptr,
            | Self::Infix(_, ptr) => ptr,
            | _ => unreachable!(),
        }
    }
}

impl<'a> Ctx<'a> {
    fn new(db: &'a dyn Db, owner: TypedItemId, file: File) -> Self {
        let lib = owner.module(db).lib(db);
        let def_map = crate::def_map::query(db, lib);

        Self {
            db,
            owner,
            def_map,
            map: TypeMap::default(),
            src: TypeSourceMap {
                file,
                typ_to_src: Default::default(),
                src_to_typ: Default::default(),
                var_to_src: Default::default(),
                src_to_var: Default::default(),
            },
            vars: NoHashHashMap::default(),
        }
    }

    fn alloc_type(&mut self, typ: TypeRef, ptr: AstPtr<ast::Type>) -> TypeRefId {
        let id = self.make_type(typ, TypeRefSrc::Single(ptr));

        self.src.src_to_typ.insert(ptr, id);
        id
    }

    fn alloc_type_desugared(&mut self, typ: TypeRef) -> TypeRefId {
        self.make_type(typ, TypeRefSrc::Synthetic)
    }

    fn make_type(&mut self, typ: TypeRef, src: TypeRefSrc) -> TypeRefId {
        let id = self.map.type_refs.alloc(typ);

        self.src.typ_to_src.insert(id, src);
        id
    }

    fn missing_type(&mut self) -> TypeRefId {
        self.alloc_type_desugared(TypeRef::Missing)
    }

    pub fn lower_type(&mut self, ty: ast::Type) -> TypeRefId {
        self.maybe_lower_type(ty).unwrap_or_else(|| self.missing_type())
    }

    pub fn lower_type_opt(&mut self, ty: Option<ast::Type>) -> TypeRefId {
        ty.map(|t| self.lower_type(t)).unwrap_or_else(|| self.missing_type())
    }

    pub fn maybe_lower_type(&mut self, ty: ast::Type) -> Option<TypeRefId> {
        let syntax_ptr = AstPtr::new(&ty);

        Some(match ty {
            | ast::Type::Parens(t) => self.lower_type_opt(t.ty()),
            | ast::Type::Hole(_) => self.alloc_type(TypeRef::Hole, syntax_ptr),
            | ast::Type::Var(t) => {
                let name_ref = t.name()?;
                let name = name_ref.as_name(self.db);
                let path = Path::from(name);
                let var = if let Some(&var) = self.vars.get(&name) {
                    var
                } else {
                    let local_id = self.map.type_vars.alloc(TypeVar { name });
                    let var = TypeVarId::new(self.db, self.owner, Either::Left(local_id));
                    let ptr = AstPtr::new(&name_ref);

                    self.src.var_to_src.insert(local_id, ptr);
                    self.src.src_to_var.insert(ptr, local_id);
                    self.vars.insert(name, var);
                    var
                };

                let def = Some(TypeDefId::TypeVarId(var));

                self.alloc_type(TypeRef::Path { path, def }, syntax_ptr)
            },
            | ast::Type::Path(t) => {
                let tpath = t.path()?;
                let path = Path::from_ast(self.db, tpath.clone());
                let def = self.resolve_path(&path, &tpath);

                self.alloc_type(TypeRef::Path { path, def }, syntax_ptr)
            },
            | ast::Type::App(t) => {
                let base = self.lower_type_opt(t.base());
                let args = t.args().map(|t| self.lower_type(t)).collect();

                self.alloc_type(TypeRef::App { base, args }, syntax_ptr)
            },
            | ast::Type::Infix(t) => {
                let types = t.types().map(|t| self.lower_type(t)).collect();
                let ops = t.ops().map(|p| (Path::from_ast(self.db, p.clone()), p)).collect();

                self.lower_infix(types, ops)
            },
            | ast::Type::Func(t) => {
                let args = t.args().map(|t| self.lower_type(t)).collect();
                let ret = self.lower_type_opt(t.ret());

                self.alloc_type(TypeRef::Func { args, ret }, syntax_ptr)
            },
            | ast::Type::Where(t) => {
                let ty = self.lower_type_opt(t.ty());
                let clause = self.lower_where_clause(t.where_clause()?);

                self.alloc_type(TypeRef::Where { clause, ty }, syntax_ptr)
            },
            | t => todo!("{t:?}"),
        })
    }

    fn lower_where_clause(&mut self, ast: ast::WhereClause) -> WhereClause {
        let mut constraints = Vec::new();
        let mut var_kinds = Vec::new();

        for item in ast.iter() {
            match item {
                | ast::WhereClauseItem::Constraint(ctnt) => {
                    if let Some(constraint) = self.lower_constraint(ctnt) {
                        constraints.push(constraint);
                    }
                },
                | ast::WhereClauseItem::VarKind(varkind) => {
                    if let Some(var_kind) = self.lower_var_kind(varkind) {
                        var_kinds.push(var_kind);
                    }
                },
            }
        }

        WhereClause {
            constraints: constraints.into(),
            type_var_kinds: var_kinds.into(),
        }
    }

    fn lower_constraint(&mut self, ast: ast::WhereClauseConstraint) -> Option<Constraint> {
        let path_src = ast.path()?;
        let path = Path::from_ast(self.db, path_src.clone());
        let types = ast.types().map(|t| self.lower_type(t)).collect();
        let def = self.resolve_path(&path, &path_src);
        let trait_ = def.and_then(|def| match def {
            | TypeDefId::TraitId(id) => Some(id),
            | _ => None, // TODO: report error
        });

        Some(Constraint { path, trait_, types })
    }

    fn lower_var_kind(&mut self, _ast: ast::WhereClauseVarKind) -> Option<TypeVarKind> {
        todo!()
    }

    fn make_infix_app(
        &mut self,
        def: Option<TypeDefId>,
        path: Path,
        path_src: ast::Path,
        lhs: TypeRefId,
        rhs: TypeRefId,
    ) -> TypeRefId {
        let base = self.make_type(
            TypeRef::Path { path, def },
            TypeRefSrc::Operator(AstPtr::new(&path_src)),
        );
        let args = Box::new([lhs, rhs]);
        let lhs_ptr = self.src.typ_to_src[lhs].as_typ_ptr(true);
        let rhs_ptr = self.src.typ_to_src[rhs].as_typ_ptr(false);

        self.make_type(TypeRef::App { base, args }, TypeRefSrc::Infix(lhs_ptr, rhs_ptr))
    }

    fn lower_infix(&mut self, items: Vec<TypeRefId>, ops: Vec<(Path, ast::Path)>) -> TypeRefId {
        let fixities = ops
            .iter()
            .map(|(op, src)| {
                let resolved = self.resolve_path(op, src);
                let (prec, assoc) = resolved
                    .map(|def| match def {
                        | TypeDefId::FixityId(id) => {
                            let data = crate::data::fixity_data(self.db, id);
                            match data.kind(self.db) {
                                | FixityKind::Infix(assoc, prec) => (prec, assoc),
                                | _ => (Prec::ZERO, Assoc::Left), // TODO: report error
                            }
                        },
                        | _ => (Prec::ZERO, Assoc::Left),
                    })
                    .unwrap_or((Prec::ZERO, Assoc::Left));

                (resolved, prec, assoc)
            })
            .collect::<Vec<_>>();

        return go(
            self,
            ops.into_iter(),
            fixities.into_iter().peekable(),
            items.into_iter(),
        );

        use std::iter::{once, Peekable};

        fn go(
            ctx: &mut Ctx,
            mut ops: impl Iterator<Item = (Path, ast::Path)>,
            mut fixities: Peekable<impl Iterator<Item = (Option<TypeDefId>, Prec, Assoc)>>,
            mut items: impl Iterator<Item = TypeRefId>,
        ) -> TypeRefId {
            if let Some((op, op_src)) = ops.next() {
                let (id, prec, assoc) = fixities.next().unwrap();
                let left = if let Some(&(id2, prec2, _)) = fixities.peek() {
                    if id == id2 {
                        match assoc {
                            | Assoc::Left => true,
                            | Assoc::Right => false,
                            | Assoc::None => todo!(),
                        }
                    } else if prec >= prec2 {
                        true
                    } else {
                        false
                    }
                } else {
                    let lhs = items.next().unwrap_or_else(|| ctx.missing_type());
                    let rhs = items.next().unwrap_or_else(|| ctx.missing_type());

                    return ctx.make_infix_app(id, op, op_src, lhs, rhs);
                };

                if left {
                    let lhs = items.next().unwrap_or_else(|| ctx.missing_type());
                    let rhs = items.next().unwrap_or_else(|| ctx.missing_type());
                    let item = ctx.make_infix_app(id, op, op_src, lhs, rhs);
                    let items = once(item).chain(items).collect::<Vec<_>>();

                    go(ctx, ops, fixities, items.into_iter())
                } else {
                    let lhs = items.next().unwrap_or_else(|| ctx.missing_type());
                    let rhs = go(ctx, ops, fixities, items);

                    ctx.make_infix_app(id, op, op_src, lhs, rhs)
                }
            } else {
                items.next().unwrap_or_else(|| ctx.missing_type())
            }
        }
    }

    fn resolve_path(&self, path: &Path, ast: &ast::Path) -> Option<TypeDefId> {
        if let Some(name) = path.as_name() {
            if let Some(&var) = self.vars.get(&name) {
                return Some(TypeDefId::TypeVarId(var));
            }
        }

        let module = self.owner.module(self.db);

        if let Some(item) = self.def_map.resolve_path(self.db, path, module) {
            if let None = item.types {
                // TODO: report error: not a type
                return None;
            }

            let id = match item.types.unwrap() {
                | ItemId::FixityId(id) => TypeDefId::FixityId(id),
                | ItemId::TypeAliasId(id) => TypeDefId::TypeAliasId(id),
                | ItemId::TypeCtorId(id) => TypeDefId::TypeCtorId(id),
                | ItemId::TraitId(id) => TypeDefId::TraitId(id),
                | i => unreachable!("{i:?}"),
            };

            return Some(id);
        }

        Diagnostics::emit(self.db, UnresolvedPath {
            file: self.src.file,
            ast: AstPtr::new(ast),
            path: path.clone(),
            ns: Namespace::Types,
        });

        None
    }
}

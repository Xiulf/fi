pub mod attrs;
pub mod db;
pub mod diagnostic;
pub mod display;
mod from_id;
pub mod semantics;
pub mod source_analyzer;
mod source_to_def;

use base_db::input::FileId;
use base_db::libs::LibId;
use either::Either;
pub use hir_def::attrs::Documentation;
pub use hir_def::body::Body;
use hir_def::body::{PatSource, SyntheticSyntax};
pub use hir_def::data::FixityKind;
use hir_def::diagnostic::DiagnosticSink;
pub use hir_def::expr::{CaseArm, CaseValue, Expr, ExprId, Literal, Stmt};
pub use hir_def::id;
use hir_def::id::*;
pub use hir_def::in_file::InFile;
use hir_def::item_scope::ExportNs;
pub use hir_def::item_tree::{Assoc, Prec};
pub use hir_def::name::{AsName, Name};
pub use hir_def::pat::{Pat, PatId};
pub use hir_def::path::Path;
pub use hir_def::resolver::{HasResolver, Resolver, TypeNs, ValueNs};
use hir_def::visibility::Visibility;
pub use hir_ty::class::ClassEnvPath;
use hir_ty::db::HirDatabase;
pub use hir_ty::display::HirDisplay;
pub use hir_ty::infer::{InferenceResult, MethodSource};
pub use hir_ty::ty;
use hir_ty::ty::Ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lib {
    pub(crate) id: LibId,
}

#[derive(Debug)]
pub struct LibDep {
    pub lib: Lib,
    pub name: Name,
}

impl Lib {
    pub fn dependencies(self, db: &dyn HirDatabase) -> Vec<LibDep> {
        let libs = db.libs();

        libs[self.id]
            .deps
            .iter()
            .map(|&dep| {
                let lib = Lib { id: dep };
                let name = libs[dep].name.as_name();

                LibDep { lib, name }
            })
            .collect()
    }

    pub fn modules(self, db: &dyn HirDatabase) -> Vec<Module> {
        let def_map = db.def_map(self.id);

        def_map
            .modules()
            .filter(|(_, m)| m.parent.is_none())
            .map(|(id, _)| Module {
                id: def_map.module_id(id),
            })
            .collect()
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.libs()[self.id].name.as_name()
    }

    pub fn all(db: &dyn HirDatabase) -> Vec<Lib> {
        db.libs().toposort(None).into_iter().map(|id| Lib { id }).collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    pub(crate) id: ModuleId,
}

impl Module {
    pub fn name(self, db: &dyn HirDatabase) -> Name {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id].name.clone()
    }

    pub fn lib(self) -> Lib {
        Lib { id: self.id.lib }
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        let def_map = db.def_map(self.id.lib);
        let mut local_id = self.id.local_id;

        while let Some(parent) = def_map[local_id].parent {
            local_id = parent;
        }

        def_map[local_id].origin.file_id().unwrap()
    }

    pub fn parent(self, db: &dyn HirDatabase) -> Option<Module> {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id].parent.map(|p| def_map.module_id(p).into())
    }

    pub fn is_virtual(self, db: &dyn HirDatabase) -> bool {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id].origin.is_virtual()
    }

    pub fn path_to_name(self, db: &dyn HirDatabase, name: Name) -> Path {
        std::iter::successors(Some(self), |m| m.parent(db))
            .collect::<Vec<_>>()
            .into_iter()
            .map(|m| {
                m.name(db)
                    .as_ref()
                    .split(Path::SEPARATOR)
                    .map(|s| s.as_name())
                    .collect::<Vec<_>>()
            })
            .rev()
            .flatten()
            .chain(std::iter::once(name))
            .collect()
    }

    pub fn is_exported(self, db: &dyn HirDatabase, name: Name, ns: ExportNs) -> bool {
        let def_map = db.def_map(self.id.lib);
        let def = def_map[self.id.local_id].scope.get(&name);
        let vis = match ns {
            | ExportNs::Types => def.types.map(|t| t.1),
            | ExportNs::Values => def.values.map(|t| t.1),
            | ExportNs::Any => unreachable!(),
        };

        vis == Some(Visibility::Public)
    }

    pub fn children(self, db: &dyn HirDatabase) -> Vec<Module> {
        let def_map = db.def_map(self.id.lib);

        def_map
            .modules()
            .filter(|(_, m)| m.parent == Some(self.id.local_id))
            .map(|(id, _)| def_map.module_id(id).into())
            .collect()
    }

    pub fn declarations(self, db: &dyn HirDatabase) -> Vec<ModuleDef> {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id]
            .scope
            .declarations()
            .map(ModuleDef::from)
            .collect()
    }

    pub fn members(self, db: &dyn HirDatabase) -> Vec<Member> {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id].scope.members().map(Member::from).collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let def_map = db.def_map(self.id.lib);

        for diag in def_map.diagnostics() {
            diag.add_to(db.upcast(), self.id.local_id, sink);
        }

        for decl in self.declarations(db) {
            decl.diagnostics(db, sink);
        }

        for inst in self.members(db) {
            inst.diagnostics(db, sink);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathResolution {
    Def(ModuleDef),
    Local(Local),
    TypeVar(TypeVar),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDef {
    Module(Module),
    Fixity(Fixity),
    Func(Func),
    Static(Static),
    Const(Const),
    TypeAlias(TypeAlias),
    TypeCtor(TypeCtor),
    Ctor(Ctor),
    Class(Class),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefWithBody {
    Func(Func),
    Static(Static),
    Const(Const),
}

impl ModuleDef {
    pub fn module(self, db: &dyn HirDatabase) -> Option<Module> {
        match self {
            | ModuleDef::Module(_) => None,
            | ModuleDef::Fixity(it) => Some(it.module(db)),
            | ModuleDef::Func(it) => Some(it.module(db)),
            | ModuleDef::Static(it) => Some(it.module(db)),
            | ModuleDef::Const(it) => Some(it.module(db)),
            | ModuleDef::Ctor(it) => Some(it.module(db)),
            | ModuleDef::TypeAlias(it) => Some(it.module(db)),
            | ModuleDef::TypeCtor(it) => Some(it.module(db)),
            | ModuleDef::Class(it) => Some(it.module(db)),
        }
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        let name = self.name(db);

        match self.module(db) {
            | Some(module) => module.path_to_name(db, name),
            | None => name.to_string().split(Path::SEPARATOR).map(|s| s.as_name()).collect(),
        }
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        match self {
            | ModuleDef::Module(it) => it.name(db),
            | ModuleDef::Fixity(it) => it.name(db),
            | ModuleDef::Func(it) => it.name(db),
            | ModuleDef::Static(it) => it.name(db),
            | ModuleDef::Const(it) => it.name(db),
            | ModuleDef::TypeAlias(it) => it.name(db),
            | ModuleDef::TypeCtor(it) => it.name(db),
            | ModuleDef::Ctor(it) => it.name(db),
            | ModuleDef::Class(it) => it.name(db),
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        match self {
            | ModuleDef::Module(_) => return,
            | ModuleDef::Fixity(it) => it.diagnostics(db, sink),
            | ModuleDef::Func(it) => it.diagnostics(db, sink),
            | ModuleDef::Static(it) => it.diagnostics(db, sink),
            | ModuleDef::Const(it) => it.diagnostics(db, sink),
            | ModuleDef::TypeAlias(it) => it.diagnostics(db, sink),
            | ModuleDef::TypeCtor(it) => it.diagnostics(db, sink),
            | ModuleDef::Ctor(it) => it.diagnostics(db, sink),
            | ModuleDef::Class(it) => it.diagnostics(db, sink),
        }
    }
}

impl DefWithBody {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        match self {
            | DefWithBody::Func(it) => it.module(db),
            | DefWithBody::Static(it) => it.module(db),
            | DefWithBody::Const(it) => it.module(db),
        }
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        let name = self.name(db);
        let module = self.module(db);

        module.path_to_name(db, name)
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        match self {
            | DefWithBody::Func(it) => it.name(db),
            | DefWithBody::Static(it) => it.name(db),
            | DefWithBody::Const(it) => it.name(db),
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        match self {
            | DefWithBody::Func(it) => it.diagnostics(db, sink),
            | DefWithBody::Static(it) => it.diagnostics(db, sink),
            | DefWithBody::Const(it) => it.diagnostics(db, sink),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Fixity {
    pub(crate) id: FixityId,
}

impl Fixity {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.fixity_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn kind(self, db: &dyn HirDatabase) -> FixityKind {
        db.fixity_data(self.id).kind
    }

    pub fn func(self, db: &dyn HirDatabase) -> Func {
        let path = &db.fixity_data(self.id).func;
        let resolver = self.id.resolver(db.upcast());
        let id = resolver.resolve_value_fully(db.upcast(), path);

        if let Some((hir_def::resolver::ValueNs::Func(id), _)) = id {
            Func::from(id)
        } else {
            unreachable!();
        }
    }

    pub fn diagnostics(self, _db: &dyn HirDatabase, _sink: &mut DiagnosticSink) {
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Func {
    pub(crate) id: FuncId,
}

impl Func {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module(db.upcast()),
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.func_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn ty(self, db: &dyn HirDatabase) -> ty::Ty {
        db.value_ty(self.id.into()).ty
    }

    pub fn has_body(self, db: &dyn HirDatabase) -> bool {
        db.func_data(self.id).has_body
    }

    pub fn is_foreign(self, db: &dyn HirDatabase) -> bool {
        db.func_data(self.id).is_foreign
    }

    pub fn is_intrinsic(self, db: &dyn HirDatabase) -> bool {
        db.attrs(self.id.into()).by_key("intrinsic").exists()
    }

    pub fn link_name(self, db: &dyn HirDatabase) -> (Name, bool) {
        let attrs = db.attrs(self.id.into());
        let mut link_name = attrs.by_key("link_name").string_value();

        if let Some(name) = link_name.next() {
            (name.as_name(), false)
        } else if attrs.by_key("no_mangle").exists() {
            (self.name(db), false)
        } else if self.is_foreign(db) {
            (self.name(db), false)
        } else {
            let mut path = self.path(db);

            if let Some(it) = self.as_assoc_item(db) {
                if let AssocItemContainer::Member(inst) = it.container(db) {
                    let name = inst.link_name(db);

                    path.to_instance(name);
                }
            }

            (path.to_string().as_name(), true)
        }
    }

    pub fn as_assoc_item(self, db: &dyn HirDatabase) -> Option<AssocItem> {
        match self.id.lookup(db.upcast()).container {
            | ContainerId::Class(_) | ContainerId::Member(_) => Some(AssocItem::Func(self)),
            | ContainerId::Module(_) => None,
        }
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        if let Some(assoc) = self.as_assoc_item(db) {
            matches!(assoc.container(db), AssocItemContainer::Member(_))
        } else {
            self.module(db).is_exported(db, self.name(db), ExportNs::Values)
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let infer = db.infer(self.id.into());
        // let data = db.func_data(self.id);
        // let body = db.body(self.id.into());

        // if data.name.as_ref() == "test" {
        // eprintln!("{:?}:", self.id.lookup(db.upcast()).container);
        // eprintln!("{} :: {}", data.name, infer.self_type.ty.display(db));

        // for ((id, i), method) in &infer.methods {
        //     eprintln!("{:?}#{} -> {:?}", id, i, method);
        // }

        // eprintln!();

        // for (expr, ty) in infer.type_of_expr.iter() {
        //     eprintln!("{:?} -> {:?} :: {}", expr, body[expr], ty.display(db));
        // }

        // eprintln!();

        // for (pat, ty) in infer.type_of_pat.iter() {
        //     eprintln!("{:?} -> {:?} :: {}", pat, body[pat], ty.display(db));
        // }

        // eprintln!();
        // }

        infer.add_diagnostics(db, self.id.into(), sink);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Static {
    pub(crate) id: StaticId,
}

impl Static {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module(db.upcast()),
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.static_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn link_name(self, db: &dyn HirDatabase) -> (Name, bool) {
        let attrs = db.attrs(AttrDefId::StaticId(self.id));
        let mut link_name = attrs.by_key("link_name").string_value();

        if let Some(name) = link_name.next() {
            (name.as_name(), false)
        } else if attrs.by_key("no_mangle").exists() {
            (self.name(db), false)
        } else if self.is_foreign(db) {
            (self.name(db), false)
        } else {
            let mut path = self.path(db);

            if let Some(it) = self.as_assoc_item(db) {
                if let AssocItemContainer::Member(inst) = it.container(db) {
                    let name = inst.link_name(db);

                    path.to_instance(name);
                }
            }

            (path.to_string().as_name(), true)
        }
    }

    pub fn as_assoc_item(self, db: &dyn HirDatabase) -> Option<AssocItem> {
        match self.id.lookup(db.upcast()).container {
            | ContainerId::Class(_) | ContainerId::Member(_) => Some(AssocItem::Static(self)),
            | ContainerId::Module(_) => None,
        }
    }

    pub fn is_foreign(self, db: &dyn HirDatabase) -> bool {
        db.static_data(self.id).is_foreign
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        if let Some(assoc) = self.as_assoc_item(db) {
            matches!(assoc.container(db), AssocItemContainer::Member(_))
        } else {
            self.module(db).is_exported(db, self.name(db), ExportNs::Values)
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let infer = db.infer(self.id.into());

        infer.add_diagnostics(db, self.id.into(), sink);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Const {
    pub(crate) id: ConstId,
}

impl Const {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module(db.upcast()),
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.const_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let infer = db.infer(self.id.into());

        infer.add_diagnostics(db, self.id.into(), sink);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeAlias {
    pub(crate) id: TypeAliasId,
}

impl TypeAlias {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.type_alias_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn type_vars(self, db: &dyn HirDatabase) -> Vec<TypeVar> {
        db.type_alias_data(self.id)
            .type_vars
            .iter()
            .map(|&local_id| TypeVar {
                id: TypeVarId {
                    owner: TypeVarOwner::TypedDefId(TypedDefId::TypeAliasId(self.id)),
                    local_id,
                },
            })
            .collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let lower = db.type_for_alias(self.id);

        lower.add_diagnostics(db, TypeVarOwner::TypedDefId(self.id.into()), sink);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeCtor {
    pub(crate) id: TypeCtorId,
}

impl TypeCtor {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn is_foreign(self, db: &dyn HirDatabase) -> bool {
        db.type_ctor_data(self.id).is_foreign
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.type_ctor_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        self.module(db).is_exported(db, self.name(db), ExportNs::Types)
    }

    pub fn kind(self, db: &dyn HirDatabase) -> Ty {
        db.kind_for_ctor(self.id).ty.ty
    }

    pub fn type_vars(self, db: &dyn HirDatabase) -> Vec<TypeVar> {
        db.type_ctor_data(self.id)
            .type_vars
            .iter()
            .map(|&local_id| TypeVar {
                id: TypeVarId {
                    owner: TypeVarOwner::TypedDefId(TypedDefId::TypeCtorId(self.id)),
                    local_id,
                },
            })
            .collect()
    }

    pub fn ctors(self, db: &dyn HirDatabase) -> Vec<Ctor> {
        db.type_ctor_data(self.id)
            .ctors
            .iter()
            .map(|(id, _)| Ctor { parent: self, id })
            .collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let lower = db.kind_for_ctor(self.id);

        lower.add_diagnostics(db, TypeVarOwner::TypedDefId(self.id.into()), sink);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ctor {
    pub(crate) parent: TypeCtor,
    pub(crate) id: LocalCtorId,
}

impl Ctor {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        self.parent.module(db)
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.parent.file_id(db)
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.type_ctor_data(self.parent.id).ctors[self.id].name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        self.module(db).is_exported(db, self.name(db), ExportNs::Values)
    }

    pub fn type_ctor(self) -> TypeCtor {
        self.parent
    }

    pub fn types(self, db: &dyn HirDatabase) -> Vec<Ty> {
        let lower = db.ctor_ty(CtorId {
            parent: self.parent.id,
            local_id: self.id,
        });

        let data = db.type_ctor_data(self.parent.id);
        let ctor = &data.ctors[self.id];

        ctor.types.iter().map(|&t| lower.types[t]).collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let id = CtorId {
            parent: self.parent.id,
            local_id: self.id,
        };

        let lower = db.ctor_ty(id);

        lower.add_diagnostics(db, TypeVarOwner::TypedDefId(id.into()), sink);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Class {
    pub(crate) id: ClassId,
}

impl Class {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.class_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        self.module(db).is_exported(db, self.name(db), ExportNs::Types)
    }

    pub fn constraints(self, _db: &dyn HirDatabase) -> Vec<ty::Constraint> {
        Vec::new()
    }

    pub fn items(self, db: &dyn HirDatabase) -> Vec<AssocItem> {
        db.class_data(self.id)
            .items
            .iter()
            .map(|(_, id)| (*id).into())
            .collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let lower = db.lower_class(self.id);

        lower.add_diagnostics(db, TypeVarOwner::TypedDefId(self.id.into()), sink);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Member {
    pub(crate) id: MemberId,
}

impl Member {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        self.id.lookup(db.upcast()).id.file_id
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        self.class(db).is_exported(db)
    }

    pub fn link_name(self, db: &dyn HirDatabase) -> Name {
        use salsa::InternKey;
        let lower = db.lower_member(self.id);
        let name = Class::from(lower.member.class).name(db);
        let id = u32::from(self.id.as_intern_id());

        format!("$member_{}_{}", name, id).as_name()
    }

    pub fn class(self, db: &dyn HirDatabase) -> Class {
        let lower = db.lower_member(self.id);

        lower.member.class.into()
    }

    pub fn items(self, db: &dyn HirDatabase) -> Vec<AssocItem> {
        db.member_data(self.id)
            .items
            .iter()
            .map(|(_, id)| (*id).into())
            .collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let lower = db.lower_member(self.id);
        let diags = db.verify_member(self.id);
        let owner = TypeVarOwner::TypedDefId(self.id.into());

        lower.add_diagnostics(db, owner, sink);
        diags.add_diagnostics(db, owner, sink);

        for item in self.items(db) {
            item.diagnostics(db, sink);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssocItem {
    Func(Func),
    Static(Static),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssocItemContainer {
    Class(Class),
    Member(Member),
}

impl AssocItem {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        match self {
            | AssocItem::Func(it) => it.module(db),
            | AssocItem::Static(it) => it.module(db),
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn file_id(self, db: &dyn HirDatabase) -> FileId {
        match self {
            | AssocItem::Func(it) => it.file_id(db),
            | AssocItem::Static(it) => it.file_id(db),
        }
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        match self {
            | AssocItem::Func(it) => it.name(db),
            | AssocItem::Static(it) => it.name(db),
        }
    }

    pub fn container(self, db: &dyn HirDatabase) -> AssocItemContainer {
        let id = match self {
            | AssocItem::Func(it) => it.id.lookup(db.upcast()).container,
            | AssocItem::Static(it) => it.id.lookup(db.upcast()).container,
        };

        match id {
            | ContainerId::Class(id) => AssocItemContainer::Class(id.into()),
            | ContainerId::Member(id) => AssocItemContainer::Member(id.into()),
            | ContainerId::Module(_) => unreachable!(),
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        match self {
            | AssocItem::Func(it) => it.diagnostics(db, sink),
            | AssocItem::Static(it) => it.diagnostics(db, sink),
        }
    }
}

impl From<AssocItemId> for AssocItem {
    fn from(id: AssocItemId) -> Self {
        match id {
            | AssocItemId::FuncId(id) => AssocItem::Func(id.into()),
            | AssocItemId::StaticId(id) => AssocItem::Static(id.into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Local {
    pub(crate) parent: DefWithBodyId,
    pub(crate) pat_id: PatId,
}

impl Local {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        self.parent.module(db.upcast()).into()
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        let body = db.body(self.parent);

        match &body[self.pat_id] {
            | Pat::Bind { name, .. } => name.clone(),
            | _ => unreachable!(),
        }
    }

    pub fn ty(self, db: &dyn HirDatabase) -> Ty {
        let infer = db.infer(self.parent);
        infer.type_of_pat[self.pat_id]
    }

    pub fn source(self, db: &dyn HirDatabase) -> Either<PatSource, SyntheticSyntax> {
        let (_, source_map) = db.body_source_map(self.parent);
        source_map.pat_syntax(self.pat_id)
    }
}

impl From<(DefWithBodyId, PatId)> for Local {
    fn from((parent, pat_id): (DefWithBodyId, PatId)) -> Self {
        Self { parent, pat_id }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub(crate) id: TypeVarId,
}

impl TypeVar {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        self.id.owner.module(db.upcast()).into()
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        match self.id.owner {
            | TypeVarOwner::DefWithBodyId(id) => db.body(id).type_map()[self.id.local_id].name.clone(),
            | TypeVarOwner::TypedDefId(id) => match id {
                | TypedDefId::FuncId(id) => db.func_data(id).type_map()[self.id.local_id].name.clone(),
                | TypedDefId::StaticId(id) => db.static_data(id).type_map()[self.id.local_id].name.clone(),
                | TypedDefId::ConstId(id) => db.const_data(id).type_map()[self.id.local_id].name.clone(),
                | TypedDefId::TypeAliasId(id) => db.type_alias_data(id).type_map()[self.id.local_id].name.clone(),
                | TypedDefId::TypeCtorId(id) => db.type_ctor_data(id).type_map()[self.id.local_id].name.clone(),
                | TypedDefId::CtorId(id) => db.type_ctor_data(id.parent).type_map()[self.id.local_id].name.clone(),
                | TypedDefId::ClassId(id) => db.class_data(id).type_map()[self.id.local_id].name.clone(),
                | TypedDefId::MemberId(id) => db.member_data(id).type_map()[self.id.local_id].name.clone(),
            },
        }
    }
}

macro_rules! impl_from {
    ($($variant:ident),* for $ty:ident) => {
        $(
            impl From<$variant> for $ty {
                fn from(src: $variant) -> Self {
                    Self::$variant(src)
                }
            }
        )*
    };
}

impl Into<DefWithBodyId> for DefWithBody {
    fn into(self) -> DefWithBodyId {
        match self {
            | Self::Func(f) => DefWithBodyId::FuncId(f.id),
            | Self::Static(f) => DefWithBodyId::StaticId(f.id),
            | Self::Const(f) => DefWithBodyId::ConstId(f.id),
        }
    }
}

impl From<DefWithBodyId> for DefWithBody {
    fn from(id: DefWithBodyId) -> Self {
        match id {
            | DefWithBodyId::FuncId(id) => DefWithBody::Func(Func { id }),
            | DefWithBodyId::StaticId(id) => DefWithBody::Static(Static { id }),
            | DefWithBodyId::ConstId(id) => DefWithBody::Const(Const { id }),
        }
    }
}

impl_from!(Fixity, Func, Static, Const, TypeAlias, TypeCtor, Ctor, Class for ModuleDef);
impl_from!(Func, Static, Const for DefWithBody);
impl_from!(Func, Static for AssocItem);

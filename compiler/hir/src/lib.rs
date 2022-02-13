pub mod db;
pub mod diagnostic;
mod from_id;
pub mod semantics;
pub mod source_analyzer;
mod source_to_def;

use base_db::input::FileId;
use base_db::libs::LibId;
pub use hir_def::body::Body;
use hir_def::diagnostic::DiagnosticSink;
pub use hir_def::expr::{CaseArm, Expr, ExprId, Literal, Stmt};
use hir_def::id::*;
pub use hir_def::in_file::InFile;
pub use hir_def::item_tree::{Assoc, Prec};
pub use hir_def::name::{AsName, Name};
pub use hir_def::pat::{Pat, PatId};
pub use hir_def::path::Path;
pub use hir_def::{arena, attrs, id};
use hir_ty::db::HirDatabase;
pub use hir_ty::display::HirDisplay;
pub use hir_ty::infer::{InferenceResult, MethodSource};
pub use hir_ty::{display, ty};

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
            .map(|(id, _)| Module {
                id: def_map.module_id(id),
            })
            .collect()
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.libs()[self.id].name.as_name()
    }

    pub fn all(db: &dyn HirDatabase) -> Vec<Lib> {
        db.libs().toposort().into_iter().map(|id| Lib { id }).collect()
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

        def_map[self.id.local_id].origin.file_id(&def_map)
    }

    pub fn is_virtual(self, db: &dyn HirDatabase) -> bool {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id].origin.is_virtual()
    }

    pub fn path_to_name(self, db: &dyn HirDatabase, name: Name) -> Path {
        self.name(db)
            .to_string()
            .split('/')
            .map(|s| s.as_name())
            .chain(std::iter::once(name))
            .collect()
    }

    pub fn is_exported(self, db: &dyn HirDatabase, name: Name) -> bool {
        let def_map = db.def_map(self.id.lib);

        true
        // !def_map[self.id.local_id]
        //     .exports
        //     .get(db.upcast(), &def_map, self.id.local_id, &name)
        //     .is_none()
    }

    pub fn declarations(self, db: &dyn HirDatabase) -> Vec<ModuleDef> {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id]
            .scope
            .declarations()
            .map(ModuleDef::from)
            .collect()
    }

    pub fn instances(self, db: &dyn HirDatabase) -> Vec<Instance> {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id]
            .scope
            .instances()
            .map(Instance::from)
            .collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let file = self.file_id(db);
        let item_tree = db.item_tree(file);
        let def_map = db.def_map(self.id.lib);

        for diag in &item_tree.diagnostics {
            diag.add_to(db.upcast(), &item_tree, sink);
        }

        for diag in def_map.diagnostics() {
            diag.add_to(db.upcast(), self.id.local_id, sink);
        }

        for decl in self.declarations(db) {
            decl.diagnostics(db, sink);
        }

        for inst in self.instances(db) {
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

    pub fn assoc(self, db: &dyn HirDatabase) -> Assoc {
        db.fixity_data(self.id).assoc
    }

    pub fn prec(self, db: &dyn HirDatabase) -> Prec {
        db.fixity_data(self.id).prec
    }

    pub fn func(self, db: &dyn HirDatabase) -> Func {
        let path = &db.fixity_data(self.id).func;
        let resolver = hir_def::resolver::HasResolver::resolver(self.id, db.upcast());
        let id = resolver.resolve_value_fully(db.upcast(), path);

        if let Some((hir_def::resolver::ValueNs::Func(id), _)) = id {
            Func::from(id)
        } else {
            unreachable!();
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
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
        db.value_ty(self.id.into())
    }

    pub fn has_body(self, db: &dyn HirDatabase) -> bool {
        db.func_data(self.id).has_body
    }

    pub fn is_foreign(self, db: &dyn HirDatabase) -> bool {
        db.func_data(self.id).is_foreign
    }

    pub fn link_name(self, db: &dyn HirDatabase) -> Name {
        if self.is_foreign(db) {
            self.name(db)
        } else {
            let mut path = self.path(db);

            if let Some(it) = self.as_assoc_item(db) {
                if let AssocItemContainer::Instance(inst) = it.container(db) {
                    let name = inst.link_name(db);

                    path.to_instance(name);
                }
            }

            path.to_string().as_name()
        }
    }

    pub fn as_assoc_item(self, db: &dyn HirDatabase) -> Option<AssocItem> {
        match self.id.lookup(db.upcast()).container {
            | ContainerId::Class(_) | ContainerId::Instance(_) => Some(AssocItem::Func(self)),
            | ContainerId::Module(_) => None,
        }
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        if let Some(assoc) = self.as_assoc_item(db) {
            matches!(assoc.container(db), AssocItemContainer::Instance(_))
        } else {
            self.module(db).is_exported(db, self.name(db))
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let data = db.func_data(self.id);
        let infer = db.infer(self.id.into());
        let lower = hir_ty::lower::func_ty(db, self.id);
        let body = db.body(self.id.into());

        // eprintln!("fun {} :: {}", data.name, lower.ty.display(db));
        //
        // for (expr, ty) in infer.type_of_expr.iter() {
        //     eprintln!("{:?} :: {}", body[expr], ty.display(db));
        // }
        //
        // for (pat, ty) in infer.type_of_pat.iter() {
        //     eprintln!("{:?} :: {}", body[pat], ty.display(db));
        // }
        //
        // eprintln!();

        infer.add_diagnostics(db, self.id.into(), sink);
        lower.add_diagnostics(db, TypeVarOwner::TypedDefId(self.id.into()), sink);
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

    pub fn link_name(self, db: &dyn HirDatabase) -> Name {
        if self.is_foreign(db) {
            self.name(db)
        } else {
            let mut path = self.path(db);

            if let Some(it) = self.as_assoc_item(db) {
                if let AssocItemContainer::Instance(inst) = it.container(db) {
                    let name = inst.link_name(db);

                    path.to_instance(name);
                }
            }

            path.to_string().as_name()
        }
    }

    pub fn as_assoc_item(self, db: &dyn HirDatabase) -> Option<AssocItem> {
        match self.id.lookup(db.upcast()).container {
            | ContainerId::Class(_) | ContainerId::Instance(_) => Some(AssocItem::Static(self)),
            | ContainerId::Module(_) => None,
        }
    }

    pub fn is_foreign(self, db: &dyn HirDatabase) -> bool {
        db.static_data(self.id).is_foreign
    }

    pub fn is_exported(self, db: &dyn HirDatabase) -> bool {
        if let Some(assoc) = self.as_assoc_item(db) {
            matches!(assoc.container(db), AssocItemContainer::Instance(_))
        } else {
            self.module(db).is_exported(db, self.name(db))
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.type_ctor_data(self.id).name.clone()
    }

    pub fn path(self, db: &dyn HirDatabase) -> Path {
        self.module(db).path_to_name(db, self.name(db))
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let lower = db.type_for_ctor(self.id);

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

    pub fn type_ctor(self) -> TypeCtor {
        self.parent
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

    pub fn constraints(self, db: &dyn HirDatabase) -> Vec<ty::Constraint> {
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
pub struct Instance {
    pub(crate) id: InstanceId,
}

impl Instance {
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

    pub fn link_name(self, db: &dyn HirDatabase) -> Name {
        let data = db.instance_data(self.id);
        let type_map = data.type_map();
        let mut name = data.class.segments().last().unwrap().to_string();

        for (_i, ty) in data.types.iter().enumerate() {
            name.push('_');
            ty_link_name(&type_map, ty, &mut name).unwrap();
        }

        name.as_name()
    }

    pub fn class(self, db: &dyn HirDatabase) -> Class {
        let lower = db.lower_instance(self.id);

        lower.instance.class.into()
    }

    pub fn items(self, db: &dyn HirDatabase) -> Vec<AssocItem> {
        db.instance_data(self.id)
            .items
            .iter()
            .map(|(_, id)| (*id).into())
            .collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let lower = db.lower_instance(self.id);

        lower.add_diagnostics(db, TypeVarOwner::TypedDefId(self.id.into()), sink);

        for item in self.items(db) {
            item.diagnostics(db, sink);
        }
    }
}

fn ty_link_name(
    map: &hir_def::type_ref::TypeMap,
    ty: &hir_def::type_ref::LocalTypeRefId,
    out: &mut String,
) -> std::fmt::Result {
    use hir_def::type_ref::{PtrLen, TypeRef};
    use std::fmt::Write;

    match &map[*ty] {
        | TypeRef::Error => write!(out, "error"),
        | TypeRef::Placeholder => write!(out, "_"),
        | TypeRef::Figure(i) => write!(out, "{}", i),
        | TypeRef::Symbol(s) => write!(out, "{:?}", s),
        | TypeRef::Kinded(t, k) => {
            write!(out, "(")?;
            ty_link_name(map, t, out)?;
            write!(out, " :: ")?;
            ty_link_name(map, k, out)?;
            write!(out, ")")
        },
        | TypeRef::App(a, b) => {
            write!(out, "(")?;
            ty_link_name(map, a, out)?;
            write!(out, " ")?;
            ty_link_name(map, b, out)?;
            write!(out, ")")
        },
        | TypeRef::Tuple(ts) => {
            write!(out, "(")?;

            for (i, t) in ts.iter().enumerate() {
                if i != 0 {
                    write!(out, ", ")?;
                }

                ty_link_name(map, t, out)?;
            }

            write!(out, ")")
        },
        | TypeRef::Path(p) => write!(out, "{}", p),
        | TypeRef::Ptr(t, PtrLen::Single) => {
            write!(out, "*")?;
            ty_link_name(map, t, out)
        },
        | TypeRef::Ptr(t, PtrLen::Multiple(None)) => {
            write!(out, "[*]")?;
            ty_link_name(map, t, out)
        },
        | TypeRef::Ptr(t, PtrLen::Multiple(Some(s))) => {
            write!(out, "[*:{}]", s.0)?;
            ty_link_name(map, t, out)
        },
        | TypeRef::Slice(t) => {
            write!(out, "[]")?;
            ty_link_name(map, t, out)
        },
        | TypeRef::Array(t, len) => {
            write!(out, "[{}]", len)?;
            ty_link_name(map, t, out)
        },
        | _ => unimplemented!(),
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
    Instance(Instance),
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
            | ContainerId::Instance(id) => AssocItemContainer::Instance(id.into()),
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
    pub fn name(self, db: &dyn HirDatabase) -> Name {
        let body = db.body(self.parent);

        match &body[self.pat_id] {
            | Pat::Bind { name, .. } => name.clone(),
            | _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub(crate) id: TypeVarId,
}

impl TypeVar {
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
                | TypedDefId::InstanceId(id) => db.instance_data(id).type_map()[self.id.local_id].name.clone(),
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

impl_from!(Fixity, Func, Static, Const, TypeAlias, TypeCtor, Ctor, Class for ModuleDef);
impl_from!(Func, Static for AssocItem);

use crate::arena::Idx;
use crate::db::DefDatabase;
use crate::def_map::ModuleData;
use crate::in_file::InFile;
use crate::item_tree::*;
use crate::type_ref::{LocalTypeVarId, TypeMap, TypeSourceMap};
use base_db::libs::LibId;
use std::hash::{Hash, Hasher};

macro_rules! impl_intern {
    ($id:ident, $loc:path, $intern:ident, $lookup:ident) => {
        impl salsa::InternKey for $id {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $id(v)
            }

            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }

        impl Intern for $loc {
            type ID = $id;

            fn intern(self, db: &dyn DefDatabase) -> $id {
                db.$intern(self)
            }
        }

        impl Lookup for $id {
            type Data = $loc;

            fn lookup(&self, db: &dyn DefDatabase) -> $loc {
                db.$lookup(*self)
            }
        }
    };
}

pub trait Intern {
    type ID;

    fn intern(self, db: &dyn DefDatabase) -> Self::ID;
}

pub trait Lookup {
    type Data;

    fn lookup(&self, db: &dyn DefDatabase) -> Self::Data;
}

pub trait HasModule {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId;
}

pub trait HasSource {
    type Value;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId {
    pub lib: LibId,
    pub local_id: LocalModuleId,
}

pub type LocalModuleId = Idx<ModuleData>;

#[derive(Debug)]
pub struct ItemLoc<N: ItemTreeNode> {
    pub module: ModuleId,
    pub id: ItemTreeId<N>,
}

#[derive(Debug)]
pub struct AssocItemLoc<N: ItemTreeNode> {
    pub container: ContainerId,
    pub id: ItemTreeId<N>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContainerId {
    Module(ModuleId),
    Class(ClassId),
    Instance(InstanceId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FixityId(salsa::InternId);
pub type FixityLoc = ItemLoc<Fixity>;
impl_intern!(FixityId, FixityLoc, intern_fixity, lookup_intern_fixity);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(salsa::InternId);
pub type FuncLoc = AssocItemLoc<Func>;
impl_intern!(FuncId, FuncLoc, intern_func, lookup_intern_func);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StaticId(salsa::InternId);
pub type StaticLoc = AssocItemLoc<Static>;
impl_intern!(StaticId, StaticLoc, intern_static, lookup_intern_static);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstId(salsa::InternId);
pub type ConstLoc = ItemLoc<Const>;
impl_intern!(ConstId, ConstLoc, intern_const, lookup_intern_const);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeAliasId(salsa::InternId);
pub type TypeAliasLoc = ItemLoc<TypeAlias>;
impl_intern!(TypeAliasId, TypeAliasLoc, intern_type_alias, lookup_intern_type_alias);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeCtorId(salsa::InternId);
pub type TypeCtorLoc = ItemLoc<TypeCtor>;
impl_intern!(TypeCtorId, TypeCtorLoc, intern_type_ctor, lookup_intern_type_ctor);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CtorId {
    pub parent: TypeCtorId,
    pub local_id: LocalCtorId,
}

pub type LocalCtorId = Idx<crate::data::CtorData>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassId(salsa::InternId);
pub type ClassLoc = ItemLoc<Class>;
impl_intern!(ClassId, ClassLoc, intern_class, lookup_intern_class);

impl ClassId {
    pub fn dummy() -> Self {
        let id = salsa::InternId::from(salsa::InternId::MAX);

        Self(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstanceId(salsa::InternId);
pub type InstanceLoc = ItemLoc<Instance>;
impl_intern!(InstanceId, InstanceLoc, intern_instance, lookup_intern_instance);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId {
    pub owner: TypeVarOwner,
    pub local_id: LocalTypeVarId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeVarOwner {
    DefWithBodyId(DefWithBodyId),
    TypedDefId(TypedDefId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttrDefId {
    ModuleId(ModuleId),
    FixityId(FixityId),
    FuncId(FuncId),
    StaticId(StaticId),
    ConstId(ConstId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    ClassId(ClassId),
    InstanceId(InstanceId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDefId {
    ModuleId(ModuleId),
    FixityId(FixityId),
    FuncId(FuncId),
    StaticId(StaticId),
    ConstId(ConstId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    CtorId(CtorId),
    ClassId(ClassId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedDefId {
    FuncId(FuncId),
    StaticId(StaticId),
    TypeAliasId(TypeAliasId),
    TypeCtorId(TypeCtorId),
    CtorId(CtorId),
    ClassId(ClassId),
    InstanceId(InstanceId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueTyDefId {
    FuncId(FuncId),
    StaticId(StaticId),
    ConstId(ConstId),
    CtorId(CtorId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefWithBodyId {
    FuncId(FuncId),
    StaticId(StaticId),
    ConstId(ConstId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssocItemId {
    FuncId(FuncId),
    StaticId(StaticId),
}

impl HasModule for ContainerId {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId {
        match *self {
            | ContainerId::Module(id) => id,
            | ContainerId::Class(id) => id.lookup(db).module,
            | ContainerId::Instance(id) => id.lookup(db).module,
        }
    }
}

impl HasModule for ModuleDefId {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId {
        match *self {
            | ModuleDefId::ModuleId(m) => m,
            | ModuleDefId::FixityId(id) => id.lookup(db).module,
            | ModuleDefId::FuncId(id) => id.lookup(db).module(db),
            | ModuleDefId::StaticId(id) => id.lookup(db).module(db),
            | ModuleDefId::ConstId(id) => id.lookup(db).module(db),
            | ModuleDefId::TypeAliasId(id) => id.lookup(db).module,
            | ModuleDefId::TypeCtorId(id) => id.lookup(db).module,
            | ModuleDefId::CtorId(id) => id.parent.lookup(db).module(db),
            | ModuleDefId::ClassId(id) => id.lookup(db).module,
        }
    }
}

impl HasModule for TypedDefId {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId {
        match *self {
            | TypedDefId::FuncId(id) => id.lookup(db).module(db),
            | TypedDefId::StaticId(id) => id.lookup(db).module(db),
            | TypedDefId::TypeAliasId(id) => id.lookup(db).module,
            | TypedDefId::TypeCtorId(id) => id.lookup(db).module,
            | TypedDefId::CtorId(id) => id.parent.lookup(db).module(db),
            | TypedDefId::ClassId(id) => id.lookup(db).module,
            | TypedDefId::InstanceId(id) => id.lookup(db).module,
        }
    }
}

impl HasModule for DefWithBodyId {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId {
        match *self {
            | DefWithBodyId::FuncId(id) => id.lookup(db).module(db),
            | DefWithBodyId::StaticId(id) => id.lookup(db).module(db),
            | DefWithBodyId::ConstId(id) => id.lookup(db).module(db),
        }
    }
}

impl HasModule for AssocItemId {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId {
        match *self {
            | AssocItemId::FuncId(id) => id.lookup(db).module(db),
            | AssocItemId::StaticId(id) => id.lookup(db).module(db),
        }
    }
}

impl HasModule for TypeVarOwner {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId {
        match self {
            | TypeVarOwner::DefWithBodyId(id) => id.module(db),
            | TypeVarOwner::TypedDefId(id) => id.module(db),
        }
    }
}

impl TypeVarOwner {
    pub fn with_type_map<T>(self, db: &dyn DefDatabase, f: impl FnOnce(&TypeMap) -> T) -> T {
        match self {
            | TypeVarOwner::DefWithBodyId(def) => f(db.body(def).type_map()),
            | TypeVarOwner::TypedDefId(id) => match id {
                | TypedDefId::FuncId(id) => f(db.func_data(id).type_map()),
                | TypedDefId::StaticId(id) => f(db.static_data(id).type_map()),
                | TypedDefId::TypeAliasId(id) => f(db.type_alias_data(id).type_map()),
                | TypedDefId::TypeCtorId(id) => f(db.type_ctor_data(id).type_map()),
                | TypedDefId::CtorId(id) => f(db.type_ctor_data(id.parent).type_map()),
                | TypedDefId::ClassId(id) => f(db.class_data(id).type_map()),
                | TypedDefId::InstanceId(id) => f(db.instance_data(id).type_map()),
            },
        }
    }

    pub fn with_type_source_map<T>(self, db: &dyn DefDatabase, f: impl FnOnce(&TypeSourceMap) -> T) -> T {
        match self {
            | TypeVarOwner::DefWithBodyId(def) => f(&**db.body_source_map(def).1),
            | TypeVarOwner::TypedDefId(id) => match id {
                | TypedDefId::FuncId(id) => f(db.func_data(id).type_source_map()),
                | TypedDefId::StaticId(id) => f(db.static_data(id).type_source_map()),
                | TypedDefId::TypeAliasId(id) => f(db.type_alias_data(id).type_source_map()),
                | TypedDefId::TypeCtorId(id) => f(db.type_ctor_data(id).type_source_map()),
                | TypedDefId::CtorId(id) => f(db.type_ctor_data(id.parent).type_source_map()),
                | TypedDefId::ClassId(id) => f(db.class_data(id).type_source_map()),
                | TypedDefId::InstanceId(id) => f(db.instance_data(id).type_source_map()),
            },
        }
    }

    pub fn container(self, db: &dyn DefDatabase) -> ContainerId {
        match self {
            | TypeVarOwner::DefWithBodyId(id) => id.container(db),
            | TypeVarOwner::TypedDefId(id) => id.container(db),
        }
    }
}

impl DefWithBodyId {
    pub fn container(self, db: &dyn DefDatabase) -> ContainerId {
        match self {
            | DefWithBodyId::FuncId(id) => id.lookup(db).container,
            | DefWithBodyId::StaticId(id) => id.lookup(db).container,
            | DefWithBodyId::ConstId(id) => ContainerId::Module(id.lookup(db).module),
        }
    }

    pub fn has_body(self, db: &dyn DefDatabase) -> bool {
        match self {
            | DefWithBodyId::FuncId(id) => db.func_data(id).has_body,
            | DefWithBodyId::StaticId(id) => !db.static_data(id).is_foreign,
            | DefWithBodyId::ConstId(_) => true,
        }
    }
}

impl TypedDefId {
    pub fn container(self, db: &dyn DefDatabase) -> ContainerId {
        match self {
            | TypedDefId::FuncId(id) => id.lookup(db).container,
            | TypedDefId::StaticId(id) => id.lookup(db).container,
            | TypedDefId::TypeAliasId(id) => ContainerId::Module(id.lookup(db).module),
            | TypedDefId::TypeCtorId(id) => ContainerId::Module(id.lookup(db).module),
            | TypedDefId::CtorId(id) => ContainerId::Module(id.parent.lookup(db).module),
            | TypedDefId::ClassId(id) => ContainerId::Module(id.lookup(db).module),
            | TypedDefId::InstanceId(id) => ContainerId::Module(id.lookup(db).module),
        }
    }
}

impl<N: ItemTreeNode> HasModule for ItemLoc<N> {
    fn module(&self, _: &dyn DefDatabase) -> ModuleId {
        self.module
    }
}

impl<N: ItemTreeNode> HasModule for AssocItemLoc<N> {
    fn module(&self, db: &dyn DefDatabase) -> ModuleId {
        self.container.module(db)
    }
}

impl<N: ItemTreeNode> HasSource for AssocItemLoc<N> {
    type Value = N::Source;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        let tree = db.item_tree(self.id.file_id);
        let ast_id_map = db.ast_id_map(self.id.file_id);
        let root = db.parse(self.id.file_id).syntax_node();
        let node = &tree[self.id.value];

        InFile::new(self.id.file_id, ast_id_map.get(node.ast_id()).to_node(&root))
    }
}

impl<N: ItemTreeNode> HasSource for ItemLoc<N> {
    type Value = N::Source;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        let tree = db.item_tree(self.id.file_id);
        let ast_id_map = db.ast_id_map(self.id.file_id);
        let root = db.parse(self.id.file_id).syntax_node();
        let node = &tree[self.id.value];

        InFile::new(self.id.file_id, ast_id_map.get(node.ast_id()).to_node(&root))
    }
}

impl HasSource for TypedDefId {
    type Value = syntax::ast::Item;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        match self {
            | TypedDefId::FuncId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::StaticId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::TypeAliasId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::TypeCtorId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::CtorId(id) => id.parent.lookup(db).source(db).map(Into::into),
            | TypedDefId::ClassId(id) => id.lookup(db).source(db).map(Into::into),
            | TypedDefId::InstanceId(id) => id.lookup(db).source(db).map(Into::into),
        }
    }
}

impl HasSource for DefWithBodyId {
    type Value = syntax::ast::Item;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        match self {
            | DefWithBodyId::FuncId(id) => id.lookup(db).source(db).map(Into::into),
            | DefWithBodyId::StaticId(id) => id.lookup(db).source(db).map(Into::into),
            | DefWithBodyId::ConstId(id) => id.lookup(db).source(db).map(Into::into),
        }
    }
}

impl HasSource for TypeVarOwner {
    type Value = syntax::ast::Item;

    fn source(&self, db: &dyn DefDatabase) -> InFile<Self::Value> {
        match self {
            | TypeVarOwner::DefWithBodyId(def) => def.source(db),
            | TypeVarOwner::TypedDefId(def) => def.source(db),
        }
    }
}

impl<N: ItemTreeNode> Clone for ItemLoc<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: ItemTreeNode> Copy for ItemLoc<N> {
}

impl<N: ItemTreeNode> PartialEq for ItemLoc<N> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<N: ItemTreeNode> Eq for ItemLoc<N> {
}

impl<N: ItemTreeNode> Hash for ItemLoc<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<N: ItemTreeNode> Clone for AssocItemLoc<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: ItemTreeNode> Copy for AssocItemLoc<N> {
}

impl<N: ItemTreeNode> PartialEq for AssocItemLoc<N> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<N: ItemTreeNode> Eq for AssocItemLoc<N> {
}

impl<N: ItemTreeNode> Hash for AssocItemLoc<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
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

impl_from!(ModuleId, FixityId, FuncId, StaticId, ConstId, TypeAliasId, TypeCtorId, ClassId, InstanceId for AttrDefId);
impl_from!(ModuleId, FixityId, FuncId, StaticId, ConstId, TypeAliasId, TypeCtorId, CtorId, ClassId for ModuleDefId);
impl_from!(FuncId, StaticId, TypeAliasId, TypeCtorId, CtorId, ClassId, InstanceId for TypedDefId);
impl_from!(FuncId, StaticId, ConstId, CtorId for ValueTyDefId);
impl_from!(FuncId, StaticId, ConstId for DefWithBodyId);
impl_from!(DefWithBodyId, TypedDefId for TypeVarOwner);

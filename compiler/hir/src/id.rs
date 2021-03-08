use crate::arena::Idx;
use crate::db::DefDatabase;
use crate::def_map::ModuleData;
use crate::item_tree::*;
use base_db::libs::LibId;
use std::hash::{Hash, Hasher};

macro_rules! impl_intern {
    ($id:ident, $loc:ident, $intern:ident, $lookup:ident) => {
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
pub struct ForeignId(salsa::InternId);
pub type ForeignLoc = ItemLoc<Foreign>;
impl_intern!(ForeignId, ForeignLoc, intern_foreign, lookup_intern_foreign);

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
pub type ConstLoc = AssocItemLoc<Const>;
impl_intern!(ConstId, ConstLoc, intern_const, lookup_intern_const);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(salsa::InternId);
pub type TypeLoc = ItemLoc<Type>;
impl_intern!(TypeId, TypeLoc, intern_type, lookup_intern_type);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CtorId {
    pub parent: TypeId,
    pub local_id: LocalCtorId,
}

pub type LocalCtorId = Idx<()>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassId(salsa::InternId);
pub type ClassLoc = ItemLoc<Class>;
impl_intern!(ClassId, ClassLoc, intern_class, lookup_intern_class);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstanceId(salsa::InternId);
pub type InstanceLoc = ItemLoc<Instance>;
impl_intern!(InstanceId, InstanceLoc, intern_instance, lookup_intern_instance);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDefId {
    ModuleId(ModuleId),
    ForeignId(ForeignId),
    FixityId(FixityId),
    FuncId(FuncId),
    StaticId(StaticId),
    ConstId(ConstId),
    TypeId(TypeId),
    CtorId(CtorId),
    ClassId(ClassId),
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

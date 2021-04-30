pub mod db;
pub mod diagnostic;
mod from_id;
pub mod semantics;
pub mod source_analyzer;
mod source_to_def;

use base_db::input::FileId;
use base_db::libs::LibId;
pub use hir_def::expr::{Expr, Literal, Stmt};
use hir_def::id::*;
pub use hir_def::name::{AsName, Name};
pub use hir_def::pat::Pat;
use hir_def::pat::PatId;
use hir_ty::db::HirDatabase;

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

    pub fn root_module(self, db: &dyn HirDatabase) -> Module {
        let def_map = db.def_map(self.id);

        Module {
            id: def_map.module_id(def_map.root()),
        }
    }

    pub fn root_file(self, db: &dyn HirDatabase) -> FileId {
        db.libs()[self.id].root_file
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

    pub fn children(self, db: &dyn HirDatabase) -> Vec<Module> {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id]
            .children
            .iter()
            .map(|(_, mid)| Module {
                id: def_map.module_id(*mid),
            })
            .collect::<Vec<_>>()
    }

    pub fn parent(self, db: &dyn HirDatabase) -> Option<Module> {
        let def_map = db.def_map(self.id.lib);
        let parent_id = def_map[self.id.local_id].parent?;

        Some(Module {
            id: def_map.module_id(parent_id),
        })
    }

    pub fn path_to_root(self, db: &dyn HirDatabase) -> Vec<Module> {
        let mut res = vec![self];
        let mut curr = self;

        while let Some(next) = curr.parent(db) {
            res.push(next);
            curr = next;
        }

        res
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathResolution {
    Def(ModuleDef),
    Local(Local),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDef {
    Module(Module),
    Fixity(Fixity),
    Foreign(Foreign),
    Func(Func),
    Static(Static),
    Const(Const),
    Type(Type),
    Ctor(Ctor),
    Class(Class),
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.fixity_data(self.id).name.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Foreign {
    pub(crate) id: ForeignId,
}

impl Foreign {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type {
    pub(crate) id: TypeId,
}

impl Type {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ctor {
    pub(crate) id: CtorId,
}

impl Ctor {
    pub fn module(self, db: &dyn HirDatabase) -> Module {
        Module {
            id: self.id.parent.lookup(db.upcast()).module,
        }
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
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

impl ModuleDef {
    pub fn module(self, db: &dyn HirDatabase) -> Option<Module> {
        match self {
            | ModuleDef::Module(it) => it.parent(db),
            | ModuleDef::Fixity(it) => Some(it.module(db)),
            | ModuleDef::Foreign(it) => Some(it.module(db)),
            | ModuleDef::Func(it) => Some(it.module(db)),
            | ModuleDef::Static(it) => Some(it.module(db)),
            | ModuleDef::Const(it) => Some(it.module(db)),
            | ModuleDef::Ctor(it) => Some(it.module(db)),
            | ModuleDef::Type(it) => Some(it.module(db)),
            | ModuleDef::Class(it) => Some(it.module(db)),
        }
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        match self {
            | ModuleDef::Module(it) => it.name(db),
            | ModuleDef::Fixity(it) => it.name(db),
            | _ => unimplemented!(),
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

impl_from!(Fixity, Foreign, Func, Static, Const, Type, Ctor, Class for ModuleDef);

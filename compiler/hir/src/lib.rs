pub mod db;
pub mod diagnostic;
mod from_id;
pub mod semantics;
pub mod source_analyzer;
mod source_to_def;

use base_db::input::FileId;
use base_db::libs::LibId;
use hir_def::diagnostic::DiagnosticSink;
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

    pub fn declarations(self, db: &dyn HirDatabase) -> Vec<ModuleDef> {
        let def_map = db.def_map(self.id.lib);

        def_map[self.id.local_id]
            .scope
            .declarations()
            .map(ModuleDef::from)
            .collect()
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let def_map = db.def_map(self.id.lib);

        def_map.diagnostics().for_each(|d| {
            d.add_to(db.upcast(), self.id.local_id, sink);
        });

        for decl in self.declarations(db) {
            if let ModuleDef::Module(_) = decl {
                continue;
            }

            decl.diagnostics(db, sink);
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
            | ModuleDef::Module(it) => it.parent(db),
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

    pub fn typeck_test(self, db: &dyn HirDatabase) {
        use hir_ty::display::HirDisplay;

        match self {
            | ModuleDef::TypeAlias(it) => {
                let ty = db.type_for_alias(it.id);

                println!("{}: {}", it.name(db), ty.display(db));
            },
            | ModuleDef::TypeCtor(it) => {
                let ty = db.type_for_ctor(it.id);

                println!("{}: {}", it.name(db), ty.display(db));
            },
            | _ => {},
        }
    }

    pub fn diagnostics(self, db: &dyn HirDatabase, sink: &mut DiagnosticSink) {
        let id: ModuleDefId = match self {
            | ModuleDef::Module(it) => it.id.into(),
            | ModuleDef::Fixity(it) => it.id.into(),
            | ModuleDef::Func(it) => it.id.into(),
            | ModuleDef::Static(it) => it.id.into(),
            | ModuleDef::Const(it) => it.id.into(),
            | ModuleDef::TypeAlias(it) => it.id.into(),
            | ModuleDef::TypeCtor(it) => it.id.into(),
            | ModuleDef::Ctor(it) => CtorId {
                local_id: it.id,
                parent: it.parent.id,
            }
            .into(),
            | ModuleDef::Class(it) => it.id.into(),
        };

        let module = match self.module(db) {
            | Some(it) => it,
            | None => return,
        };

        // TODO: typecheck definition
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.fixity_data(self.id).name.clone()
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.func_data(self.id).name.clone()
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.static_data(self.id).name.clone()
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.const_data(self.id).name.clone()
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.type_alias_data(self.id).name.clone()
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.type_ctor_data(self.id).name.clone()
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

    pub fn type_ctor(self) -> TypeCtor {
        self.parent
    }

    pub fn lib(self, db: &dyn HirDatabase) -> Lib {
        self.module(db).lib()
    }

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.type_ctor_data(self.parent.id).ctors[self.id].name.clone()
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

    pub fn name(self, db: &dyn HirDatabase) -> Name {
        db.class_data(self.id).name.clone()
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
        self.id.lookup(db.upcast()).name.clone()
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

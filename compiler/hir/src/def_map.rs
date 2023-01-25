use std::sync::Arc;

use base_db::input::File;
use base_db::libs::LibId;
use rustc_hash::FxHashMap;

use crate::data::ModuleData;
use crate::id::{
    ContainerId, CtorId, FixityId, ImplId, ItemId, ModuleId, ModuleParentId, TraitId, TypeAliasId, TypeCtorId, ValueId,
};
use crate::item_tree::{Item, ItemTreeId, LocalItemTreeId};
use crate::name::Name;
use crate::path::Path;
use crate::{item_tree, Db};

#[salsa::tracked]
pub struct DefMap {
    #[return_ref]
    pub modules: FxHashMap<ModuleId, ModuleData>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ModuleScope {
    types: FxHashMap<Name, ItemId>,
    values: FxHashMap<Name, ItemId>,
    items: Vec<ItemId>,
    impls: Vec<ImplId>,
}

#[salsa::tracked]
pub fn query(db: &dyn Db, lib: LibId) -> DefMap {
    let mut modules = FxHashMap::default();

    for file in lib.source_root(db).iter(db) {
        let _ = lower_file(db, &mut modules, lib, file);
    }

    let modules = modules
        .into_iter()
        .map(|(k, v)| (k, ModuleData::new(db, k, v.scope)))
        .collect();

    DefMap::new(db, modules)
}

struct Ctx<'a> {
    db: &'a dyn Db,
    modules: &'a mut FxHashMap<ModuleId, RawModuleData>,
    module: Option<ModuleId>,
    file: File,
    item_tree: Arc<item_tree::ItemTree>,
}

struct RawModuleData {
    scope: ModuleScope,
}

fn lower_file(db: &dyn Db, modules: &mut FxHashMap<ModuleId, RawModuleData>, lib: LibId, file: File) -> Option<()> {
    let mut ctx = Ctx {
        db,
        modules,
        file,
        module: None,
        item_tree: item_tree::query(db, file),
    };

    let item_tree = item_tree::query(db, file);
    let source_file = base_db::parse(db, file);
    let module = source_file.module()?;
    let path = Path::from_ast(db, module.name()?);

    ctx.lower(lib.into(), path, item_tree.items());
    Some(())
}

impl Ctx<'_> {
    fn lower(&mut self, mut parent: ModuleParentId, path: Path, items: &[Item]) {
        for (i, name) in path.iter().enumerate() {
            let id = ModuleId::new(self.db, parent, name);
            self.init_module(parent, id, name);
            parent = id.into();

            if i == path.len() - 1 {
                let m = self.module.replace(id);

                for &item in items {
                    self.lower_item(id, item);
                }

                self.module = m;
                return;
            }
        }
    }

    fn init_module(&mut self, parent: ModuleParentId, id: ModuleId, name: Name) {
        if !self.modules.contains_key(&id) {
            self.modules.insert(id, RawModuleData {
                scope: ModuleScope::default(),
            });

            if let ModuleParentId::ModuleId(parent) = parent {
                self.modules
                    .get_mut(&parent)
                    .unwrap()
                    .scope
                    .types
                    .insert(name, ItemId::ModuleId(id));
            }
        }
    }

    fn scope(&mut self) -> &mut ModuleScope {
        &mut self.modules.get_mut(&self.module.unwrap()).unwrap().scope
    }

    fn lower_item(&mut self, module: ModuleId, item: Item) {
        match item {
            | Item::Module(it) => self.lower_module(module.into(), it),
            | Item::Fixity(it) => self.lower_fixity(module, it),
            | Item::Value(it) => self.lower_value(module.into(), it),
            | Item::TypeAlias(it) => self.lower_type_alias(module, it),
            | Item::TypeCtor(it) => self.lower_type_ctor(module, it),
            | Item::Trait(it) => self.lower_trait(module, it),
            | Item::Impl(it) => self.lower_impl(module, it),
        }
    }

    fn lower_module(&mut self, parent: ModuleParentId, it: LocalItemTreeId<item_tree::Module>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = ModuleId::new(self.db, parent, data.name);
        self.init_module(parent, id, data.name);
        let m = self.module.replace(id);

        for &item in data.items.iter() {
            self.lower_item(id, item);
        }

        self.module = m;
        self.scope().types.insert(data.name, ItemId::ModuleId(id));
    }

    fn lower_fixity(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Fixity>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = FixityId::new(self.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::FixityId(id);

        self.scope().items.push(item);

        if data.is_type {
            self.scope().types.insert(data.name, item);
        } else {
            self.scope().values.insert(data.name, item);
        }
    }

    fn lower_value(&mut self, container: ContainerId, it: LocalItemTreeId<item_tree::Value>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = ValueId::new(self.db, container, ItemTreeId::new(self.file, it));
        let item = ItemId::ValueId(id);

        self.scope().items.push(item);
        self.scope().values.insert(data.name, item);
    }

    fn lower_type_alias(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::TypeAlias>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TypeAliasId::new(self.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TypeAliasId(id);

        self.scope().items.push(item);
        self.scope().types.insert(data.name, item);
    }

    fn lower_type_ctor(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::TypeCtor>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TypeCtorId::new(self.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TypeCtorId(id);

        self.scope().items.push(item);
        self.scope().types.insert(data.name, item);

        for &local_id in data.ctors.iter() {
            let data = &item_tree[local_id];
            let id = CtorId::new(self.db, id, local_id);
            let item = ItemId::CtorId(id);

            self.scope().items.push(item);
            self.scope().values.insert(data.name, item);
        }
    }

    fn lower_trait(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Trait>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TraitId::new(self.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TraitId(id);

        self.scope().items.push(item);
        self.scope().types.insert(data.name, item);

        for &item in data.items.iter() {
            self.lower_value(id.into(), item);
        }
    }

    fn lower_impl(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Impl>) {
        let id = ImplId::new(self.db, module, ItemTreeId::new(self.file, it));

        self.scope().impls.push(id);
    }
}

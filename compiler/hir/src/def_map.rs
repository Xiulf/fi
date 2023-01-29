use std::sync::Arc;

use base_db::input::File;
use base_db::libs::LibId;
use diagnostics::Diagnostics;
use ra_ap_stdx::hash::{NoHashHashMap, NoHashHashSet};
use syntax::ast::AstNode;
use vfs::InFile;

use crate::ast_id::AstId;
use crate::data::ModuleData;
use crate::diagnostics::UnresolvedImport;
use crate::id::{
    ContainerId, CtorId, FixityId, ImplId, ItemId, ModuleId, ModuleParentId, TraitId, TypeAliasId, TypeCtorId, ValueId,
};
use crate::item_tree::{Item, ItemTreeId, LocalItemTreeId};
use crate::name::Name;
use crate::path::Path;
use crate::per_ns::PerNs;
use crate::{item_tree, Db};

#[salsa::tracked]
pub struct DefMap {
    #[return_ref]
    pub modules: NoHashHashMap<ModuleId, ModuleData>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ModuleScope {
    types: NoHashHashMap<Name, ItemId>,
    values: NoHashHashMap<Name, ItemId>,
    modules: NoHashHashMap<Name, ItemId>,
    exports: ModuleExports,
    items: Vec<ItemId>,
    impls: Vec<ImplId>,
}

#[derive(Default, Debug, PartialEq, Eq)]
struct ModuleExports {
    modules: NoHashHashSet<ModuleId>,
    names: NoHashHashSet<Name>,
    export_all: bool,
}

impl ModuleScope {
    pub fn get(&self, name: Name) -> PerNs<ItemId> {
        PerNs::new(
            self.types.get(&name).copied(),
            self.values.get(&name).copied(),
            self.modules.get(&name).copied(),
        )
    }
}

#[salsa::tracked]
pub fn query(db: &dyn Db, lib: LibId) -> DefMap {
    let mut ctx = Ctx {
        db,
        modules: NoHashHashMap::default(),
        root_modules: NoHashHashMap::default(),
        imports: Vec::new(),
    };

    for file in lib.source_root(db).iter(db) {
        let _ = ctx.lower_file(lib, file);
    }

    ctx.resolve_imports();

    let modules = ctx
        .modules
        .into_iter()
        .map(|(k, v)| (k, ModuleData::new(db, k, v.scope)))
        .collect();

    DefMap::new(db, modules)
}

struct Ctx<'a> {
    db: &'a dyn Db,
    modules: NoHashHashMap<ModuleId, RawModuleData>,
    root_modules: NoHashHashMap<Name, ModuleId>,
    imports: Vec<ImportDirective>,
}

struct ModuleCtx<'a, 'b> {
    base: &'b mut Ctx<'a>,
    module: Option<ModuleId>,
    file: File,
    item_tree: Arc<item_tree::ItemTree>,
}

#[derive(Default, Debug)]
struct RawModuleData {
    scope: ModuleScope,
    all_imports: AllImports,
}

#[derive(Default, Debug)]
struct AllImports {
    types: NoHashHashSet<Name>,
    values: NoHashHashSet<Name>,
    modules: NoHashHashSet<Name>,
}

#[derive(Debug)]
struct ImportDirective {
    resolve_in: ModuleId,
    module_id: ModuleId,
    import: Import,
}

#[derive(Debug)]
struct Import {
    src: ItemTreeId<item_tree::Import>,
    path: Path,
    rename: Option<Name>,
    hiding: Box<[Name]>,
    all: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ImportType {
    Named,
    All,
}

impl Ctx<'_> {
    fn lower_file(&mut self, lib: LibId, file: File) -> Option<()> {
        let mut ctx = ModuleCtx {
            item_tree: item_tree::query(self.db, file),
            module: None,
            base: self,
            file,
        };

        let item_tree = item_tree::query(ctx.base.db, file);
        let source_file = base_db::parse(ctx.base.db, file);
        let module = source_file.module()?;
        let path = Path::from_ast(ctx.base.db, module.name()?);

        ctx.lower(lib.into(), path, item_tree.items());
        Some(())
    }
}

impl ModuleCtx<'_, '_> {
    fn lower(&mut self, mut parent: ModuleParentId, path: Path, items: &[Item]) {
        for (i, name) in path.iter().enumerate() {
            let id = ModuleId::new(self.base.db, parent, name);
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
        if !self.base.modules.contains_key(&id) {
            self.base.modules.insert(id, RawModuleData::default());

            match parent {
                | ModuleParentId::LibId(_) => {
                    self.base.root_modules.insert(name, id);
                },
                | ModuleParentId::ModuleId(parent) => {
                    self.base
                        .modules
                        .get_mut(&parent)
                        .unwrap()
                        .scope
                        .modules
                        .insert(name, ItemId::ModuleId(id));
                },
            }
        }
    }

    fn data(&mut self) -> &mut RawModuleData {
        self.base.modules.get_mut(&self.module.unwrap()).unwrap()
    }

    fn lower_item(&mut self, module: ModuleId, item: Item) {
        match item {
            | Item::Module(it) => self.lower_module(module.into(), it),
            | Item::Import(it) => self.lower_import(module, it),
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
        let id = ModuleId::new(self.base.db, parent, data.name);
        self.init_module(parent, id, data.name);
        let m = self.module.replace(id);

        for &item in data.items.iter() {
            self.lower_item(id, item);
        }

        let ast_map = crate::ast_id::query(self.base.db, self.file);
        let root = base_db::parse(self.base.db, self.file);
        let module = ast_map.get(data.ast_id).to_node(root.syntax());
        self.lower_exports(module.exports());
        self.module = m;
    }

    fn lower_exports(&mut self, exports: Option<syntax::ast::Exports>) {
        if let None = exports {
            let scope = &mut self.base.modules.get_mut(&self.module.unwrap()).unwrap().scope;
            for item in &scope.items {
                let name = item.name(self.base.db).unwrap();
                scope.exports.names.insert(name);
            }
            return;
        }

        for export in exports.unwrap().iter() {}
    }

    fn lower_import(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Import>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let module_id = if let Some(qualify) = data.qualify {
            let module_id = self.base.modules[&module].scope.modules.get(&qualify);
            let module_id = module_id.map(|&id| match id {
                | ItemId::ModuleId(id) => id,
                | _ => unreachable!(),
            });

            match module_id {
                | Some(id) => id,
                | None => {
                    let module_id = ModuleId::new(self.base.db, module.into(), qualify);
                    self.init_module(module.into(), module_id, qualify);
                    // self.data().scope.modules.insert(qualify, module_id.into());
                    module_id
                },
            }
        } else {
            module
        };

        self.base.imports.push(ImportDirective {
            resolve_in: module,
            module_id,
            import: Import {
                src: InFile::new(self.file, it),
                rename: data.rename,
                hiding: data.hiding.clone().unwrap_or_default(),
                path: data.path.clone(),
                all: data.all,
            },
        });
    }

    fn lower_fixity(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Fixity>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = FixityId::new(self.base.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::FixityId(id);

        self.data().scope.items.push(item);

        if data.is_type {
            self.data().scope.types.insert(data.name, item);
        } else {
            self.data().scope.values.insert(data.name, item);
        }
    }

    fn lower_value(&mut self, container: ContainerId, it: LocalItemTreeId<item_tree::Value>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = ValueId::new(self.base.db, container, ItemTreeId::new(self.file, it));
        let item = ItemId::ValueId(id);

        self.data().scope.items.push(item);
        self.data().scope.values.insert(data.name, item);
    }

    fn lower_type_alias(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::TypeAlias>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TypeAliasId::new(self.base.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TypeAliasId(id);

        self.data().scope.items.push(item);
        self.data().scope.types.insert(data.name, item);
    }

    fn lower_type_ctor(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::TypeCtor>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TypeCtorId::new(self.base.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TypeCtorId(id);

        self.data().scope.items.push(item);
        self.data().scope.types.insert(data.name, item);

        for &local_id in data.ctors.iter() {
            let data = &item_tree[local_id];
            let id = CtorId::new(self.base.db, id, local_id);
            let item = ItemId::CtorId(id);

            self.data().scope.items.push(item);
            self.data().scope.values.insert(data.name, item);
        }
    }

    fn lower_trait(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Trait>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TraitId::new(self.base.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TraitId(id);

        self.data().scope.items.push(item);
        self.data().scope.types.insert(data.name, item);

        for &item in data.items.iter() {
            self.lower_value(id.into(), item);
        }
    }

    fn lower_impl(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Impl>) {
        let id = ImplId::new(self.base.db, module, ItemTreeId::new(self.file, it));

        self.data().scope.impls.push(id);
    }
}

impl Ctx<'_> {
    fn resolve_imports(&mut self) {
        let mut prev_unresolved = self.imports.len() + 1;

        while self.imports.len() < prev_unresolved {
            prev_unresolved = self.imports.len();
            let imports = std::mem::replace(&mut self.imports, Vec::new());

            for directive in imports {
                match self.resolve_import(directive.resolve_in, &directive.import) {
                    | Some(res) => self.record_import(directive, res),
                    | None => self.imports.push(directive),
                }
            }
        }

        for directive in self.imports.drain(..) {
            let item_tree = crate::item_tree::query(self.db, directive.import.src.file);
            let data = &item_tree[directive.import.src.value];

            Diagnostics::emit(self.db, UnresolvedImport {
                module: directive.module_id,
                ast: AstId(InFile::new(directive.import.src.file, data.ast_id)),
                index: data.index,
            });
        }
    }

    fn resolve_import(&self, mut module: ModuleId, import: &Import) -> Option<PerNs<ItemId>> {
        let mut segments = import.path.iter();
        let root_name = segments.next()?;
        let root = self.root_modules.get(&root_name).copied();
        let res = PerNs::new(None, None, root.map(ItemId::ModuleId));
        let mut res = res.or(self.modules[&module].scope.get(root_name));

        while let Some(segment) = segments.next() {
            module = match res.modules? {
                | ItemId::ModuleId(id) => id,
                | _ => unreachable!(),
            };

            res = self.modules[&module].scope.get(segment);
        }

        res.to_option()
    }

    fn record_import(&mut self, directive: ImportDirective, res: PerNs<ItemId>) {
        if !directive.import.all {
            let name = match directive.import.rename {
                | Some(name) => name,
                | None => directive.import.path.last(),
            };

            self.update(directive.module_id, &[(name, res)], ImportType::Named);
            return;
        }

        let module = match res.modules {
            | Some(ItemId::ModuleId(id)) => id,
            | _ => unreachable!(),
        };

        let resolutions = self.modules[&module]
            .scope
            .resolutions()
            .filter(|(n, _)| !directive.import.hiding.contains(n))
            .collect::<Vec<_>>();

        self.update(directive.module_id, &resolutions, ImportType::All);
    }

    fn update(&mut self, module: ModuleId, resolutions: &[(Name, PerNs<ItemId>)], import_type: ImportType) {
        let data = self.modules.get_mut(&module).unwrap();
        let mut changed = false;

        for &(name, res) in resolutions {
            changed |= data.scope.push_res(&mut data.all_imports, name, res, import_type);
        }

        if !changed {
            return;
        }
    }
}

impl ModuleScope {
    pub fn resolutions(&self) -> impl Iterator<Item = (Name, PerNs<ItemId>)> + '_ {
        let keys: NoHashHashSet<Name> = self
            .types
            .keys()
            .chain(self.values.keys())
            .chain(self.modules.keys())
            .copied()
            .collect();

        keys.into_iter().map(|name| (name, self.get(name)))
    }

    fn push_res(
        &mut self,
        all_imports: &mut AllImports,
        lookup: Name,
        def: PerNs<ItemId>,
        import_type: ImportType,
    ) -> bool {
        use std::collections::hash_map::Entry;
        let mut changed = false;
        macro_rules! check_changed {
            (
                $changed:ident,($this:ident / $def:ident).
                $field:ident,
                $all_imports:ident[$lookup:ident],
                $import_type:ident
            ) => {
                match ($this.$field.entry($lookup), $def.$field) {
                    | (Entry::Vacant(entry), Some(id)) => {
                        match $import_type {
                            | ImportType::All => $all_imports.$field.insert($lookup),
                            | ImportType::Named => $all_imports.$field.remove(&$lookup),
                        };
                        entry.insert(id);
                        $changed = true;
                    },
                    | (Entry::Occupied(mut entry), Some(id))
                        if $all_imports.$field.contains(&$lookup) && $import_type == ImportType::Named =>
                    {
                        $all_imports.$field.remove(&$lookup);
                        entry.insert(id);
                        $changed = true;
                    },
                    | _ => {},
                }
            };
        }

        check_changed!(changed, (self / def).types, all_imports[lookup], import_type);
        check_changed!(changed, (self / def).values, all_imports[lookup], import_type);
        check_changed!(changed, (self / def).modules, all_imports[lookup], import_type);
        changed
    }
}

impl DefMap {
    pub fn dump(self, db: &dyn Db) -> String {
        use ra_ap_stdx::format_to;
        let mut out = String::new();

        for (id, module) in self.modules(db) {
            format_to!(out, "{} - {id:?}:\n", id.name(db).display(db));
            format_to!(out, "  types:\n");

            for (name, item) in &module.scope(db).types {
                format_to!(out, "    - {}: {:?}\n", name.display(db), item);
            }

            format_to!(out, "  values:\n");

            for (name, item) in &module.scope(db).values {
                format_to!(out, "    - {}: {:?}\n", name.display(db), item);
            }

            format_to!(out, "  modules:\n");

            for (name, item) in &module.scope(db).modules {
                format_to!(out, "    - {}: {:?}\n", name.display(db), item);
            }
        }

        out
    }
}

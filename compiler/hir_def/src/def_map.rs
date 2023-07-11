use base_db::input::File;
use base_db::libs::LibId;
use diagnostics::Diagnostics;
use ra_ap_stdx::hash::{NoHashHashMap, NoHashHashSet};
use syntax::ast::{self, AstNode};
use syntax::ptr::AstPtr;
use triomphe::Arc;
use vfs::InFile;

use crate::ast_id::AstId;
use crate::data::ModuleData;
use crate::diagnostics::{UnknownExport, UnknownName, UnresolvedImport};
use crate::id::{
    ContainerId, CtorId, FieldId, FixityId, ImplId, ItemId, ModuleId, ModuleParentId, TraitId, TypeAliasId, TypeCtorId,
    ValueId,
};
use crate::item_tree::{Item, ItemTreeId, LocalItemTreeId};
use crate::name::{AsName, Name};
use crate::path::Path;
use crate::per_ns::{Namespace, PerNs};
use crate::{item_tree, Db};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefMap {
    lib: LibId,
    root_modules: NoHashHashMap<Name, ModuleId>,
    modules: NoHashHashMap<ModuleId, ModuleData>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ModuleScope {
    types: NoHashHashMap<Name, ItemId>,
    values: NoHashHashMap<Name, ItemId>,
    modules: NoHashHashMap<Name, ItemId>,
    exports: ModuleExports,
    items: Vec<(Name, ItemId)>,
    impls: Vec<ImplId>,
}

#[derive(Default, Debug, PartialEq, Eq)]
struct ModuleExports {
    modules: NoHashHashSet<ModuleId>,
    names: NoHashHashMap<Name, Option<ExportSrc>>,
    export_all: bool,
}

type ExportSrc = InFile<AstPtr<ast::NameRef>>;

impl DefMap {
    pub fn modules(&self) -> impl Iterator<Item = (ModuleId, ModuleData)> + '_ {
        self.modules.iter().map(|(&k, &v)| (k, v))
    }

    pub fn root_modules(&self) -> &NoHashHashMap<Name, ModuleId> {
        &self.root_modules
    }

    pub fn resolve_path(&self, db: &dyn Db, path: &Path, current_module: ModuleId) -> Option<PerNs<ItemId>> {
        let mut module = current_module;
        let mut segments = path.iter();
        let root_name = segments.next()?;
        let root = self.root_modules.get(&root_name).copied();
        let res = PerNs::new(None, None, root.map(ItemId::ModuleId));
        let mut res = res.or(self[module].scope(db).get(root_name));

        if let Some(ItemId::ModuleId(m)) = res.modules && m.lib(db) != self.lib && !segments.is_empty() {
            let def_map = query(db, m.lib(db));
            return def_map.resolve_path(db, &segments.collect(), m);
        }

        while let Some(segment) = segments.next() {
            module = match res.modules? {
                | ItemId::ModuleId(id) => id,
                | _ => unreachable!(),
            };

            res = self[module].scope(db).get(segment);
        }

        res.to_option()
    }
}

impl std::ops::Index<ModuleId> for DefMap {
    type Output = ModuleData;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self.modules[&index]
    }
}

impl ModuleScope {
    pub fn get(&self, name: Name) -> PerNs<ItemId> {
        PerNs::new(
            self.types.get(&name).copied(),
            self.values.get(&name).copied(),
            self.modules.get(&name).copied(),
        )
    }

    pub fn items(&self) -> impl Iterator<Item = ItemId> + '_ {
        self.items.iter().map(|(_, i)| *i)
    }

    pub fn impls(&self) -> impl Iterator<Item = ImplId> + '_ {
        self.impls.iter().copied()
    }

    pub fn is_exported(&self, name: Name) -> bool {
        self.exports.is_exported(name)
    }
}

#[salsa::tracked(return_ref)]
pub fn query(db: &dyn Db, lib: LibId) -> Arc<DefMap> {
    let mut external_modules = NoHashHashMap::default();

    for &dep in lib.deps(db) {
        let def_map = query(db, dep);
        external_modules.extend(def_map.root_modules());
    }

    let mut ctx = Ctx {
        db,
        lib,
        modules: NoHashHashMap::default(),
        root_modules: NoHashHashMap::default(),
        external_modules,
        imports: Vec::new(),
    };

    for file in lib.source_root(db).iter(db) {
        let _ = ctx.lower_file(lib, file);
    }

    ctx.resolve_imports();
    ctx.verify_exports();

    let root_modules = ctx.root_modules;
    let modules = ctx
        .modules
        .into_iter()
        .filter(|(id, _)| id.lib(db) == lib)
        .map(|(k, v)| (k, ModuleData::new(db, k, v.scope)))
        .collect();

    Arc::new(DefMap {
        lib,
        root_modules,
        modules,
    })
}

struct Ctx<'a> {
    db: &'a dyn Db,
    lib: LibId,
    modules: NoHashHashMap<ModuleId, RawModuleData>,
    root_modules: NoHashHashMap<Name, ModuleId>,
    external_modules: NoHashHashMap<Name, ModuleId>,
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
    imported_by: NoHashHashSet<ModuleId>,
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
        tracing::trace!("lower_module {}", path.display(ctx.base.db));

        ctx.lower(lib.into(), path, &module, item_tree.items());
        Some(())
    }

    fn data_of(&mut self, m: ModuleId) -> &mut RawModuleData {
        self.modules.entry(m).or_default()
    }
}

impl ModuleCtx<'_, '_> {
    fn lower(&mut self, mut parent: ModuleParentId, path: Path, module: &ast::ItemModule, items: &[Item]) {
        for (i, name) in path.iter().enumerate() {
            let id = ModuleId::new(self.base.db, parent, name);
            self.init_module(parent, id, name, true);
            parent = id.into();

            if i == path.len() - 1 {
                let m = self.module.replace(id);

                for &item in items {
                    self.lower_item(id, item);
                }

                self.lower_exports(module.exports());
                self.module = m;
                return;
            }
        }
    }

    fn init_module(&mut self, parent: ModuleParentId, id: ModuleId, name: Name, export: bool) {
        if !self.base.modules.contains_key(&id) {
            self.base.modules.insert(id, RawModuleData::default());

            match parent {
                | ModuleParentId::LibId(_) => {
                    self.base.root_modules.insert(name, id);
                },
                | ModuleParentId::ModuleId(parent) => {
                    let data = self.base.modules.get_mut(&parent).unwrap();
                    data.scope.modules.insert(name, ItemId::ModuleId(id));

                    if export {
                        data.scope.exports.names.insert(name, None);
                    }
                },
            }
        }
    }

    fn data(&mut self) -> &mut RawModuleData {
        self.base.data_of(self.module.unwrap())
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
        self.init_module(parent, id, data.name, false);
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
        let scope = &mut self.base.modules.get_mut(&self.module.unwrap()).unwrap().scope;

        if let None = exports {
            for &(name, _) in &scope.items {
                scope.exports.names.insert(name, None);
            }
            return;
        }

        for export in exports.unwrap().iter() {
            match export {
                | syntax::ast::Export::Name(ex) => {
                    if let Some(name) = ex.name_ref() {
                        let src = InFile::new(self.file, AstPtr::new(&name));
                        scope.exports.names.insert(name.as_name(self.base.db), Some(src));
                    }
                },
                | syntax::ast::Export::Module(ex) => {
                    if let Some(name_ref) = ex.name_ref() {
                        let name = name_ref.as_name(self.base.db);
                        if name == self.module.unwrap().name(self.base.db) {
                            for &(name, _) in &scope.items {
                                scope.exports.names.insert(name, None);
                            }
                        } else {
                            let module = match scope.modules.get(&name) {
                                | Some(&ItemId::ModuleId(m)) => m,
                                | Some(_) => unreachable!(),
                                | None => {
                                    Diagnostics::emit(self.base.db, UnknownName {
                                        file: self.file,
                                        ast: AstPtr::new(&name_ref),
                                        ns: Namespace::Modules,
                                        name,
                                    });
                                    continue;
                                },
                            };

                            scope.exports.modules.insert(module);
                        }
                    }
                },
            }
        }
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
                    self.init_module(module.into(), module_id, qualify, false);
                    self.base.modules.get_mut(&module_id).unwrap().scope.exports.export_all = true;
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

        self.data().scope.items.push((data.name, item));

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

        self.data().scope.items.push((data.name, item));
        self.data().scope.values.insert(data.name, item);
    }

    fn lower_type_alias(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::TypeAlias>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TypeAliasId::new(self.base.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TypeAliasId(id);

        self.data().scope.items.push((data.name, item));
        self.data().scope.types.insert(data.name, item);
    }

    fn lower_type_ctor(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::TypeCtor>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TypeCtorId::new(self.base.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TypeCtorId(id);
        let is_newtype = data.ctors.len() == 1;

        self.data().scope.items.push((data.name, item));
        self.data().scope.types.insert(data.name, item);

        for &local_id in data.ctors.iter() {
            let data = &item_tree[local_id];
            let id = CtorId::new(self.base.db, id, local_id);
            let item = ItemId::CtorId(id);

            self.data().scope.items.push((data.name, item));
            self.data().scope.values.insert(data.name, item);

            if let Some(fields) = &data.fields && is_newtype {
                for &local_id in fields.iter() {
                    let data = &item_tree[local_id];
                    let id = FieldId::new(self.base.db, id, local_id);
                    let item = ItemId::FieldId(id);

                    self.data().scope.items.push((data.name, item));
                    self.data().scope.values.insert(data.name, item);
                }
            }
        }
    }

    fn lower_trait(&mut self, module: ModuleId, it: LocalItemTreeId<item_tree::Trait>) {
        let item_tree = self.item_tree.clone();
        let data = &item_tree[it];
        let id = TraitId::new(self.base.db, module, ItemTreeId::new(self.file, it));
        let item = ItemId::TraitId(id);

        self.data().scope.items.push((data.name, item));
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
    fn verify_exports(&self) {
        for (_, module) in &self.modules {
            for (&name, src) in &module.scope.exports.names {
                if module.scope.get(name).is_none() {
                    let src = src.as_ref().unwrap();
                    Diagnostics::emit(self.db, UnknownExport {
                        file: src.file,
                        ast: src.value,
                        name,
                    });
                }
            }
        }
    }

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
                ast: AstId(InFile::new(directive.import.src.file, data.ast_id)),
                index: data.index,
            });
        }
    }

    fn resolve_import(&self, mut module: ModuleId, import: &Import) -> Option<PerNs<ItemId>> {
        let mut segments = import.path.iter();
        let root_name = segments.next()?;
        let root = self.root_modules.get(&root_name).copied();
        let root = root.or_else(|| self.external_modules.get(&root_name).copied());
        let res = PerNs::new(None, None, root.map(ItemId::ModuleId));
        let mut res = res.or(self.modules[&module].scope.get(root_name));

        if let Some(ItemId::ModuleId(m)) = res.modules && m.lib(self.db) != self.lib && !segments.is_empty() {
            let def_map = query(self.db, m.lib(self.db));
            return def_map.resolve_path(self.db, &segments.collect(), m);
        }

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

        fn exports<'a>(ctx: &'a Ctx, m: ModuleId) -> Box<dyn Iterator<Item = (Name, PerNs<ItemId>)> + 'a> {
            let output = |scope: &'a ModuleScope| {
                Box::new(
                    scope
                        .resolutions()
                        .filter(move |(n, _)| scope.exports.is_exported(*n))
                        .chain(scope.exports.modules.iter().flat_map(|&m2| exports(ctx, m2))),
                )
            };

            if m.lib(ctx.db) != ctx.lib {
                let def_map = query(ctx.db, m.lib(ctx.db));
                let scope = def_map[m].scope(ctx.db);
                output(scope)
            } else {
                output(&ctx.modules[&m].scope)
            }
        }

        let resolutions = exports(self, module)
            .filter(|(n, _)| !directive.import.hiding.contains(n))
            .collect::<Vec<_>>();

        self.update(directive.module_id, &resolutions, ImportType::All);
        self.add_reexport(module, directive.module_id);
    }

    fn add_reexport(&mut self, from: ModuleId, to: ModuleId) {
        self.data_of(from).imported_by.insert(to);

        for module in self.modules[&from].scope.exports.modules.clone() {
            self.add_reexport(module, to);
        }
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

        let resolutions = resolutions
            .iter()
            .copied()
            .filter(|(n, _)| data.scope.exports.is_exported(*n))
            .collect::<Vec<_>>();

        for id in data.imported_by.clone() {
            self.update(id, &resolutions, ImportType::All);
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

impl ModuleExports {
    pub fn is_exported(&self, name: Name) -> bool {
        if self.export_all {
            return true;
        }

        self.names.contains_key(&name)
    }
}

impl DefMap {
    pub fn debug(&self, db: &dyn Db) -> String {
        use ra_ap_stdx::format_to;
        let mut out = String::new();

        for (id, module) in self.modules() {
            let scope = module.scope(db);
            format_to!(out, "{} - {id:?}:\n", id.name(db).display(db));
            format_to!(out, "  exports: ");

            if scope.exports.export_all {
                format_to!(out, "*");
            } else {
                for (name, _) in &scope.exports.names {
                    format_to!(out, "{}, ", name.display(db));
                }

                for id in &scope.exports.modules {
                    format_to!(out, "module {}, ", id.name(db).display(db));
                }
            }

            format_to!(out, "\n  types:\n");

            for (name, item) in &scope.types {
                format_to!(out, "    - {}: {:?}\n", name.display(db), item);
            }

            format_to!(out, "  values:\n");

            for (name, item) in &scope.values {
                format_to!(out, "    - {}: {:?}\n", name.display(db), item);
            }

            format_to!(out, "  modules:\n");

            for (name, item) in &scope.modules {
                format_to!(out, "    - {}: {:?}\n", name.display(db), item);
            }
        }

        out
    }
}

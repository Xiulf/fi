use crate::db::DefDatabase;
use crate::id::{ClassId, HasModule, InstanceId, LocalModuleId, Lookup, ModuleDefId, ModuleId};
use crate::name::Name;
use crate::per_ns::PerNs;
use base_db::libs::LibId;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::fmt::Write as _;

#[derive(Clone, Copy)]
pub enum ImportType {
    All,
    Named,
}

#[derive(Default, Debug)]
pub struct PerNsAllImports {
    types: FxHashSet<(LocalModuleId, Name)>,
    values: FxHashSet<(LocalModuleId, Name)>,
    modules: FxHashSet<(LocalModuleId, Name)>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ItemScope {
    types: FxHashMap<Name, ModuleDefId>,
    values: FxHashMap<Name, ModuleDefId>,
    modules: FxHashMap<Name, ModuleDefId>,
    unresolved: FxHashSet<Name>,
    defs: Vec<ModuleDefId>,
    instances: Vec<InstanceId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemInNs {
    Types(ModuleDefId),
    Values(ModuleDefId),
    Modules(ModuleDefId),
}

impl ItemScope {
    pub fn entries<'a>(&'a self) -> impl Iterator<Item = (&'a Name, PerNs)> + 'a {
        let keys: FxHashSet<_> = self
            .types
            .keys()
            .chain(self.values.keys())
            .chain(self.modules.keys())
            .chain(self.unresolved.iter())
            .collect();

        keys.into_iter().map(move |name| (name, self.get(name)))
    }

    pub fn declarations(&self) -> impl Iterator<Item = ModuleDefId> + '_ {
        self.defs.iter().copied()
    }

    pub fn instances(&self) -> impl Iterator<Item = InstanceId> + ExactSizeIterator + '_ {
        self.instances.iter().copied()
    }

    pub fn values(&self) -> impl Iterator<Item = ModuleDefId> + ExactSizeIterator + '_ {
        self.values.values().copied()
    }

    pub fn get(&self, name: &Name) -> PerNs {
        PerNs {
            types: self.types.get(name).copied(),
            values: self.values.get(name).copied(),
            modules: self.modules.get(name).copied(),
        }
    }

    pub fn name_of(&self, item: ItemInNs) -> Option<&Name> {
        for (name, per_ns) in self.entries() {
            if item.match_with(per_ns) {
                return Some(name);
            }
        }

        None
    }

    pub fn classes<'a>(&'a self) -> impl Iterator<Item = ClassId> + 'a {
        self.types.values().filter_map(|def| match def {
            | ModuleDefId::ClassId(t) => Some(*t),
            | _ => None,
        })
    }

    pub fn define_def(&mut self, def: ModuleDefId) {
        self.defs.push(def);
    }

    pub fn define_instance(&mut self, instance: InstanceId) {
        self.instances.push(instance);
    }

    pub fn push_res(&mut self, name: Name, def: PerNs) -> bool {
        let mut changed = false;

        if let Some(types) = def.types {
            self.types.entry(name.clone()).or_insert_with(|| {
                changed = true;
                types
            });
        }

        if let Some(values) = def.values {
            self.values.entry(name.clone()).or_insert_with(|| {
                changed = true;
                values
            });
        }

        if let Some(modules) = def.modules {
            self.modules.entry(name.clone()).or_insert_with(|| {
                changed = true;
                modules
            });
        }

        if def.is_none() {
            if self.unresolved.insert(name) {
                changed = true;
            }
        }

        changed
    }

    pub fn push_res_with_import(
        &mut self,
        all_imports: &mut PerNsAllImports,
        lookup: (LocalModuleId, Name),
        def: PerNs,
        def_import_type: ImportType,
    ) -> bool {
        let mut changed = false;

        macro_rules! check_changed {
            (
                $changed:ident,($this:ident / $def:ident).
                $field:ident,
                $all_imports:ident[$lookup:ident],
                $def_import_type:ident
            ) => {{
                let existing = $this.$field.entry($lookup.1.clone());

                match (existing, $def.$field) {
                    | (Entry::Vacant(entry), Some(_)) => {
                        match $def_import_type {
                            | ImportType::All => {
                                $all_imports.$field.insert($lookup.clone());
                            },
                            | ImportType::Named => {
                                $all_imports.$field.remove(&lookup);
                            },
                        }

                        if let Some(f_id) = $def.$field {
                            entry.insert(f_id);
                        }

                        $changed = true;
                    },
                    | (Entry::Occupied(mut entry), Some(_))
                        if $all_imports.$field.contains(&$lookup) && matches!($def_import_type, ImportType::Named) =>
                    {
                        $all_imports.$field.remove(&$lookup);

                        if let Some(f_id) = $def.$field {
                            entry.insert(f_id);
                        }

                        $changed = true;
                    }
                    | _ => {},
                }
            }};
        }

        check_changed!(changed, (self / def).types, all_imports[lookup], def_import_type);
        check_changed!(changed, (self / def).values, all_imports[lookup], def_import_type);
        check_changed!(changed, (self / def).modules, all_imports[lookup], def_import_type);

        if def.is_none() {
            if self.unresolved.insert(lookup.1) {
                changed = true;
            }
        }

        changed
    }

    pub fn resolutions<'a>(&'a self) -> impl Iterator<Item = (Name, PerNs)> + 'a {
        self.entries().map(|(name, res)| (name.clone(), res))
    }

    pub fn dump(&self, buf: &mut String) {
        let mut entries: Vec<_> = self.resolutions().collect();

        entries.sort_by_key(|(name, _)| name.clone());

        for (name, def) in entries {
            write!(buf, "{}:", name).unwrap();

            if def.types.is_some() {
                buf.push_str(" t");
            }

            if def.values.is_some() {
                buf.push_str(" v");
            }

            if def.modules.is_some() {
                buf.push_str(" m");
            }

            if def.is_none() {
                buf.push_str(" _");
            }

            buf.push('\n');
        }
    }
}

impl ItemInNs {
    fn match_with(self, per_ns: PerNs) -> bool {
        match self {
            | ItemInNs::Types(def) => per_ns.types.filter(|other_def| *other_def == def).is_some(),
            | ItemInNs::Values(def) => per_ns.values.filter(|other_def| *other_def == def).is_some(),
            | ItemInNs::Modules(def) => per_ns.modules.filter(|other_def| *other_def == def).is_some(),
        }
    }

    pub fn as_module_def_id(self) -> ModuleDefId {
        match self {
            | ItemInNs::Types(id) => id,
            | ItemInNs::Values(id) => id,
            | ItemInNs::Modules(id) => id,
        }
    }

    pub fn lib(&self, db: &dyn DefDatabase) -> LibId {
        match self.as_module_def_id() {
            | ModuleDefId::ModuleId(id) => id.lib,
            | ModuleDefId::FixityId(id) => id.lookup(db).module(db).lib,
            | ModuleDefId::FuncId(id) => id.lookup(db).module(db).lib,
            | ModuleDefId::StaticId(id) => id.lookup(db).module(db).lib,
            | ModuleDefId::ConstId(id) => id.lookup(db).module(db).lib,
            | ModuleDefId::TypeId(id) => id.lookup(db).module(db).lib,
            | ModuleDefId::CtorId(id) => id.parent.lookup(db).module(db).lib,
            | ModuleDefId::ClassId(id) => id.lookup(db).module(db).lib,
        }
    }
}

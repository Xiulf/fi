use crate::db::DefDatabase;
use crate::id::{ClassId, HasModule, LocalModuleId, Lookup, MemberId, ModuleDefId, ModuleId};
use crate::name::Name;
use crate::per_ns::PerNs;
use crate::visibility::Visibility;
use base_db::libs::LibId;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::io;

#[derive(Clone, Copy)]
pub enum ImportType {
    Glob,
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
    types: FxHashMap<Name, (ModuleDefId, Visibility)>,
    values: FxHashMap<Name, (ModuleDefId, Visibility)>,
    modules: FxHashMap<Name, (ModuleDefId, Visibility)>,
    reexports: FxHashMap<Name, PerNs<ModuleDefId>>,
    unresolved: FxHashSet<Name>,
    defs: Vec<ModuleDefId>,
    members: Vec<MemberId>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ItemExports {
    pub(crate) export_all: bool,
    names: FxHashMap<Name, ExportNs>,
    groups: FxHashMap<Name, ExportGroup>,
    modules: FxHashSet<ModuleId>,
}

#[derive(Debug, Eq, Hash)]
pub enum ExportNs {
    Values,
    Types,
    Any,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportGroup {
    All,
    Named(FxHashSet<Name>),
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

    pub fn members(&self) -> impl Iterator<Item = MemberId> + ExactSizeIterator + '_ {
        self.members.iter().copied()
    }

    pub fn values(&self) -> impl Iterator<Item = (ModuleDefId, Visibility)> + ExactSizeIterator + '_ {
        self.values.values().copied()
    }

    pub fn get(&self, name: &Name) -> PerNs {
        PerNs {
            types: self.types.get(name).copied(),
            values: self.values.get(name).copied(),
            modules: self.modules.get(name).copied(),
        }
    }

    pub fn get_reexport(&self, name: &Name) -> PerNs {
        self.reexports
            .get(name)
            .copied()
            .map(|id| id.map(|id| (id, Visibility::Public)))
            .unwrap_or_else(PerNs::none)
    }

    pub fn name_of(&self, item: ItemInNs) -> Option<&Name> {
        for (name, per_ns) in self.entries() {
            if item.match_with(per_ns) {
                return Some(name);
            }
        }

        None
    }

    pub fn classes(&self) -> impl Iterator<Item = ClassId> + '_ {
        self.types.values().filter_map(|(def, _)| match def {
            | ModuleDefId::ClassId(t) => Some(*t),
            | _ => None,
        })
    }

    pub fn define_def(&mut self, def: ModuleDefId) {
        self.defs.push(def);
    }

    pub fn define_member(&mut self, member: MemberId) {
        self.members.push(member);
    }

    pub fn push_reexport(&mut self, name: Name, def: PerNs<ModuleDefId>) {
        match self.reexports.entry(name) {
            | Entry::Vacant(e) => {
                e.insert(def);
            },
            | Entry::Occupied(mut e) => {
                e.insert(e.get().or(def));
            },
        }
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
                    | (Entry::Vacant(entry), Some(f_id)) => {
                        match $def_import_type {
                            | ImportType::Glob => {
                                $all_imports.$field.insert($lookup.clone());
                            },
                            | ImportType::Named => {
                                $all_imports.$field.remove(&lookup);
                            },
                        }

                        entry.insert(f_id);
                        $changed = true;
                    },
                    | (Entry::Occupied(mut entry), Some(f_id))
                        if $all_imports.$field.contains(&$lookup) && matches!($def_import_type, ImportType::Named) =>
                    {
                        $all_imports.$field.remove(&$lookup);

                        entry.insert(f_id);
                        $changed = true;
                    },
                    | _ => {},
                }
            }};
        }

        check_changed!(changed, (self / def).types, all_imports[lookup], def_import_type);
        check_changed!(changed, (self / def).values, all_imports[lookup], def_import_type);
        check_changed!(changed, (self / def).modules, all_imports[lookup], def_import_type);

        if def.is_none() && self.unresolved.insert(lookup.1) {
            changed = true;
        }

        changed
    }

    pub fn resolutions<'a>(&'a self) -> impl Iterator<Item = (Name, PerNs)> + 'a {
        self.entries()
            .chain(
                self.reexports
                    .iter()
                    .map(|(name, def)| (name, def.map(|id| (id, Visibility::Public)))),
            )
            .map(|(name, res)| (name.clone(), res))
    }

    pub fn dump(&self, writer: &mut dyn io::Write) -> io::Result<()> {
        let mut entries: Vec<_> = self.resolutions().collect();

        entries.sort_by_key(|(name, _)| name.clone());

        for (name, def) in entries {
            write!(writer, "{}:", name)?;

            if let Some((_, vis)) = def.types {
                write!(writer, " {} t", vis)?;
            }

            if let Some((_, vis)) = def.values {
                write!(writer, " {} v", vis)?;
            }

            if let Some((_, vis)) = def.modules {
                write!(writer, " {} m", vis)?;
            }

            if def.is_none() {
                write!(writer, " _")?;
            }

            write!(writer, "\n")?;
        }

        Ok(())
    }
}

impl ItemExports {
    pub fn add_name(&mut self, name: Name, ns: ExportNs) {
        self.names.insert(name, ns);
    }

    pub fn add_module(&mut self, module: ModuleId) {
        self.modules.insert(module);
    }

    pub fn add_group(&mut self, name: Name, group: ExportGroup) {
        self.names.insert(name.clone(), ExportNs::Types);
        self.groups.insert(name, group);
    }

    pub fn get_group(&self, name: &Name) -> Option<&ExportGroup> {
        self.groups.get(name)
    }

    pub fn resolve_visibility(&self, name: &Name, ns: ExportNs, in_module: ModuleId) -> Visibility {
        if self.export_all {
            Visibility::Public
        } else if Some(&ns) == self.names.get(name) {
            Visibility::Public
        } else {
            Visibility::Module(in_module)
        }
    }

    pub fn resolve_module_visibility(&self, id: ModuleId, in_module: ModuleId) -> Visibility {
        if self.modules.contains(&id) {
            Visibility::Public
        } else {
            Visibility::Module(in_module)
        }
    }

    pub fn modules(&self) -> impl Iterator<Item = ModuleId> + '_ {
        self.modules.iter().copied()
    }
}

impl PartialEq for ExportNs {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            | (Self::Values, Self::Values) => true,
            | (Self::Types, Self::Types) => true,
            | (Self::Any, _) => true,
            | (_, Self::Any) => true,
            | (_, _) => false,
        }
    }
}

impl ItemInNs {
    fn match_with(self, per_ns: PerNs) -> bool {
        match self {
            | ItemInNs::Types(def) => per_ns.types.filter(|other_def| other_def.0 == def).is_some(),
            | ItemInNs::Values(def) => per_ns.values.filter(|other_def| other_def.0 == def).is_some(),
            | ItemInNs::Modules(def) => per_ns.modules.filter(|other_def| other_def.0 == def).is_some(),
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
            | ModuleDefId::TypeAliasId(id) => id.lookup(db).module(db).lib,
            | ModuleDefId::TypeCtorId(id) => id.lookup(db).module(db).lib,
            | ModuleDefId::CtorId(id) => id.parent.lookup(db).module(db).lib,
            | ModuleDefId::ClassId(id) => id.lookup(db).module(db).lib,
        }
    }
}

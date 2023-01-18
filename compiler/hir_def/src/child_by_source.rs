use base_db::input::FileId;

use crate::db::DefDatabase;
use crate::dyn_map::DynMap;
use crate::id::{AssocItemId, ClassId, CtorId, Lookup, MemberId, ModuleDefId, ModuleId, TypeCtorId};
use crate::item_scope::ItemScope;
use crate::keys;
use crate::source::{HasChildSource, HasSource};

pub trait ChildBySource {
    fn child_by_source(&self, db: &dyn DefDatabase, file_id: FileId) -> DynMap {
        let mut res = DynMap::default();

        self.child_by_source_to(db, &mut res, file_id);
        res
    }

    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId);
}

impl ChildBySource for ModuleId {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId) {
        let def_map = db.def_map(self.lib);
        let module_data = &def_map[self.local_id];

        module_data.scope.child_by_source_to(db, map, file_id);
    }
}

impl ChildBySource for ItemScope {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId) {
        self.declarations()
            .for_each(|item| add_module_def(db, file_id, map, item));

        self.members().for_each(|member| add_member(db, file_id, map, member));

        fn add_module_def(db: &dyn DefDatabase, file_id: FileId, map: &mut DynMap, item: ModuleDefId) {
            match item {
                | ModuleDefId::FuncId(func) => {
                    let loc = func.lookup(db);
                    if loc.id.file_id == file_id {
                        map[keys::FUNC].insert(loc.source(db).value, func);
                    }
                },
                | ModuleDefId::ConstId(const_) => {
                    let loc = const_.lookup(db);
                    if loc.id.file_id == file_id {
                        map[keys::CONST].insert(loc.source(db).value, const_);
                    }
                },
                | ModuleDefId::StaticId(static_) => {
                    let loc = static_.lookup(db);
                    if loc.id.file_id == file_id {
                        map[keys::STATIC].insert(loc.source(db).value, static_);
                    }
                },
                | ModuleDefId::ClassId(class) => {
                    let loc = class.lookup(db);
                    if loc.id.file_id == file_id {
                        map[keys::CLASS].insert(loc.source(db).value, class);
                    }
                },
                | _ => {},
            }
        }

        fn add_member(db: &dyn DefDatabase, file_id: FileId, map: &mut DynMap, inst: MemberId) {
            let loc = inst.lookup(db);
            if loc.id.file_id == file_id {
                map[keys::MEMBER].insert(loc.source(db).value, inst);
            }
        }
    }
}

impl ChildBySource for ClassId {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId) {
        let data = db.class_data(*self);

        for &(_, item) in data.items.iter() {
            add_assoc_item(db, map, file_id, item);
        }
    }
}

impl ChildBySource for MemberId {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId) {
        let data = db.member_data(*self);

        for &(_, item) in data.items.iter() {
            add_assoc_item(db, map, file_id, item);
        }
    }
}

fn add_assoc_item(db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId, item: AssocItemId) {
    match item {
        | AssocItemId::FuncId(func) => {
            let loc = func.lookup(db);
            if loc.id.file_id == file_id {
                map[keys::FUNC].insert(loc.source(db).value, func);
            }
        },
        | AssocItemId::StaticId(static_) => {
            let loc = static_.lookup(db);
            if loc.id.file_id == file_id {
                map[keys::STATIC].insert(loc.source(db).value, static_);
            }
        },
    }
}

impl ChildBySource for TypeCtorId {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, _: FileId) {
        let arena_map = self.child_source(db);
        let arena_map = arena_map.as_ref();

        for (local_id, source) in arena_map.value.iter() {
            let id = CtorId {
                parent: *self,
                local_id,
            };
            map[keys::CTOR].insert(source.clone(), id);
        }
    }
}

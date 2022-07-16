use base_db::input::FileId;

use crate::db::DefDatabase;
use crate::dyn_map::DynMap;
use crate::id::{HasSource, Lookup, MemberId, ModuleDefId, ModuleId};
use crate::item_scope::ItemScope;
use crate::keys;

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
                        let src = loc.source(db);

                        map[keys::FUNC].insert(src, func);
                    }
                },
                // @TODO
                | _ => {},
            }
        }

        fn add_member(db: &dyn DefDatabase, file_id: FileId, map: &mut DynMap, inst: MemberId) {
            let loc = inst.lookup(db);

            if loc.id.file_id == file_id {
                let src = loc.source(db);

                map[keys::INST].insert(src, inst);
            }
        }
    }
}

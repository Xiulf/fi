use crate::{
    db::DefDatabase,
    dyn_map::DynMap,
    id::{HasSource, InstanceId, Lookup, ModuleDefId, ModuleId},
    item_scope::ItemScope,
    keys,
};
use base_db::input::FileId;

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

        self.instances().for_each(|inst| add_instance(db, file_id, map, inst));

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

        fn add_instance(db: &dyn DefDatabase, file_id: FileId, map: &mut DynMap, inst: InstanceId) {
            let loc = inst.lookup(db);

            if loc.id.file_id == file_id {
                let src = loc.source(db);

                map[keys::INST].insert(src, inst);
            }
        }
    }
}

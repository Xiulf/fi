use std::sync::Mutex;

use super::*;

pub struct ClassSolveCtx {
    members: FxHashMap<ClassId, Arc<Members>>,
    state: Mutex<State>,
}

#[derive(Default)]
struct State {}

impl ClassSolveCtx {
    pub fn class_ctx_query(db: &dyn HirDatabase, lib: LibId) -> Arc<Self> {
        let mut members = FxHashMap::default();

        for lib in db.libs().toposort(Some(lib)) {
            let def_map = db.def_map(lib);

            members.extend(
                def_map
                    .modules()
                    .flat_map(|(_, m)| m.scope.classes())
                    .map(|c| (c, db.members(lib, c))),
            );
        }

        Arc::new(Self {
            members,
            state: Mutex::default(),
        })
    }
}

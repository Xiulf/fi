use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::id::{LocalModuleId, ModuleDefId};
use crate::name::Name;
use crate::path::Path;
use crate::per_ns::PerNs;
use crate::visibility::Visibility;
use base_db::libs::LibId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResolveMode {
    Import,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum FixPoint {
    Yes,
    No,
}

#[derive(Debug, Clone)]
pub(super) struct ResolveResult {
    pub(super) resolved_def: PerNs,
    pub(super) segment_index: Option<usize>,
    pub(super) fixpoint: FixPoint,
    pub(super) lib: Option<LibId>,
}

impl ResolveResult {
    fn empty(fixpoint: FixPoint) -> Self {
        ResolveResult::with(PerNs::none(), fixpoint, None, None)
    }

    fn with(resolved_def: PerNs, fixpoint: FixPoint, segment_index: Option<usize>, lib: Option<LibId>) -> Self {
        ResolveResult {
            resolved_def,
            fixpoint,
            segment_index,
            lib,
        }
    }
}

impl DefMap {
    pub fn resolve_path(&self, db: &dyn DefDatabase, original: LocalModuleId, path: &Path) -> (PerNs, Option<usize>) {
        let res = self.resolve_mod_path(db, ResolveMode::Other, original, path);

        (res.resolved_def, res.segment_index)
    }

    pub(super) fn resolve_import(
        &self,
        db: &dyn DefDatabase,
        original: LocalModuleId,
        path: &Path,
    ) -> (PerNs, Option<usize>) {
        let res = self.resolve_mod_path(db, ResolveMode::Import, original, path);

        (res.resolved_def, res.segment_index)
    }

    pub(super) fn resolve_mod_path(
        &self,
        db: &dyn DefDatabase,
        mode: ResolveMode,
        original: LocalModuleId,
        path: &Path,
    ) -> ResolveResult {
        let mut segments = path.segments().iter().enumerate();
        let mut curr_per_ns = {
            let (_, segment) = match segments.next() {
                | Some((idx, segment)) => (idx, segment),
                | None => return ResolveResult::empty(FixPoint::Yes),
            };

            self.resolve_name_in_module(db, original, &segment, mode)
        };

        for (i, segment) in segments {
            let curr = match curr_per_ns.modules {
                | Some(r) => r.0,
                | None => return ResolveResult::empty(FixPoint::No),
            };

            curr_per_ns = match curr {
                | ModuleDefId::ModuleId(module) => {
                    if module.lib != self.lib {
                        let path = Path::from_segments(path.segments()[i..].iter().cloned());
                        let def_map = db.def_map(module.lib);
                        let (def, s) = def_map.resolve_import(db, module.local_id, &path);

                        return ResolveResult::with(def, FixPoint::Yes, s.map(|s| s + i), Some(module.lib));
                    }

                    let module_data = &self[module.local_id];
                    let in_scope = module_data.scope.get(&segment);

                    in_scope
                    // if module.local_id == original {
                    //     in_scope
                    // } else {
                    //     module_data
                    //         .exports
                    //         .get(db, self, module.local_id, &segment)
                    //         .or(in_scope)
                    // }
                },
                | s => return ResolveResult::with(PerNs::from(s), FixPoint::Yes, Some(i), Some(self.lib)),
            };
        }

        ResolveResult::with(curr_per_ns, FixPoint::Yes, None, Some(self.lib))
    }

    pub(crate) fn resolve_name_in_module(
        &self,
        db: &dyn DefDatabase,
        module: LocalModuleId,
        name: &Name,
        mode: ResolveMode,
    ) -> PerNs {
        let in_scope = self[module].scope.get(name);
        // let from_scope = match mode {
        //     | ResolveMode::Import => self[module].exports.get(db, self, module, name).or(in_scope),
        //     | ResolveMode::Other => in_scope,
        // };
        let from_scope = in_scope;

        let from_extern = self
            .extern_prelude
            .get(name)
            .map_or(PerNs::none(), |&it| PerNs::modules(it, Visibility::Public));

        from_scope.or(from_extern)
    }
}

use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::id::{LocalModuleId, ModuleDefId};
use crate::name::Name;
use crate::path::ModPath;
use crate::per_ns::PerNs;
use base_db::libs::LibId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ResolveMode {
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
    pub(crate) fn resolve_path(
        &self,
        db: &dyn DefDatabase,
        original: LocalModuleId,
        path: &ModPath,
    ) -> (PerNs, Option<usize>) {
        let res = self.resolve_mod_path(db, ResolveMode::Other, original, path);

        (res.resolved_def, res.segment_index)
    }

    pub(super) fn resolve_mod_path(
        &self,
        db: &dyn DefDatabase,
        mode: ResolveMode,
        original: LocalModuleId,
        path: &ModPath,
    ) -> ResolveResult {
        let mut result = ResolveResult::empty(FixPoint::No);
        let new = self.resolve_mod_path_single(db, mode, original, path);

        result.resolved_def = result.resolved_def.or(new.resolved_def);

        if result.fixpoint == FixPoint::No {
            result.fixpoint = new.fixpoint;
        }

        result.lib = result.lib.or(new.lib);
        result.segment_index = match (result.segment_index, new.segment_index) {
            | (Some(idx), None) => Some(idx),
            | (Some(old), Some(new)) => Some(old.max(new)),
            | (None, new) => new,
        };

        result
    }

    pub(super) fn resolve_mod_path_single(
        &self,
        db: &dyn DefDatabase,
        mode: ResolveMode,
        original: LocalModuleId,
        path: &ModPath,
    ) -> ResolveResult {
        let mut segments = path.segments().iter().enumerate();
        let mut curr_per_ns = {
            let (_, segment) = match segments.next() {
                | Some((idx, segment)) => (idx, segment),
                | None => return ResolveResult::empty(FixPoint::Yes),
            };

            self.resolve_name_in_module(db, original, &segment)
        };

        for (i, segment) in segments {
            let curr = match curr_per_ns.modules {
                | Some(r) => r,
                | None => return ResolveResult::empty(FixPoint::No),
            };

            curr_per_ns = match curr {
                | ModuleDefId::ModuleId(module) => {
                    if module.lib != self.lib {
                        let path = ModPath::from_segments(path.segments()[i..].iter().cloned());
                        let def_map = db.def_map(module.lib);
                        let (def, s) = def_map.resolve_path(db, module.local_id, &path);

                        return ResolveResult::with(def, FixPoint::Yes, s.map(|s| s + i), Some(module.lib));
                    }

                    let module_data = &self[module.local_id];

                    module_data.scope.get(&segment)
                },
                | s => return ResolveResult::with(PerNs::modules(s), FixPoint::Yes, Some(i), Some(self.lib)),
            };
        }

        ResolveResult::with(curr_per_ns, FixPoint::Yes, None, Some(self.lib))
    }

    fn resolve_name_in_module(&self, db: &dyn DefDatabase, module: LocalModuleId, name: &Name) -> PerNs {
        let from_scope = self[module].scope.get(name);
        let from_extern = self
            .extern_prelude
            .get(name)
            .map_or(PerNs::none(), |&it| PerNs::modules(it));

        from_scope.or(from_extern)
    }
}

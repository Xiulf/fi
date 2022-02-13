use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::id::{LocalModuleId, ModuleDefId};
use crate::name::{AsName, Name};
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
        let (name, module) = match path.segments().split_last() {
            | Some((name, rest)) if rest.is_empty() => (name, None),
            | Some((name, rest)) => (name, Some(Path::from_segments(rest.to_vec()).to_string().as_name())),
            | None => return ResolveResult::empty(FixPoint::Yes),
        };

        let (module_id, or_module) = match module {
            | Some(module) => match self.module_names.get(&module) {
                | Some(&module_id) => (module_id, None),
                | None => {
                    let module = path.to_string().as_name();

                    match self.module_names.get(&module) {
                        | Some(&module_id) => (module_id, None),
                        | None => return ResolveResult::empty(FixPoint::Yes),
                    }
                },
            },
            | None => (self.module_id(original), self.module_names.get(name)),
        };

        let mut def = if module_id.lib != self.lib {
            let def_map = db.def_map(module_id.lib);

            def_map.resolve_name_in_module(db, module_id.local_id, name, mode)
        } else {
            self.resolve_name_in_module(db, module_id.local_id, name, mode)
        };

        if let Some(id) = or_module {
            def = def.or(PerNs::modules((ModuleDefId::ModuleId(*id), Visibility::Public)));
        }

        ResolveResult::with(def, FixPoint::Yes, None, Some(module_id.lib))

        /*let mut segments = path.segments().iter().enumerate();
        let mut curr_per_ns = {
            let (_, segment) = match segments.next() {
                | Some((idx, segment)) => (idx, segment),
                | None => return ResolveResult::empty(FixPoint::Yes),
            };

            self.resolve_name_in_module(db, original, &segment, mode)
        };

        let mut visibility = Visibility::Public;
        let mut parent = self.root;

        for (i, segment) in segments {
            let (curr, vis) = match curr_per_ns.modules {
                | Some(r) => r,
                | None => return ResolveResult::empty(FixPoint::No),
            };

            if let Visibility::Public = visibility {
                if !vis.is_visible_from_def_map(self, original) {
                    visibility = vis;
                }
            }

            curr_per_ns = match curr {
                | ModuleDefId::ModuleId(module) => {
                    if module.lib != self.lib {
                        let path = Path::from_segments(path.segments()[i..].iter().cloned());
                        let def_map = db.def_map(module.lib);
                        let (def, s) = def_map.resolve_import(db, module.local_id, &path);

                        return ResolveResult::with(
                            def.with_lower_vis(visibility),
                            FixPoint::Yes,
                            s.map(|s| s + i),
                            Some(module.lib),
                        );
                    }

                    if self[module.local_id].parent == Some(parent) {
                        visibility = Visibility::Public;
                    }

                    parent = module.local_id;

                    let module_data = &self[module.local_id];
                    let in_scope = module_data.scope.get(&segment);

                    if module.local_id == original {
                        in_scope.with_lower_vis(visibility)
                    } else {
                        module_data
                            .scope
                            .get_reexport(&segment)
                            .or(in_scope)
                            .with_lower_vis(visibility)
                    }
                },
                | s => {
                    return ResolveResult::with(
                        PerNs::from(s).map(|id| (id, visibility)),
                        FixPoint::Yes,
                        Some(i),
                        Some(self.lib),
                    );
                },
            };
        }

        let def = match curr_per_ns.modules {
            | Some((ModuleDefId::ModuleId(m), _)) if m.lib == self.lib && self[m.local_id].parent == Some(parent) => {
                curr_per_ns.with_vis(Visibility::Public)
            },
            | _ => curr_per_ns.with_lower_vis(visibility),
        };

        ResolveResult::with(def, FixPoint::Yes, None, Some(self.lib))*/
    }

    pub(crate) fn resolve_name_in_module(
        &self,
        db: &dyn DefDatabase,
        module: LocalModuleId,
        name: &Name,
        mode: ResolveMode,
    ) -> PerNs {
        let in_scope = self[module].scope.get(name);
        let from_scope = match mode {
            | ResolveMode::Import => self[module].scope.get_reexport(name).or(in_scope),
            | ResolveMode::Other => in_scope,
        };

        from_scope
    }
}

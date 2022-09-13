use std::collections::HashSet;
use std::fs;
use std::sync::Arc;

use base_db::cfg::CfgOptions;
use base_db::input::FileId;
use base_db::libs::LibSet;
use base_db::manifest::{self, Manifest};
use base_db::paths::{AbsolutePath, AbsolutePathBuf, InputPaths};

use super::notifications::Progress;
use super::LspState;
use crate::analysis::AnalysisChange;

impl LspState {
    pub fn fetch_workspaces(&mut self) -> anyhow::Result<()> {
        self.report_progress("projects scanned", Progress::Begin, None, None)?;
        let manifests = self
            .config
            .workspaces
            .iter()
            .filter_map(|path| Manifest::load(path).ok())
            .collect::<Vec<_>>();

        if manifests == self.manifests {
            self.report_progress("projects scanned", Progress::End, None, None)?;
            return Ok(());
        }

        let cfg = CfgOptions::default();
        let mut libs = LibSet::default();
        let mut paths = InputPaths::default();
        let mut roots = Vec::with_capacity(manifests.len());
        let mut vfs = self.vfs.write();
        let mut workspace_libs = Vec::with_capacity(manifests.len());

        for (manifest, path) in manifests.iter().zip(&self.config.workspaces) {
            let lib = manifest::load_manifest(
                &mut self.analysis.db,
                &mut paths,
                manifest,
                &cfg,
                &mut libs,
                &mut roots,
                path.parent().unwrap(),
                &mut |db, _, _, _, path, text| {
                    let text_arc = text.clone();
                    vfs.set_file_content(path, Some(text.into_bytes().into()));
                    let file_id = vfs.file_id(path).unwrap();
                    let file = FileId(file_id.0);

                    db.set_file_text(file, text_arc.into());
                    file
                },
            )?;

            workspace_libs.push(lib);
        }

        let mut change = AnalysisChange::default();

        change.set_libs(libs);
        change.set_roots(roots);

        self.analysis.apply_change(change);
        self.manifests = manifests;
        self.libs = Arc::new(workspace_libs);
        drop(vfs);
        self.report_progress("projects scanned", Progress::End, None, None)?;

        Ok(())
    }
}

pub fn discover(path: &AbsolutePath) -> Option<AbsolutePathBuf> {
    if path.file_name().unwrap_or_default() == Manifest::FILE_NAME {
        return Some(path.to_absolute_path_buf());
    }

    let mut curr = Some(path);

    while let Some(path) = curr {
        let file = path.join(Manifest::FILE_NAME);

        if fs::metadata(&file).is_ok() {
            return Some(file);
        }

        curr = path.parent();
    }

    None
}

pub fn discover_all(paths: &[AbsolutePathBuf]) -> Vec<AbsolutePathBuf> {
    let mut res = paths
        .iter()
        .filter_map(|it| discover(it.as_ref()))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    res.sort();
    res
}

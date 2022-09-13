use std::collections::HashSet;
use std::fs;
use std::sync::Arc;

use base_db::libs::LibSet;
use paths::{AbsPath, AbsPathBuf};
use project::manifest::Manifest;
use project::Workspace;

use super::notifications::Progress;
use super::LspState;
use crate::analysis::AnalysisChange;

impl LspState {
    pub fn fetch_workspaces(&mut self) -> anyhow::Result<()> {
        self.report_progress("projects scanned", Progress::Begin, None, None)?;
        let mut vfs = self.vfs.write();
        let workspaces = self
            .config
            .workspaces
            .iter()
            .filter_map(|path| Workspace::load(path.clone(), &mut vfs).ok())
            .collect::<Vec<_>>();

        if workspaces == *self.workspaces {
            drop(vfs);
            self.report_progress("projects scanned", Progress::End, None, None)?;
            return Ok(());
        }

        let mut change = AnalysisChange::default();
        let mut libs = LibSet::default();

        for ws in &workspaces {
            libs.extend(ws.to_libs(&Default::default()));
        }

        change.set_libs(libs);
        self.analysis.apply_change(change);
        self.workspaces = Arc::new(workspaces);

        drop(vfs);

        self.process_vfs_changes();
        self.report_progress("projects scanned", Progress::End, None, None)?;

        Ok(())
    }
}

pub fn discover(path: &AbsPath) -> Option<AbsPathBuf> {
    if path.file_name().unwrap_or_default() == Manifest::FILE_NAME {
        return Some(path.to_owned());
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

pub fn discover_all(paths: &[AbsPathBuf]) -> Vec<AbsPathBuf> {
    let mut res = paths
        .iter()
        .filter_map(|it| discover(it.as_ref()))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    res.sort();
    res
}

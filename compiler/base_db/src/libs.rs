use std::fmt;
use std::ops::Index;
use std::path::PathBuf;

use cfg::CfgOptions;
use rustc_hash::{FxHashMap, FxHashSet};
use vfs::FileId;

#[derive(Debug, Clone)]
pub struct LibData {
    pub id: LibId,
    pub name: String,
    pub kind: LibKind,
    pub deps: Vec<LibId>,
    pub links: Vec<PathBuf>,
    pub cfg_options: CfgOptions,

    /// Shade projects don't have a root file
    /// However, source roots are only created after loading projects
    /// so this is used to identify the project's source root.
    pub root_file: FileId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LibKind {
    Dynamic,
    Static,
    Executable,
}

impl Default for LibKind {
    fn default() -> Self {
        LibKind::Dynamic
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LibId(pub u32);

#[derive(Default, Debug, Clone)]
pub struct LibSet {
    libs: FxHashMap<LibId, LibData>,
}

impl LibSet {
    pub fn add_lib(
        &mut self,
        name: impl Into<String>,
        kind: LibKind,
        links: Vec<PathBuf>,
        cfg_options: CfgOptions,
        root_file: FileId,
    ) -> LibId {
        let name = name.into();
        let id = LibId(self.libs.len() as u32);
        let data = LibData {
            id,
            name,
            kind,
            links,
            cfg_options,
            root_file,
            deps: Vec::new(),
        };

        self.libs.insert(id, data);
        id
    }

    pub fn add_dep(&mut self, from: LibId, to: LibId, cfg: &CfgOptions) -> Result<(), CyclicDependenciesError> {
        if self.dfs_find(from, to, &mut FxHashSet::default()) {
            return Err(CyclicDependenciesError {
                from: (from, self[from].name.clone()),
                to: (to, self[to].name.clone()),
            });
        }

        self.libs.get_mut(&from).unwrap().deps.push(to);

        let to = self.libs.get_mut(&to).unwrap();

        to.cfg_options = to.cfg_options.merge(cfg);

        Ok(())
    }

    pub fn extend(&mut self, other: LibSet) {
        self.libs.extend(other.libs);
    }

    pub fn is_empty(&self) -> bool {
        self.libs.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = LibId> + '_ {
        self.libs.keys().copied()
    }

    pub fn find(&self, name: &str) -> Option<&LibData> {
        self.libs.values().find(|l| l.name == name)
    }

    pub fn all_deps(&self, lib: LibId) -> Vec<LibId> {
        let mut res = Vec::new();
        let mut visited = FxHashSet::default();

        go(self, &mut visited, &mut res, lib);

        return res;

        fn go(libs: &LibSet, visited: &mut FxHashSet<LibId>, res: &mut Vec<LibId>, source: LibId) {
            if !visited.insert(source) {
                return;
            }

            for &dep in libs[source].deps.iter() {
                go(libs, visited, res, dep);
            }

            res.push(source);
        }
    }

    pub fn toposort(&self) -> Vec<LibId> {
        let mut res = Vec::new();
        let mut visited = FxHashSet::default();

        for &lib in self.libs.keys() {
            go(self, &mut visited, &mut res, lib);
        }

        return res;

        fn go(libs: &LibSet, visited: &mut FxHashSet<LibId>, res: &mut Vec<LibId>, source: LibId) {
            if !visited.insert(source) {
                return;
            }

            for &dep in libs[source].deps.iter() {
                go(libs, visited, res, dep);
            }

            res.push(source);
        }
    }

    fn dfs_find(&self, target: LibId, from: LibId, visited: &mut FxHashSet<LibId>) -> bool {
        if !visited.insert(from) {
            return false;
        }

        if target == from {
            return true;
        }

        for dep in &self[from].deps {
            if self.dfs_find(target, *dep, visited) {
                return true;
            }
        }

        false
    }
}

impl Index<LibId> for LibSet {
    type Output = LibData;

    fn index(&self, index: LibId) -> &Self::Output {
        &self.libs[&index]
    }
}

#[derive(Debug)]
pub struct CyclicDependenciesError {
    from: (LibId, String),
    to: (LibId, String),
}

impl fmt::Display for CyclicDependenciesError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cyclid dependencies: {} -> {}", self.from.1, self.to.1)
    }
}

impl std::error::Error for CyclicDependenciesError {
}

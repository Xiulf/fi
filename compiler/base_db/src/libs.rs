use std::fmt;
use std::ops::Index;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::input::{FileId, SourceRootId};

#[derive(Debug, Clone)]
pub struct LibData {
    pub id: LibId,
    pub name: String,
    pub kind: LibKind,
    pub deps: Vec<LibId>,
    pub dependent: Vec<LibId>,
    pub source_root: SourceRootId,
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
        source_root: SourceRootId,
        root_file: FileId,
    ) -> (LibId, bool) {
        let name = name.into();

        if let Some(id) = self
            .libs
            .iter()
            .find_map(|(id, data)| if data.name == name { Some(*id) } else { None })
        {
            (id, true)
        } else {
            let id = LibId(self.libs.len() as u32);
            let data = LibData {
                id,
                name,
                kind,
                root_file,
                source_root,
                deps: Vec::new(),
                dependent: Vec::new(),
            };

            self.libs.insert(id, data);

            (id, false)
        }
    }

    pub fn add_dep(&mut self, from: LibId, to: LibId) -> Result<(), CyclicDependenciesError> {
        if self.dfs_find(from, to, &mut FxHashSet::default()) {
            return Err(CyclicDependenciesError {
                from: (from, self[from].name.clone()),
                to: (to, self[to].name.clone()),
            });
        }

        self.libs.get_mut(&from).unwrap().deps.push(to);
        self.libs.get_mut(&to).unwrap().dependent.push(from);

        Ok(())
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

    pub fn dependant(&self, _lib: LibId) -> Vec<LibId> {
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

            for &dep in libs[source].dependent.iter() {
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

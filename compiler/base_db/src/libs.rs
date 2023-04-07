use rustc_hash::FxHashSet;

use crate::input::SourceRoot;
use crate::Db;

#[derive(Default, Debug, Clone)]
pub struct LibSet {
    libs: Vec<LibId>,
}

#[salsa::input]
pub struct LibId {
    #[return_ref]
    pub name: String,
    pub kind: LibKind,
    #[return_ref]
    pub deps: Vec<LibId>,
    pub source_root: SourceRoot,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LibKind {
    Executable,
    #[default]
    DynamicLib,
    StaticLib,
}

impl LibId {
    pub fn all_deps(db: &dyn Db, lib: LibId) -> Vec<LibId> {
        let mut res = Vec::new();
        let mut visited = FxHashSet::default();

        go(db, &mut visited, &mut res, lib);

        return res;

        fn go(db: &dyn Db, visited: &mut FxHashSet<LibId>, res: &mut Vec<LibId>, source: LibId) {
            if !visited.insert(source) {
                return;
            }

            for &dep in source.deps(db) {
                go(db, visited, res, dep);
            }

            res.push(source);
        }
    }
}

impl LibSet {
    pub fn add_lib(&mut self, db: &dyn Db, name: impl ToString, kind: LibKind, source_root: SourceRoot) -> LibId {
        let lib = LibId::new(db, name.to_string(), kind, Vec::new(), source_root);
        self.libs.push(lib);
        lib
    }

    pub fn add_dep(&mut self, db: &mut dyn Db, from: LibId, to: LibId) -> Result<(), CyclicDependencyError> {
        if Self::dfs_find(db, from, to, &mut FxHashSet::default()) {
            return Err(CyclicDependencyError { from, to });
        }

        let mut deps = from.deps(db).clone();
        deps.push(to);
        from.set_deps(db).to(deps);
        Ok(())
    }

    pub fn extend(&mut self, other: Self) {
        self.libs.extend(other.libs);
    }

    pub fn is_empty(&self) -> bool {
        self.libs.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = LibId> + '_ {
        self.libs.iter().copied()
    }

    pub fn find<'a>(&'a self, db: &'a dyn Db, name: &'a str) -> impl Iterator<Item = LibId> + 'a {
        self.libs.iter().filter(move |l| l.name(db) == name).copied()
    }

    pub fn toposort(&self, db: &dyn Db, start: Option<LibId>) -> Vec<LibId> {
        let mut res = Vec::new();
        let mut visited = FxHashSet::default();

        if let Some(start) = start {
            go(db, &mut visited, &mut res, start);
            return res;
        }

        for &lib in &self.libs {
            go(db, &mut visited, &mut res, lib);
        }

        return res;

        fn go(db: &dyn Db, visited: &mut FxHashSet<LibId>, res: &mut Vec<LibId>, source: LibId) {
            if !visited.insert(source) {
                return;
            }

            for &dep in source.deps(db).iter() {
                go(db, visited, res, dep);
            }

            res.push(source);
        }
    }

    fn dfs_find(db: &dyn Db, target: LibId, from: LibId, visited: &mut FxHashSet<LibId>) -> bool {
        if !visited.insert(from) {
            return false;
        }

        if target == from {
            return true;
        }

        for &dep in from.deps(db) {
            if Self::dfs_find(db, target, dep, visited) {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CyclicDependencyError {
    pub from: LibId,
    pub to: LibId,
}

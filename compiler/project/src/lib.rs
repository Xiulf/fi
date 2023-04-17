#![feature(trait_upcasting)]

use base_db::input::SourceRoot;
use base_db::libs::LibSet;
use base_db::Db;
use index_vec::IndexVec;
use manifest::{Manifest, ProjectType};
use paths::{AbsPath, AbsPathBuf};
use semver::Version;
use vfs::file_set::FileSetConfig;
use vfs::{FileSet, VirtualFileSystem};

pub mod manifest;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Packages {
    packages: IndexVec<Package, PackageData>,
}

index_vec::define_index_type! {
    pub struct Package = u32;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageData {
    pub name: String,
    pub version: Version,
    pub type_: ProjectType,
    pub dependencies: Vec<Dependency>,
    pub root_dir: AbsPathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub package: Package,
}

impl Packages {
    pub fn load_project(
        &mut self,
        vfs: &mut VirtualFileSystem,
        db: &mut dyn Db,
        path: AbsPathBuf,
    ) -> anyhow::Result<Package> {
        tracing::trace!("load project {}", path.display());
        let manifest = Manifest::load(path.join(Manifest::FILE_NAME))?;
        let package = self.alloc_package(&manifest, path.clone());

        manifest::load_files(vfs, db, path.join(&manifest.project.src_dir))?;

        for (name, dep) in manifest.dependencies {
            let dep = self.load_dependency(vfs, db, &name, dep, &path)?;

            self.add_dependency(package, Dependency { package: dep });
        }

        Ok(package)
    }

    pub fn load_files(
        &mut self,
        vfs: &mut VirtualFileSystem,
        db: &mut dyn Db,
        files: Vec<AbsPathBuf>,
        name: String,
        type_: ProjectType,
        dependencies: Vec<String>,
        search_dir: AbsPathBuf,
    ) -> anyhow::Result<Package> {
        let mut root_dir = files[0].parent().unwrap().to_path_buf();

        for file in files {
            while !file.starts_with(&root_dir) {
                root_dir = root_dir.parent().unwrap().to_path_buf();
            }

            manifest::load_files(vfs, db, file)?;
        }

        let manifest = Manifest {
            project: manifest::Project {
                name,
                version: Version::new(0, 0, 0),
                type_,
                src_dir: Manifest::default_src_dir(),
            },
            dependencies: Default::default(),
        };

        let package = self.alloc_package(&manifest, root_dir);

        for dep in dependencies {
            let (name, dep) = manifest::parse_dependency(&dep)?;
            let dep = self.load_dependency(vfs, db, name, dep, &search_dir)?;

            self.add_dependency(package, Dependency { package: dep });
        }

        Ok(package)
    }

    fn load_dependency(
        &mut self,
        vfs: &mut VirtualFileSystem,
        db: &mut dyn Db,
        name: &str,
        dep: manifest::Dependency,
        parent_dir: &AbsPath,
    ) -> anyhow::Result<Package> {
        match dep {
            | manifest::Dependency::Local { path } => {
                let path = if path.is_relative() {
                    parent_dir.join(path)
                } else {
                    AbsPathBuf::assert(path)
                };

                self.load_project(vfs, db, path)
            },
            | manifest::Dependency::Normal { version: _ } => todo!("{name}"),
        }
    }

    pub fn to_file_sets(&self, vfs: &VirtualFileSystem, db: &dyn Db) -> Vec<FileSet> {
        let mut fs = FileSetConfig::builder();

        for data in self.packages.iter() {
            fs.add_file_set(vec![data.root_dir.clone().into()]);
        }

        fs.build().partition(vfs, db)
    }

    pub fn to_source_roots(&self, vfs: &VirtualFileSystem, db: &dyn Db) -> Vec<SourceRoot> {
        self.to_file_sets(vfs, db)
            .into_iter()
            .map(|fs| SourceRoot::new(db, fs))
            .collect()
    }

    pub fn to_lib_set(&self, db: &mut dyn Db, source_roots: &[SourceRoot]) -> anyhow::Result<LibSet> {
        let mut set = LibSet::default();
        let mut map = index_vec::index_vec![None; self.packages.len()];

        for (pkg, data) in self.packages.iter_enumerated() {
            let source_root = source_roots[pkg.index()];
            let lib = set.add_lib(db, &data.name, data.type_.into(), source_root);

            map[pkg] = Some(lib);
        }

        for (pkg, data) in self.packages.iter_enumerated() {
            for dep in &data.dependencies {
                set.add_dep(db, map[pkg].unwrap(), map[dep.package].unwrap())
                    .map_err(|_| anyhow::anyhow!("cyclic dependencies"))?;
            }
        }

        Ok(set)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Package, &PackageData)> {
        self.packages.iter_enumerated()
    }

    fn alloc_package(&mut self, manifest: &Manifest, root_dir: AbsPathBuf) -> Package {
        for (pkg, data) in self.packages.iter_mut_enumerated() {
            if data.name == manifest.project.name
                && data.version == manifest.project.version
                && data.type_ == manifest.project.type_
            {
                return pkg;
            }
        }

        self.packages.push(PackageData {
            name: manifest.project.name.clone(),
            version: manifest.project.version.clone(),
            type_: manifest.project.type_,
            dependencies: Vec::new(),
            root_dir,
        })
    }

    fn add_dependency(&mut self, pkg: Package, dep: Dependency) {
        self.packages[pkg].dependencies.push(dep);
    }
}

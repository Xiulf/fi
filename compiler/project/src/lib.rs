use std::ops::Index;
use std::path::PathBuf;

use arena::{Arena, Idx};
use base_db::libs::{LibKind, LibSet};
use cfg::CfgOptions;
use manifest::Manifest;
use paths::AbsPathBuf;
use rustc_hash::FxHashMap;
use vfs::file_set::{FileSet, FileSetConfig};
use vfs::{FileId, VfsPath, VirtualFileSystem};

pub mod manifest;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Workspace {
    local: Option<LocalProject>,
    packages: Arena<PackageData>,
    root_dir: AbsPathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalProject {
    lib_name: String,
    lib_output: LibKind,
    lib_links: Vec<PathBuf>,

    files: Vec<FileId>,
}

pub type Package = Idx<PackageData>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageData {
    pub name: String,
    pub version: String,
    pub output: LibKind,
    pub links: Vec<PathBuf>,
    pub dependencies: Vec<Dependency>,

    pub root_file: FileId,
    pub manifest_path: AbsPathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub package: Package,
    pub cfg_opts: CfgOptions,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageRoot {
    pub include: Vec<AbsPathBuf>,
    pub exclude: Vec<AbsPathBuf>,
}

impl Workspace {
    pub fn load(root_dir: AbsPathBuf, vfs: &mut VirtualFileSystem) -> anyhow::Result<Self> {
        let mut workspace = Workspace {
            local: None,
            packages: Arena::default(),
            root_dir: root_dir.clone(),
        };

        let cfg = cfg::CfgOptions::default();

        manifest::load_project(&mut workspace, vfs, &cfg, &root_dir)?;

        Ok(workspace)
    }

    pub fn local_files(
        vfs: &mut VirtualFileSystem,
        root_dir: AbsPathBuf,
        files: Vec<AbsPathBuf>,
        lib_name: String,
        lib_output: LibKind,
        lib_links: Vec<PathBuf>,
        dependencies: Vec<AbsPathBuf>,
    ) -> anyhow::Result<Self> {
        let files = files
            .into_iter()
            .map(|path| manifest::load_file(vfs, path))
            .collect::<anyhow::Result<_>>()?;

        let mut workspace = Workspace {
            local: Some(LocalProject {
                lib_name,
                lib_output,
                lib_links,
                files,
            }),
            packages: Arena::default(),
            root_dir,
        };

        let cfg = cfg::CfgOptions::default();

        for dep in dependencies {
            manifest::load_project(&mut workspace, vfs, &cfg, &dep)?;
        }

        Ok(workspace)
    }

    pub fn packages(&self) -> impl Iterator<Item = Package> + ExactSizeIterator + '_ {
        self.packages.iter().map(|(id, _)| id)
    }

    pub fn to_roots(&self) -> Vec<PackageRoot> {
        self.local
            .iter()
            .map(|_| PackageRoot {
                include: vec![self.root_dir.clone()],
                exclude: Vec::new(),
            })
            .chain(self.packages.iter().map(|(_, data)| {
                let mut root = PackageRoot::default();
                let package_root = data.manifest_path.parent().unwrap().to_owned();

                root.include.push(package_root.clone());

                root.exclude.push(package_root.join(".git"));
                root.exclude.push(package_root.join("target"));

                root
            }))
            .collect()
    }

    pub fn file_sets(workspaces: &[Self], vfs: &VirtualFileSystem) -> Vec<FileSet> {
        let mut fs = FileSetConfig::builder();

        for root in workspaces.iter().flat_map(|ws| ws.to_roots()) {
            let file_set = root.include.iter().cloned().map(VfsPath::from).collect();

            fs.add_file_set(file_set);
        }

        let config = fs.build();

        config.partition(vfs)
    }

    pub fn to_libs(&self, cfg_opts: &CfgOptions) -> LibSet {
        let mut libs = LibSet::default();
        let mut map = FxHashMap::default();

        if let Some(local) = &self.local {
            libs.add_lib(
                local.lib_name.clone(),
                local.lib_output,
                local.lib_links.clone(),
                cfg_opts.clone(),
                local.files[0],
            );
        }

        for (id, data) in self.packages.iter() {
            let lib = libs.add_lib(
                data.name.clone(),
                data.output,
                data.links.clone(),
                cfg_opts.clone(),
                data.root_file,
            );

            map.insert(id, lib);
        }

        for (id, data) in self.packages.iter() {
            for dep in data.dependencies.iter() {
                let from = map[&id];
                let to = map[&dep.package];

                if let Err(e) = libs.add_dep(from, to, &dep.cfg_opts) {
                    tracing::error!("{}", e);
                }
            }
        }

        libs
    }

    fn package_for_name(&self, name: &str) -> Option<Package> {
        self.packages
            .iter()
            .find(|(_, data)| data.name == name)
            .map(|(id, _)| id)
    }

    fn alloc_package(&mut self, manifest: &Manifest, manifest_path: AbsPathBuf, root_file: FileId) -> Package {
        self.packages.alloc(PackageData {
            name: manifest.project.name.clone(),
            version: manifest.project.version.clone(),
            output: manifest.project.output,
            links: manifest.project.link.clone(),
            dependencies: Vec::new(),
            manifest_path,
            root_file,
        })
    }

    fn add_dependency(&mut self, package: Package, dep: Dependency) {
        self.packages[package].dependencies.push(dep);
    }
}

impl Index<Package> for Workspace {
    type Output = PackageData;

    fn index(&self, package: Package) -> &Self::Output {
        &self.packages[package]
    }
}

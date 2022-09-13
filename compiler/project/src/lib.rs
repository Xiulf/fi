use std::ops::Index;

use arena::{Arena, Idx};
use manifest::{Manifest, OutputKind};
use paths::AbsPathBuf;
use vfs::VirtualFileSystem;

pub mod manifest;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Workspace {
    packages: Arena<PackageData>,
    root_dir: AbsPathBuf,
}

pub type Package = Idx<PackageData>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageData {
    pub name: String,
    pub version: String,
    pub output: OutputKind,
    pub dependencies: Vec<Dependency>,

    pub manifest_path: AbsPathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub package: Package,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageRoot {
    pub include: Vec<AbsPathBuf>,
    pub exclude: Vec<AbsPathBuf>,
}

impl Workspace {
    pub fn load(root_dir: AbsPathBuf, vfs: &mut VirtualFileSystem) -> anyhow::Result<Self> {
        let mut workspace = Workspace {
            packages: Arena::default(),
            root_dir: root_dir.clone(),
        };

        let cfg = cfg::CfgOptions::default();

        manifest::load_project(&mut workspace, vfs, &cfg, &root_dir)?;

        Ok(workspace)
    }

    pub fn packages(&self) -> impl Iterator<Item = Package> + ExactSizeIterator + '_ {
        self.packages.iter().map(|(id, _)| id)
    }

    pub fn to_roots(&self) -> Vec<PackageRoot> {
        self.packages
            .iter()
            .map(|(_, data)| {
                let mut root = PackageRoot::default();
                let package_root = data.manifest_path.parent().unwrap().to_owned();

                root.include.push(package_root.clone());

                root.exclude.push(package_root.join(".git"));
                root.exclude.push(package_root.join("target"));

                root
            })
            .collect()
    }

    fn package_for_name(&self, name: &str) -> Option<Package> {
        self.packages
            .iter()
            .find(|(_, data)| data.name == name)
            .map(|(id, _)| id)
    }

    fn alloc_package(&mut self, manifest: &Manifest, manifest_path: AbsPathBuf) -> Package {
        self.packages.alloc(PackageData {
            name: manifest.project.name.clone(),
            version: manifest.project.version.clone(),
            output: manifest.project.output,
            dependencies: Vec::new(),
            manifest_path,
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

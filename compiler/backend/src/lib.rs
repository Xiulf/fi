use std::path::{Path, PathBuf};
use std::sync::Arc;

#[salsa::query_group(BackendDatabaseStorage)]
pub trait BackendDatabase: lower::LowerDatabase {
    fn assembly(&self, lib: hir::ir::LibId, module: hir::ir::ModuleId) -> Arc<Assembly>;

    fn link_type(&self, lib: hir::ir::LibId, module: hir::ir::ModuleId) -> linker::LinkOutputType;
}

fn link_type(
    db: &dyn BackendDatabase,
    lib: hir::ir::LibId,
    module: hir::ir::ModuleId,
) -> linker::LinkOutputType {
    let file = db.module_tree(lib).file(module);
    let hir = db.module_hir(file);

    if let Some(out_ty) = hir.out_type() {
        match out_ty {
            "exe" => linker::LinkOutputType::Exe,
            "staticlib" => linker::LinkOutputType::Lib,
            "dylib" => linker::LinkOutputType::Dylib,
            _ => panic!("invalid output type: {}", out_ty),
        }
    } else {
        let has_main = hir.items.values().any(|item| item.is_main());

        if has_main {
            linker::LinkOutputType::Exe
        } else {
            linker::LinkOutputType::Dylib
        }
    }
}

fn lib_prefix(out_type: linker::LinkOutputType) -> &'static str {
    match out_type {
        linker::LinkOutputType::Exe => "",
        linker::LinkOutputType::Lib => "lib",
        linker::LinkOutputType::Dylib => "lib",
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assembly {
    path: PathBuf,
}

impl Assembly {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

pub fn assembly(
    db: &dyn BackendDatabase,
    lib: hir::ir::LibId,
    module: hir::ir::ModuleId,
) -> Arc<Assembly> {
    let mir = db.lower(lib, module);
    let obj_file = lowlang::assemble::assemble(&mir, (*db.target(lib)).clone());
    let mut linker = linker::get_linker(&db.target(lib));
    let tree = db.module_tree(lib);
    let data = tree.data(module);
    let out_type = db.link_type(lib, module);
    let extension = linker::extension(out_type, &db.target(lib));
    let out_filename: PathBuf = format!(
        "{}/{}{}{}",
        db.manifest(lib).package.target_dir.display(),
        lib_prefix(out_type),
        data.name,
        extension.to_string_lossy(),
    )
    .into();

    if let linker::LinkOutputType::Lib = out_type {
        let file = std::fs::File::create(&out_filename).unwrap();
        let mut ab = ar::GnuBuilder::new(
            file,
            std::iter::once(
                obj_file
                    .path()
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned(),
            )
            .chain(data.children.iter().map(|&dep| {
                let data = tree.get(dep);
                let asm = db.assembly(lib, data.id);

                asm.path()
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned()
            }))
            .map(|p| p.into_bytes())
            .collect(),
        );

        ab.append_path(obj_file.path()).unwrap();

        for &dep in &data.children {
            let data = tree.get(dep);
            let asm = db.assembly(lib, data.id);

            ab.append_path(asm.path()).unwrap();
        }

        std::mem::drop(ab);
        std::process::Command::new("ranlib")
            .arg(&out_filename)
            .status()
            .unwrap();
    } else {
        linker.add_object(obj_file.path());
        linker.include_path(db.manifest(lib).package.target_dir.as_path());

        for &dep in &data.children {
            let data = tree.get(dep);
            let _ = db.assembly(lib, data.id);

            match db.link_type(lib, data.id) {
                linker::LinkOutputType::Exe => unreachable!(),
                linker::LinkOutputType::Lib => linker.link_staticlib(&**data.name.symbol),
                linker::LinkOutputType::Dylib => linker.link_dylib(&**data.name.symbol),
            }
        }

        linker.finalize();
        linker.set_output_type(out_type, &out_filename);
        linker.output_filename(&out_filename);
        linker.cmd().status().unwrap();
    }

    Arc::new(Assembly { path: out_filename })
}

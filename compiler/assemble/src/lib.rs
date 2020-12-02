use std::path::{Path, PathBuf};
use std::sync::Arc;

#[cfg(feature = "cranelift")]
use codegen_cranelift::create_backend as codegen_backend;

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: ::mir::MirDatabase + ::mir::ToMirDb {
    fn assembly(&self, lib: source::LibId, module: mir::ir::ModuleId) -> Arc<Assembly>;

    fn link_type(&self, lib: source::LibId, module: mir::ir::ModuleId) -> linker::LinkOutputType;
}

fn link_type(
    db: &dyn CodegenDatabase,
    lib: source::LibId,
    module: mir::ir::ModuleId,
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
    db: &dyn crate::CodegenDatabase,
    lib: source::LibId,
    module: mir::ir::ModuleId,
) -> Arc<Assembly> {
    let mir = db.module_mir(lib, module);
    let obj_file = codegen_backend(db.to_mir_db(), lib, mir);
    let mut linker = linker::get_linker(&db.target(lib));
    let tree = db.module_tree(lib);
    let data = tree.data(module);
    let out_type = db.link_type(lib, module);
    let extension = linker::extension(out_type, &db.target(lib));
    let out_filename: PathBuf = format!(
        "{}/{}.{}",
        db.manifest(lib).package.target_dir.display(),
        data.name,
        extension.to_string_lossy(),
    )
    .into();

    linker.add_object(obj_file.path());

    for &dep in &data.children {
        let data = tree.get(dep);
        let asm = db.assembly(lib, data.id);

        match db.link_type(lib, data.id) {
            linker::LinkOutputType::Exe => unreachable!(),
            linker::LinkOutputType::Lib => linker.link_staticlib(asm.path()),
            linker::LinkOutputType::Dylib => linker.link_dylib(asm.path()),
        }
    }

    linker.set_output_type(out_type, &out_filename);
    linker.output_filename(&out_filename);
    linker.finalize();
    linker.cmd().status().unwrap();

    Arc::new(Assembly { path: out_filename })
}

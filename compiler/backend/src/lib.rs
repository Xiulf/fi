use std::path::{Path, PathBuf};
use std::sync::Arc;

#[salsa::query_group(BackendDatabaseStorage)]
pub trait BackendDatabase: lower::LowerDatabase {
    fn assembly(&self, lib: hir::ir::LibId, module: hir::ir::ModuleId) -> Arc<Assembly>;

    fn link_type(&self, lib: hir::ir::LibId, module: hir::ir::ModuleId) -> linker::LinkOutputType;
}

fn link_type(db: &dyn BackendDatabase, lib: hir::ir::LibId, module: hir::ir::ModuleId) -> linker::LinkOutputType {
    println!("link_type before");
    let file = db.module_tree(lib).file(module);
    println!("link_type after");
    let hir = db.module_hir(file);

    if let Some(out_ty) = hir.out_type() {
        match out_ty {
            | "exe" => linker::LinkOutputType::Exe,
            | "staticlib" => linker::LinkOutputType::Lib,
            | "dylib" => linker::LinkOutputType::Dylib,
            | _ => panic!("invalid output type: {}", out_ty),
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
        | linker::LinkOutputType::Exe => "",
        | linker::LinkOutputType::Lib => "lib",
        | linker::LinkOutputType::Dylib => "lib",
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

pub fn assembly(db: &dyn BackendDatabase, lib: hir::ir::LibId, module: hir::ir::ModuleId) -> Arc<Assembly> {
    let mir = db.lower(lib, module);
    let obj_file = lowlang::assemble::assemble(&mir, (*db.target(lib)).clone());
    let mut linker = linker::get_linker(&db.target(lib));
    println!("assembly before");
    let tree = db.module_tree(lib);
    println!("assembly after");
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
            std::iter::once(os_str_to_bytes(obj_file.path().file_name().unwrap()))
                .chain(dep_idents(db, lib, &tree, &data.children))
                .collect(),
        );

        ab.append_path(obj_file.path()).unwrap();

        dep_paths(&mut ab, db, lib, &tree, &data.children);

        std::mem::drop(ab);

        if cfg!(not(windows)) {
            std::process::Command::new("ranlib").arg(&out_filename).status().unwrap();
        }

        fn dep_idents(
            db: &dyn BackendDatabase,
            lib: hir::ir::LibId,
            tree: &hir::module_tree::ModuleTree,
            deps: &[hir::module_tree::ModuleIndex],
        ) -> Vec<Vec<u8>> {
            deps.iter()
                .copied()
                .flat_map(|dep| {
                    let data = tree.get(dep);
                    let asm = db.assembly(lib, data.id);

                    std::iter::once(os_str_to_bytes(asm.path().file_name().unwrap())).chain(dep_idents(db, lib, tree, &data.children))
                })
                .collect()
        }

        fn dep_paths<T: std::io::Write>(
            ab: &mut ar::GnuBuilder<T>,
            db: &dyn BackendDatabase,
            lib: hir::ir::LibId,
            tree: &hir::module_tree::ModuleTree,
            deps: &[hir::module_tree::ModuleIndex],
        ) {
            for &dep in deps {
                let data = tree.get(dep);
                let asm = db.assembly(lib, data.id);

                ab.append_path(asm.path()).unwrap();
                dep_paths(ab, db, lib, tree, &data.children);
            }
        }
    } else {
        linker.add_object(obj_file.path());
        linker.include_path(db.manifest(lib).package.target_dir.as_path());
        linker.runtime_path(db.manifest(lib).package.target_dir.as_path());

        dep_paths(&mut *linker, db, lib, &tree, &data.children);

        linker.finalize();
        linker.set_output_type(out_type, &out_filename);
        linker.output_filename(&out_filename);
        linker.cmd().status().unwrap();

        fn dep_paths(
            linker: &mut dyn linker::linker::Linker,
            db: &dyn BackendDatabase,
            lib: hir::ir::LibId,
            tree: &hir::module_tree::ModuleTree,
            deps: &[hir::module_tree::ModuleIndex],
        ) {
            for &dep in deps {
                let data = tree.get(dep);
                let _ = db.assembly(lib, data.id);

                match db.link_type(lib, data.id) {
                    | linker::LinkOutputType::Exe => unreachable!(),
                    | linker::LinkOutputType::Lib => linker.link_staticlib(&**data.name.symbol),
                    | linker::LinkOutputType::Dylib => linker.link_dylib(&**data.name.symbol),
                }

                dep_paths(linker, db, lib, tree, &data.children);
            }
        }
    }

    Arc::new(Assembly { path: out_filename })
}

#[cfg(windows)]
fn os_str_to_bytes(os_str: &std::ffi::OsStr) -> Vec<u8> {
    use std::os::windows::ffi::OsStrExt;
    let utf16 = os_str.encode_wide().collect::<Vec<_>>();
    let utf16 = utf16.as_slice();
    let utf8 = utf16 as *const _;
    let utf8 = unsafe { std::slice::from_raw_parts(utf8 as *const u8, utf16.len() * 2) };

    utf8.to_vec()
}

#[cfg(not(windows))]
fn os_str_to_bytes(os_str: &std::ffi::OsStr) -> Vec<u8> {
    let utf8 = os_str as *const _ as *const [u8];
    let utf8 = unsafe { &*utf8 };

    utf8.to_vec()
}

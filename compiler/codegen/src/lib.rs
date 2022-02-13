pub mod assembly;
pub mod db;
pub mod linker;

use std::sync::Arc;

pub(crate) fn build_assembly(db: &dyn db::CodegenDatabase, lib: hir::Lib) -> Arc<assembly::Assembly> {
    for module in lib.modules(db.upcast()) {
        let ir = db.module_ir(module);

        println!("{}", ir.display(db.upcast()));
    }

    // let object = crate::ModuleCtx::with_mcx(db, |mcx| mcx.build(lib));
    // let mut file = NamedTempFile::new().unwrap();
    //
    // file.write(object.emit().unwrap().as_slice()).unwrap();

    // let mut linker = crate::linker::create();
    // let file = NamedTempFile::new().unwrap();
    //
    // linker.args(&["-rpath", "."]);
    // linker.add_object(object_file.path());
    //
    // for dep in lib.dependencies(db.upcast()) {
    //     let _ = db.lib_assembly(dep.lib);
    //     let name = dep.lib.name(db.upcast()).to_string();
    //
    //     match db.libs()[dep.lib.into()].kind {
    //         | LibKind::Dynamic => linker.add_shared_object(&name),
    //         | LibKind::Static => linker.add_static_lib(&name),
    //         | LibKind::Executable => panic!("cannot link with an executable"),
    //     }
    // }
    //
    // match db.libs()[lib.into()].kind {
    //     | LibKind::Dynamic => linker.build_shared_object(file.path()),
    //     | LibKind::Static => linker.build_static_lib(file.path()),
    //     | LibKind::Executable => linker.build_executable(file.path()),
    // }
    //
    // eprintln!("{:?}", linker.cmd());
    //
    // linker.run();

    // Arc::new(Assembly { lib, file })
    todo!()
}

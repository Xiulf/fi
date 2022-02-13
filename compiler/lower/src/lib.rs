pub mod db;
pub mod types;

use std::sync::Arc;

pub fn module_ir(db: &dyn db::LowerDatabase, module: hir::Module) -> Arc<ir::Module> {
    let name = module.name(db.upcast());
    let mut m = ir::Module::new(name.to_string());

    for decl in module.declarations(db.upcast()) {
        match decl {
            | hir::ModuleDef::Func(func) => {
                if let Some(_) = func.as_assoc_item(db.upcast()) {
                    continue;
                }

                let linkage = if func.is_foreign(db.upcast()) {
                    ir::Linkage::Import
                } else if func.is_exported(db.upcast()) {
                    ir::Linkage::Export
                } else {
                    ir::Linkage::Local
                };

                let id = db.func_ir(func);

                m.declare_func(linkage, id);
            },
            | _ => {},
        }
    }

    Arc::new(m)
}

pub fn func_ir(db: &dyn db::LowerDatabase, func: hir::Func) -> ir::FuncId {
    let name = func.link_name(db.upcast());
    let ty = db.lower_type(func.ty(db.upcast()));

    println!("{} : ${}", name, ty.display(db.upcast()));
    todo!()
}

pub fn body_ir(db: &dyn db::LowerDatabase, body: hir::id::DefWithBodyId) -> ir::BodyId {
    todo!()
}

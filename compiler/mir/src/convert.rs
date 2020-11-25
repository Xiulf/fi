use crate::{ir, MirDatabase};
use hir::ir as hir;
use std::collections::HashMap;
use std::sync::Arc;

pub fn convert(db: &dyn MirDatabase, lib: hir::LibId, id: hir::ModuleId) -> Arc<ir::Module> {
    let file = db.module_tree(lib).file(id);
    let hir = db.module_hir(file);
    let mut converter = Converter::new(db);

    converter.convert(&hir);

    Arc::new(converter.finish())
}

pub struct Converter<'db> {
    db: &'db dyn MirDatabase,
    bodies: Vec<ir::Body>,
}

pub struct BodyConverter<'db> {
    db: &'db dyn MirDatabase,
    types: Arc<check::TypeCheckResult>,
    locals: HashMap<hir::HirId, ir::Local>,
}

impl<'db> Converter<'db> {
    pub fn new(db: &'db dyn MirDatabase) -> Self {
        Converter {
            db,
            bodies: Vec::new(),
        }
    }

    pub fn finish(self) -> ir::Module {
        ir::Module {
            bodies: self.bodies,
        }
    }

    pub fn convert(&mut self, hir: &hir::Module) {
        for (_, item) in &hir.items {
            match &item.kind {
                hir::ItemKind::Func { body, .. }
                | hir::ItemKind::Const { body, .. }
                | hir::ItemKind::Static { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let mut conv = BodyConverter::new(self.db, ty);

                    self.bodies.push(conv.convert(body));
                }
                _ => {}
            }
        }

        // @todo: convert impl items
    }
}

impl<'db> BodyConverter<'db> {
    pub fn new(db: &'db dyn MirDatabase, types: Arc<check::TypeCheckResult>) -> Self {
        BodyConverter {
            db,
            types,
            locals: HashMap::new(),
        }
    }

    pub fn convert(&mut self, body: &hir::Body) -> ir::Body {}
}

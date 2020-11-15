use crate::ir;
use crate::HirDatabase;
use std::collections::BTreeMap;
use std::sync::Arc;
use syntax::ast;

pub fn convert(db: &dyn HirDatabase, file: source::FileId) -> Arc<ir::Module> {
    let ast = db.parse(file);
    let mut converter = Converter::new(db);

    converter.convert_ast(&ast);
    converter.finish(&ast)
}

pub struct Converter<'db> {
    db: &'db (dyn HirDatabase + 'db),
    id_counter: usize,
    current_item: ir::DefId,
    items: BTreeMap<ir::HirId, ir::Item>,
    iface_items: BTreeMap<ir::IfaceItemId, ir::IfaceItem>,
    impl_items: BTreeMap<ir::ImplItemId, ir::ImplItem>,
    bodies: BTreeMap<ir::BodyId, ir::Body>,
}

impl<'db> Converter<'db> {
    pub fn new(db: &'db (dyn HirDatabase + 'db)) -> Self {
        Converter {
            db,
            id_counter: 0,
            current_item: ir::DefId::dummy(),
            items: BTreeMap::new(),
            iface_items: BTreeMap::new(),
            impl_items: BTreeMap::new(),
            bodies: BTreeMap::new(),
        }
    }

    pub fn finish(self, ast: &ast::Module) -> Arc<ir::Module> {
        Arc::new(ir::Module {
            id: ir::ModuleId::from_name(ast.name.symbol),
            span: ast.span,
            name: ast.name,
            body_ids: self.bodies.keys().copied().collect(),
            items: self.items,
            iface_items: self.iface_items,
            impl_items: self.impl_items,
            bodies: self.bodies,
        })
    }

    pub fn convert_ast(&mut self, ast: &ast::Module) {}
}

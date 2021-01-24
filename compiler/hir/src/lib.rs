#![feature(drain_filter, vec_remove_item, or_patterns)]

pub mod convert;
pub mod ir;
pub mod lang;
pub mod module_tree;
pub mod resolve;

use module_tree::ModuleTree;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: syntax::SyntaxDatabase {
    #[salsa::invoke(module_tree::load_external)]
    fn external_modules(&self, lib: source::LibId) -> Arc<Vec<module_tree::ExternalModuleData>>;

    fn external_item_data(&self, lib: source::LibId, module: ir::ModuleId) -> Arc<ExternalItemData>;

    #[salsa::invoke(ModuleTree::query)]
    fn module_tree(&self, lib: source::LibId) -> Arc<ModuleTree>;

    #[salsa::invoke(convert::convert)]
    fn module_hir(&self, file: source::FileId) -> Arc<ir::Module>;

    #[salsa::invoke(lang::LangItems::collect)]
    fn lang_items(&self) -> Arc<lang::LangItems>;
}

#[derive(Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ExternalItemData {
    pub file: source::FileId,
    pub items: HashMap<ir::DefId, (ir::Ident, Vec<ir::Attribute>, ExternalItemOrigin)>,
    pub datas: HashMap<ir::DefId, Vec<(ir::Ident, ir::HirId)>>,
    pub classes: HashMap<ir::DefId, Vec<(ir::Ident, ir::HirId)>>,
    pub instances: HashMap<ir::DefId, Vec<(ir::Ident, ir::HirId)>>,
    pub fixities: HashMap<ir::DefId, (ir::Assoc, ir::Prec, ir::Res)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ExternalItemOrigin {
    Item,
    ClassItem(ir::DefId),
    InstanceItem(ir::DefId),
}

pub fn store_item_data(db: &dyn HirDatabase, lib: source::LibId) {
    let manifest = db.manifest(lib);
    let path = format!("{}/meta/items", manifest.package.target_dir.display());

    for data in &db.module_tree(lib).data {
        let hir = db.module_hir(data.file);
        let file = std::fs::File::create(format!("{}/{:?}", path, data.id)).unwrap();
        let mut items = HashMap::new();
        let mut datas = HashMap::new();
        let mut classes = HashMap::new();
        let mut instances = HashMap::new();
        let mut fixities = HashMap::new();

        for item in hir.items.values() {
            match &item.kind {
                | ir::ItemKind::Func { .. }
                | ir::ItemKind::Static { .. }
                | ir::ItemKind::Const { .. }
                | ir::ItemKind::DataCtor { .. }
                | ir::ItemKind::Foreign { .. }
                | ir::ItemKind::Alias { .. } => {
                    items.insert(item.id.owner, (item.name, item.attrs.clone(), ExternalItemOrigin::Item));
                },
                | ir::ItemKind::Data { body, .. } => {
                    items.insert(item.id.owner, (item.name, item.attrs.clone(), ExternalItemOrigin::Item));
                    datas.insert(item.id.owner, body.iter().map(|id| (hir.items[id].name, *id)).collect());
                },
                | ir::ItemKind::Class { body, .. } => {
                    items.insert(item.id.owner, (item.name, item.attrs.clone(), ExternalItemOrigin::Item));
                    classes.insert(item.id.owner, body.items.iter().map(|item| (item.name, item.id.0)).collect());
                },
                | ir::ItemKind::Instance { body, .. } => {
                    items.insert(item.id.owner, (item.name, item.attrs.clone(), ExternalItemOrigin::Item));
                    instances.insert(item.id.owner, body.items.iter().map(|item| (item.name, item.id.0)).collect());
                },
                | ir::ItemKind::Fixity { assoc, prec, func } => {
                    items.insert(item.id.owner, (item.name, item.attrs.clone(), ExternalItemOrigin::Item));
                    fixities.insert(item.id.owner, (*assoc, *prec, *func));
                },
            }
        }

        for item in hir.class_items.values() {
            items.insert(item.id.owner, (item.name, Vec::new(), ExternalItemOrigin::ClassItem(item.owner.owner)));
        }

        for item in hir.instance_items.values() {
            items.insert(item.id.owner, (item.name, Vec::new(), ExternalItemOrigin::InstanceItem(item.owner.owner)));
        }

        let data = ExternalItemData {
            file: data.file,
            items,
            datas,
            classes,
            instances,
            fixities,
        };

        bincode::serialize_into(file, &data).unwrap();
    }
}

fn external_item_data(db: &dyn HirDatabase, lib: source::LibId, module: ir::ModuleId) -> Arc<ExternalItemData> {
    let manifest = db.manifest(lib);
    let path = format!("{}/meta/items/{:?}", manifest.package.target_dir.display(), module);
    let file = std::fs::File::open(path).unwrap();

    bincode::deserialize_from(file).unwrap()
}

impl ExternalItemData {
    pub fn is_intrinsic(&self, id: &ir::DefId) -> bool {
        let intrinsic = ir::Symbol::new("repr");

        self.items[id].1.iter().any(|a| a.name.symbol == intrinsic)
    }

    pub fn is_no_mangle(&self, id: &ir::DefId) -> bool {
        let no_mangle = ir::Symbol::new("no_mangle");

        self.items[id].1.iter().any(|a| a.name.symbol == no_mangle)
    }

    pub fn repr(&self, id: &ir::DefId) -> Option<&str> {
        let repr = ir::Symbol::new("repr");

        self.items[id]
            .1
            .iter()
            .filter_map(|a| if a.name.symbol == repr { a.str_arg() } else { None })
            .next()
    }

    pub fn abi(&self, id: &ir::DefId) -> Option<&str> {
        let abi = ir::Symbol::new("abi");

        self.items[id]
            .1
            .iter()
            .filter_map(|a| if a.name.symbol == abi { a.str_arg() } else { None })
            .next()
    }
}

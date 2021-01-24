use super::*;

#[derive(Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ExternalTypeData {
    pub file: source::FileId,
    pub types: HashMap<ir::DefId, ty::Ty>,
    pub variants: HashMap<ir::DefId, ty::Variants>,
    pub classes: HashMap<ir::DefId, ExternalClass>,
    pub instances: HashMap<ir::DefId, Vec<ty::Instance>>,
}

#[derive(Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ExternalClass {
    pub var_kinds: HashMap<ty::TypeVar, ty::Ty>,
    pub deps: Vec<ir::FunDep>,
    pub items: Vec<(ir::Ident, ir::HirId)>,
}

pub fn load_external(db: &dyn TypeDatabase, lib: source::LibId, module: ir::ModuleId) -> Arc<ExternalTypeData> {
    let manifest = db.manifest(lib);
    let path = format!("{}/meta/types/{:?}", manifest.package.target_dir.display(), module);
    let file = std::fs::File::open(path).unwrap();

    bincode::deserialize_from(file).unwrap()
}

pub fn store_external(db: &dyn TypeDatabase, lib: source::LibId) {
    let manifest = db.manifest(lib);
    let path = format!("{}/meta/types", manifest.package.target_dir.display());

    for data in &db.module_tree(lib).data {
        let file = std::fs::File::create(format!("{}/{:?}", path, data.id)).unwrap();
        let hir = db.module_hir(data.file);
        let types = db.typeck_module(lib, data.id);
        let items = types.items.iter().map(|(k, v)| (*k, v.ty.clone())).collect();
        let variants = types.variants.clone();
        let classes = types
            .classes
            .iter()
            .map(|(id, ct)| {
                if let ir::ItemKind::Class { head, body } = &hir.items[&(*id).into()].kind {
                    (*id, ExternalClass {
                        var_kinds: ct.var_kinds.clone(),
                        deps: head.fundeps.clone(),
                        items: body.items.iter().map(|i| (i.name, i.id.0)).collect(),
                    })
                } else {
                    unreachable!();
                }
            })
            .collect();

        let instances = types.instances.clone();
        let data = ExternalTypeData {
            file: data.file,
            types: items,
            variants,
            classes,
            instances,
        };

        bincode::serialize_into(file, &data).unwrap();
    }
}

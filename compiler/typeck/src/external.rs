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
        let mut types = HashMap::new();
        let mut variants = HashMap::new();
        let mut classes = HashMap::new();
        let mut instances = HashMap::new();
        let ctx = ctx::Ctx::new(db, data.file);

        for item in hir.items.values() {
            match &item.kind {
                | ir::ItemKind::Func { .. }
                | ir::ItemKind::Static { .. }
                | ir::ItemKind::Const { .. }
                | ir::ItemKind::Foreign { .. }
                | ir::ItemKind::DataCtor { .. } => {
                    types.insert(item.id.owner, db.typecheck(item.id.owner).ty.clone());
                },
                | ir::ItemKind::Alias { .. } => {
                    types.insert(item.id.owner, db.typecheck(item.id.owner).ty.clone());
                },
                | ir::ItemKind::Data { .. } => {
                    types.insert(item.id.owner, db.typecheck(item.id.owner).ty.clone());
                    variants.insert(item.id.owner, db.variants(item.id.owner));
                },
                | ir::ItemKind::Class { head, body } => {
                    classes.insert(item.id.owner, ExternalClass {
                        var_kinds: head.vars.iter().map(|v| (ty::TypeVar(v.id), ctx.ty_kind(v.span, data.file))).collect(),
                        deps: head.fundeps.clone(),
                        items: body.items.iter().map(|i| (i.name, i.id.0)).collect(),
                    });
                },
                | ir::ItemKind::Instance { head, .. } => {
                    instances.entry(head.trait_).or_insert(Vec::new()).push(db.instance(item.id.owner));
                },
                | _ => {},
            }
        }

        for item in hir.instance_items.values() {
            types.insert(item.id.owner, db.typecheck(item.id.owner).ty.clone());
        }

        let data = ExternalTypeData {
            file: data.file,
            types,
            variants,
            classes,
            instances,
        };

        bincode::serialize_into(file, &data).unwrap();
    }
}

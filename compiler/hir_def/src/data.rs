use ra_ap_stdx::hash::NoHashHashMap;
use syntax::ptr::AstPtr;

use crate::attrs::Attrs;
use crate::def_map::ModuleScope;
use crate::id::{self, FixityId, ImplId, TypeVarId, TypedItemId, ValueId};
use crate::item_tree::{AttrOwner, FixityKind};
use crate::name::Name;
use crate::source::HasSource;
use crate::type_ref::TypeRefId;
use crate::Db;

#[salsa::tracked]
pub struct ModuleData {
    #[id]
    pub id: id::ModuleId,
    #[return_ref]
    pub scope: ModuleScope,
}

#[salsa::tracked]
pub struct FixityData {
    #[id]
    pub id: id::FixityId,
    pub kind: FixityKind,
}

#[salsa::tracked]
pub struct ValueData {
    #[id]
    pub id: id::ValueId,
    #[return_ref]
    pub attrs: Attrs,
    pub ty: Option<TypeRefId>,
    #[return_ref]
    pub type_vars: Box<[TypeVarId]>,
    pub is_foreign: bool,
    pub has_body: bool,
}

#[salsa::tracked]
pub struct TypeAliasData {
    #[id]
    pub id: id::TypeAliasId,
}

#[salsa::tracked]
pub struct TypeCtorData {
    #[id]
    pub id: id::TypeCtorId,
}

#[salsa::tracked]
pub struct CtorData {
    #[id]
    pub id: id::CtorId,
}

#[salsa::tracked]
pub struct TraitData {
    #[id]
    pub id: id::TraitId,
}

#[salsa::tracked]
pub struct ImplData {
    #[id]
    pub id: id::ImplId,
    #[return_ref]
    pub items: NoHashHashMap<Name, ValueId>,
}

#[salsa::tracked]
pub fn fixity_data(db: &dyn Db, id: FixityId) -> FixityData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    let data = &item_tree[it.value];

    FixityData::new(db, id, data.kind)
}

#[salsa::tracked]
pub fn value_data(db: &dyn Db, id: ValueId) -> ValueData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    let data = &item_tree[it.value];
    let source = id.source(db).value;
    let (_, src_map, type_vars) = TypedItemId::from(id).type_map(db);
    let attrs = item_tree.attrs(AttrOwner::Item(it.value.into()));
    let ty = source.ty().and_then(|t| src_map.typ_for_src(AstPtr::new(&t)));
    let is_foreign = data.is_foreign;
    let has_body = data.has_body;

    ValueData::new(db, id, attrs, ty, type_vars, is_foreign, has_body)
}

#[salsa::tracked]
pub fn impl_data(db: &dyn Db, id: ImplId) -> ImplData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    let data = &item_tree[it.value];
    let items = data
        .items
        .iter()
        .map(|&local_id| {
            (
                item_tree[local_id].name,
                ValueId::new(db, id.into(), it.with_value(local_id)),
            )
        })
        .collect();

    ImplData::new(db, id, items)
}

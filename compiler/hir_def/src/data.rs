use either::Either;
use ra_ap_stdx::hash::NoHashHashMap;
use syntax::ptr::AstPtr;
use triomphe::Arc;

use crate::attrs::Attrs;
use crate::def_map::ModuleScope;
use crate::id::{
    self, CtorId, FieldId, FixityId, ImplId, ItemId, TraitId, TypeAliasId, TypeCtorId, TypeDefId, TypeVarId,
    TypedItemId, ValueDefId, ValueId,
};
use crate::item_tree::{AttrOwner, FixityKind};
use crate::name::Name;
use crate::path::Path;
use crate::source::HasSource;
use crate::type_ref::{TypeRefId, WhereClause};
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
    #[return_ref]
    pub attrs: Attrs,
    pub kind: FixityKind,
    #[return_ref]
    pub def_path: Path,
    pub def: Option<Either<ValueDefId, TypeDefId>>,
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
    pub where_clause: Option<Arc<WhereClause>>,
    pub is_foreign: bool,
    pub has_body: bool,
}

#[salsa::tracked]
pub struct TypeAliasData {
    #[id]
    pub id: id::TypeAliasId,
    #[return_ref]
    pub attrs: Attrs,
    #[return_ref]
    pub type_vars: Box<[TypeVarId]>,
    pub ty: Option<TypeRefId>,
}

#[salsa::tracked]
pub struct TypeCtorData {
    #[id]
    pub id: id::TypeCtorId,
    #[return_ref]
    pub attrs: Attrs,
    #[return_ref]
    pub type_vars: Box<[TypeVarId]>,
    pub kind: Option<TypeRefId>,
}

#[salsa::tracked]
pub struct CtorData {
    #[id]
    pub id: id::CtorId,
    #[return_ref]
    pub attrs: Attrs,
    #[return_ref]
    pub types: Box<[TypeRefId]>,
}

#[salsa::tracked]
pub struct FieldData {
    #[id]
    pub id: id::FieldId,
    #[return_ref]
    pub attrs: Attrs,
    pub ty: Option<TypeRefId>,
}

#[salsa::tracked]
pub struct TraitData {
    #[id]
    pub id: id::TraitId,
    #[return_ref]
    pub attrs: Attrs,
    #[return_ref]
    pub type_vars: Box<[TypeVarId]>,
    pub where_clause: Option<Arc<WhereClause>>,
    #[return_ref]
    pub items: NoHashHashMap<Name, ValueId>,
}

#[salsa::tracked]
pub struct ImplData {
    #[id]
    pub id: id::ImplId,
    #[return_ref]
    pub attrs: Attrs,
    pub trait_id: Option<TraitId>,
    #[return_ref]
    pub types: Box<[TypeRefId]>,
    #[return_ref]
    pub type_vars: Box<[TypeVarId]>,
    pub where_clause: Option<Arc<WhereClause>>,
    #[return_ref]
    pub items: NoHashHashMap<Name, ValueId>,
}

#[salsa::tracked]
pub fn fixity_data(db: &dyn Db, id: FixityId) -> FixityData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    let data = &item_tree[it.value];
    let module = id.module(db);
    let attrs = item_tree.attrs(AttrOwner::Item(it.value.into()));
    let def_map = crate::def_map::query(db, module.lib(db));
    let def_path = data.value.clone();
    let def = def_map.resolve_path(db, &data.value, module);
    let def = if data.is_type {
        def.and_then(|d| d.types)
            .and_then(ItemId::as_type_def_id)
            .map(Either::Right)
    } else {
        def.and_then(|d| d.values)
            .and_then(ItemId::as_value_def_id)
            .map(Either::Left)
    };

    FixityData::new(db, id, attrs, data.kind, def_path, def)
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
    let where_clause = source
        .where_clause()
        .and_then(|wc| src_map.where_clause_for_src(AstPtr::new(&wc)));
    let is_foreign = data.is_foreign;
    let has_body = data.has_body;

    ValueData::new(db, id, attrs, ty, type_vars, where_clause, is_foreign, has_body)
}

#[salsa::tracked]
pub fn type_ctor_data(db: &dyn Db, id: TypeCtorId) -> TypeCtorData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    // let data = &item_tree[it.value];
    let source = id.source(db).value;
    let (_, src_map, type_vars) = TypedItemId::from(id).type_map(db);
    let attrs = item_tree.attrs(AttrOwner::Item(it.value.into()));
    let kind = source.kind().and_then(|t| src_map.typ_for_src(AstPtr::new(&t)));

    TypeCtorData::new(db, id, attrs, type_vars, kind)
}

#[salsa::tracked]
pub fn type_alias_data(db: &dyn Db, id: TypeAliasId) -> TypeAliasData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    let source = id.source(db).value;
    let (_, src_map, type_vars) = TypedItemId::from(id).type_map(db);
    let attrs = item_tree.attrs(AttrOwner::Item(it.value.into()));
    let ty = source.ty().and_then(|t| src_map.typ_for_src(AstPtr::new(&t)));

    TypeAliasData::new(db, id, attrs, type_vars, ty)
}

#[salsa::tracked]
pub fn ctor_data(db: &dyn Db, id: CtorId) -> CtorData {
    let type_ctor = id.type_ctor(db);
    let source = id.source(db).value;
    let item_tree = crate::item_tree::query(db, type_ctor.it(db).file);
    let (_, src_map, _) = TypedItemId::from(type_ctor).type_map(db);
    let attrs = item_tree.attrs(AttrOwner::Ctor(id.local_id(db)));
    let types = source
        .types()
        .filter_map(|t| src_map.typ_for_src(AstPtr::new(&t)))
        .collect();

    CtorData::new(db, id, attrs, types)
}

#[salsa::tracked]
pub fn field_data(db: &dyn Db, id: FieldId) -> FieldData {
    let ctor = id.ctor(db);
    let type_ctor = ctor.type_ctor(db);
    let source = id.source(db).value;
    let item_tree = crate::item_tree::query(db, type_ctor.it(db).file);
    let (_, src_map, _) = TypedItemId::from(type_ctor).type_map(db);
    let attrs = item_tree.attrs(AttrOwner::Field(id.local_id(db)));
    let ty = source.ty().and_then(|t| src_map.typ_for_src(AstPtr::new(&t)));

    FieldData::new(db, id, attrs, ty)
}

#[salsa::tracked]
pub fn trait_data(db: &dyn Db, id: TraitId) -> TraitData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    let data = &item_tree[it.value];
    let source = id.source(db).value;
    let (_, src_map, type_vars) = TypedItemId::from(id).type_map(db);
    let attrs = item_tree.attrs(AttrOwner::Item(it.value.into()));
    let where_clause = source
        .where_clause()
        .and_then(|wc| src_map.where_clause_for_src(AstPtr::new(&wc)));
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

    TraitData::new(db, id, attrs, type_vars, where_clause, items)
}

#[salsa::tracked]
pub fn impl_data(db: &dyn Db, id: ImplId) -> ImplData {
    let it = id.it(db);
    let item_tree = crate::item_tree::query(db, it.file);
    let data = &item_tree[it.value];
    let source = id.source(db).value;
    let (_, src_map, type_vars) = TypedItemId::from(id).type_map(db);
    let attrs = item_tree.attrs(AttrOwner::Item(it.value.into()));
    let module = id.module(db);
    let def_map = crate::def_map::query(db, module.lib(db));
    let def = def_map.resolve_path(db, &data.trait_, module);
    let trait_ = def.and_then(|d| d.types).and_then(|t| match t {
        | ItemId::TraitId(id) => Some(id),
        | _ => None,
    });
    let types = source
        .types()
        .filter_map(|t| src_map.typ_for_src(AstPtr::new(&t)))
        .collect();
    let where_clause = source
        .where_clause()
        .and_then(|wc| src_map.where_clause_for_src(AstPtr::new(&wc)));
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

    ImplData::new(db, id, attrs, trait_, types, type_vars, where_clause, items)
}

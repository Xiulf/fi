use crate::ast_id::AstIdMap;
use crate::attrs::AttrsWithOwner;
use crate::body::{Body, BodySourceMap};
use crate::data;
use crate::def_map::DefMap;
use crate::id::*;
use crate::item_tree::ItemTree;
use crate::lang_item::{LangItem, LangItems};
use crate::scope::{ExprScopes, TypeScopes};
use base_db::input::FileId;
use base_db::libs::LibId;
use base_db::{SourceDatabaseExt, Upcast};
use smol_str::SmolStr;
use std::sync::Arc;

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase: SourceDatabaseExt {
    #[salsa::interned]
    fn intern_fixity(&self, loc: FixityLoc) -> FixityId;

    #[salsa::interned]
    fn intern_func(&self, loc: FuncLoc) -> FuncId;

    #[salsa::interned]
    fn intern_static(&self, loc: StaticLoc) -> StaticId;

    #[salsa::interned]
    fn intern_const(&self, loc: ConstLoc) -> ConstId;

    #[salsa::interned]
    fn intern_type_alias(&self, loc: TypeAliasLoc) -> TypeAliasId;

    #[salsa::interned]
    fn intern_type_ctor(&self, loc: TypeCtorLoc) -> TypeCtorId;

    #[salsa::interned]
    fn intern_class(&self, loc: ClassLoc) -> ClassId;

    #[salsa::interned]
    fn intern_instance(&self, loc: InstanceLoc) -> InstanceId;
}

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: InternDatabase {
    #[salsa::invoke(AstIdMap::ast_id_map_query)]
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;

    #[salsa::invoke(ItemTree::item_tree_query)]
    fn item_tree(&self, file_id: FileId) -> Arc<ItemTree>;

    #[salsa::invoke(DefMap::def_map_query)]
    fn def_map(&self, lib: LibId) -> Arc<DefMap>;

    #[salsa::invoke(Body::body_source_map_query)]
    fn body_source_map(&self, def: DefWithBodyId) -> (Arc<Body>, Arc<BodySourceMap>);

    #[salsa::invoke(Body::body_query)]
    fn body(&self, def: DefWithBodyId) -> Arc<Body>;

    #[salsa::invoke(ExprScopes::expr_scopes_query)]
    fn expr_scopes(&self, def: DefWithBodyId) -> Arc<ExprScopes>;

    #[salsa::invoke(TypeScopes::type_scopes_query)]
    fn type_scopes(&self, owner: TypeVarOwner) -> Arc<TypeScopes>;

    #[salsa::invoke(data::FixityData::query)]
    fn fixity_data(&self, id: FixityId) -> Arc<data::FixityData>;

    #[salsa::invoke(data::FuncData::query)]
    fn func_data(&self, id: FuncId) -> Arc<data::FuncData>;

    #[salsa::invoke(data::StaticData::query)]
    fn static_data(&self, id: StaticId) -> Arc<data::StaticData>;

    #[salsa::invoke(data::ConstData::query)]
    fn const_data(&self, id: ConstId) -> Arc<data::ConstData>;

    #[salsa::invoke(data::TypeAliasData::query)]
    fn type_alias_data(&self, id: TypeAliasId) -> Arc<data::TypeAliasData>;

    #[salsa::invoke(data::TypeCtorData::query)]
    fn type_ctor_data(&self, id: TypeCtorId) -> Arc<data::TypeCtorData>;

    #[salsa::invoke(data::ClassData::query)]
    fn class_data(&self, id: ClassId) -> Arc<data::ClassData>;

    #[salsa::invoke(data::InstanceData::query)]
    fn instance_data(&self, id: InstanceId) -> Arc<data::InstanceData>;

    #[salsa::invoke(AttrsWithOwner::attrs_query)]
    fn attrs(&self, def: AttrDefId) -> AttrsWithOwner;

    #[salsa::invoke(LangItems::lib_lang_items_query)]
    fn lib_lang_items(&self, lib: LibId) -> Arc<LangItems>;

    #[salsa::invoke(LangItems::lang_item_query)]
    fn lang_item(&self, lib: LibId, item: SmolStr) -> Option<LangItem>;
}

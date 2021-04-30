use crate::ast_id::AstIdMap;
use crate::body::{Body, BodySourceMap};
use crate::data;
use crate::def_map::DefMap;
use crate::id::*;
use crate::item_tree::ItemTree;
use crate::scope::ExprScopes;
use crate::type_ref::{TypeRef, TypeRefId};
use base_db::input::FileId;
use base_db::libs::LibId;
use base_db::{SourceDatabaseExt, Upcast};
use std::sync::Arc;

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase: SourceDatabaseExt {
    #[salsa::interned]
    fn intern_fixity(&self, loc: FixityLoc) -> FixityId;

    #[salsa::interned]
    fn intern_foreign(&self, loc: ForeignLoc) -> ForeignId;

    #[salsa::interned]
    fn intern_func(&self, loc: FuncLoc) -> FuncId;

    #[salsa::interned]
    fn intern_static(&self, loc: StaticLoc) -> StaticId;

    #[salsa::interned]
    fn intern_const(&self, loc: ConstLoc) -> ConstId;

    #[salsa::interned]
    fn intern_type(&self, loc: TypeLoc) -> TypeId;

    #[salsa::interned]
    fn intern_class(&self, loc: ClassLoc) -> ClassId;

    #[salsa::interned]
    fn intern_instance(&self, loc: InstanceLoc) -> InstanceId;

    #[salsa::interned]
    fn intern_type_ref(&self, type_ref: TypeRef) -> TypeRefId;
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

    #[salsa::invoke(data::FixityData::query)]
    fn fixity_data(&self, id: FixityId) -> Arc<data::FixityData>;
}

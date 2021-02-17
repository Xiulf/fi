use crate::ast_id::AstIdMap;
use crate::id::*;
use base_db::input::FileId;
use base_db::{SourceDatabase, Upcast};
use std::sync::Arc;

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase: SourceDatabase {
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
}

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: InternDatabase {
    #[salsa::invoke(AstIdMap::ast_id_map_query)]
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;
}

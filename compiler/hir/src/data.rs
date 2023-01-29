use crate::def_map::ModuleScope;
use crate::id;

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
}

#[salsa::tracked]
pub struct ValueData {
    #[id]
    pub id: id::ValueId,
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
}

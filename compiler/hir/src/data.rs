use crate::def_map::ModuleScope;
use crate::id;

#[salsa::tracked]
pub struct ModuleData {
    pub id: id::ModuleId,
    #[return_ref]
    pub scope: ModuleScope,
}

#[salsa::tracked]
pub struct FixityData {
    pub id: id::FixityId,
}

#[salsa::tracked]
pub struct ValueData {
    pub id: id::ValueId,
}

#[salsa::tracked]
pub struct TypeAliasData {
    pub id: id::TypeAliasId,
}

#[salsa::tracked]
pub struct TypeCtorData {
    pub id: id::TypeCtorId,
}

#[salsa::tracked]
pub struct CtorData {
    pub id: id::CtorId,
}

#[salsa::tracked]
pub struct TraitData {
    pub id: id::TraitId,
}

#[salsa::tracked]
pub struct ImplData {
    pub id: id::ImplId,
}

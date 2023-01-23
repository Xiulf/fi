use base_db::libs::LibId;

use crate::name::Name;

#[salsa::interned]
pub struct ModuleId {
    pub lib: LibId,
    pub name: Name,
}

use crate::ty::*;
use hir::ir::Ident;

pub type Result<T> = std::result::Result<T, TypeError>;

pub enum TypeError {
    Internal(String),
    CyclicType(Ty),
    Mismatch(Ty, Ty),
    HoleType(Ident, Ty),
}

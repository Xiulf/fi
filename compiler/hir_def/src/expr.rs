use crate::arena::{Idx, RawIdx};
use crate::path::Path;

pub type ExprId = Idx<Expr>;

pub(crate) fn dummy_expr_id() -> ExprId {
    ExprId::from_raw(RawIdx::from(0))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Path(Path),
}

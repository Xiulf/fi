use crate::arena::Idx;

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
}

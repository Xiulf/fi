use crate::arena::Idx;

pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Missing,
}

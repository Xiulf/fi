use crate::arena::Idx;
use crate::name::Name;

pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Missing,
    Wildcard,
    Typed {},
    App {},
    Ctor {},
    Bind { name: Name, subpat: Option<PatId> },
    Lit {},
}

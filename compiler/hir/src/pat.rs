use arena::Idx;

use crate::expr::{ExprId, Literal};
use crate::id::CtorId;
use crate::name::Name;
use crate::path::Path;

pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Missing,
    Wildcard,
    Bind {
        name: Name,
        subpat: Option<PatId>,
    },
    Ctor {
        path: Path,
        ctor: Option<CtorId>,
        args: Box<[PatId]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CaseCheck {
    Literal { lit: Literal },
    Ctor { path: Path, ctor: Option<CtorId> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DecisionTree {
    Leaf(usize),
    Bind(PatId, Box<DecisionTree>),
    Switch {
        val: ExprId,
        cases: Box<(CaseCheck, DecisionTree)>,
        default: Option<Box<DecisionTree>>,
    },
}

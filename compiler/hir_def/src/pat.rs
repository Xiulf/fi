use crate::arena::Idx;
use crate::expr::{Literal, RecordField};
use crate::name::Name;
use crate::path::Path;
use crate::type_ref::TypeRefId;

pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Missing,
    Wildcard,
    Typed { pat: PatId, ty: TypeRefId },
    App { base: PatId, args: Vec<PatId> },
    Path { path: Path },
    Bind { name: Name, subpat: Option<PatId> },
    Tuple { pats: Vec<PatId> },
    Record { fields: Vec<RecordField<PatId>> },
    Lit { lit: Literal },
}

impl Pat {
    pub fn walk(&self, mut f: impl FnMut(PatId)) {
        match self {
            | Pat::Missing | Pat::Wildcard | Pat::Lit { .. } | Pat::Path { .. } | Pat::Bind { subpat: None, .. } => {},
            | Pat::Typed { pat, .. } => f(*pat),
            | Pat::App { base, args } => {
                f(*base);
                args.iter().copied().for_each(f);
            },
            | Pat::Bind {
                subpat: Some(subpat), ..
            } => f(*subpat),
            | Pat::Tuple { pats } => {
                pats.iter().copied().for_each(f);
            },
            | Pat::Record { fields } => {
                fields.iter().map(|f| f.val).for_each(f);
            },
        }
    }
}

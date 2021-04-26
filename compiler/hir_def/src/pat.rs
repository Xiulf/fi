use crate::arena::Idx;
use crate::expr::Literal;
use crate::name::Name;
use crate::path::Path;
use crate::type_ref::TypeRef;

pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Missing,
    Wildcard,
    Typed { pat: PatId, ty: Idx<TypeRef> },
    App { base: PatId, args: Vec<PatId> },
    Ctor { path: Path },
    Bind { name: Name, subpat: Option<PatId> },
    Tuple { pats: Vec<PatId> },
    Lit { lit: Literal },
}

impl Pat {
    pub fn walk(&self, mut f: impl FnMut(PatId)) {
        match self {
            | Pat::Missing | Pat::Wildcard | Pat::Lit { .. } | Pat::Ctor { .. } | Pat::Bind { subpat: None, .. } => {},
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
        }
    }
}

use std::str::FromStr;

use target_lexicon::Triple;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompilerTarget {
    Javascript,
    Native(Triple),
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Optimization {
    #[default]
    None,
}
impl Default for CompilerTarget {
    fn default() -> Self {
        Self::Native(Triple::host())
    }
}

impl FromStr for CompilerTarget {
    type Err = target_lexicon::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            | "js" | "javascript" => Ok(Self::Javascript),
            | _ => Triple::from_str(s).map(Self::Native),
        }
    }
}

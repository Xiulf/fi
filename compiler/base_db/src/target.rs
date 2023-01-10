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

impl CompilerTarget {
    const UNKNOWN: Triple = Triple {
        architecture: target_lexicon::Architecture::Unknown,
        vendor: target_lexicon::Vendor::Unknown,
        operating_system: target_lexicon::OperatingSystem::Unknown,
        environment: target_lexicon::Environment::Unknown,
        binary_format: target_lexicon::BinaryFormat::Unknown,
    };

    pub fn triple(&self) -> &Triple {
        match self {
            | Self::Native(triple) => triple,
            | _ => &Self::UNKNOWN,
        }
    }

    pub fn is_windows(&self) -> bool {
        match self {
            | Self::Native(triple) => triple.operating_system == target_lexicon::OperatingSystem::Windows,
            | _ => false,
        }
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

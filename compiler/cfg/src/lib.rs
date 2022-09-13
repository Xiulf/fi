use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CfgOptions {
    flags: FxHashSet<SmolStr>,
    keys: FxHashMap<SmolStr, CfgValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Cfg {
    Invalid,
    Atom(CfgAtom),
    Any(Box<[Cfg]>),
    All(Box<[Cfg]>),
    Not(Box<Cfg>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CfgAtom {
    Flag(SmolStr),
    Key(SmolStr, CfgValue),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CfgValue {
    Int(i128),
    String(SmolStr),
}

impl CfgOptions {
    pub fn check(&self, cfg: &Cfg) -> Option<bool> {
        cfg.fold(&|atom| match atom {
            | CfgAtom::Flag(flag) => self.flags.contains(flag),
            | CfgAtom::Key(key, value) => self.keys.get(key).map(|v| v == value).unwrap_or(false),
        })
    }

    pub fn flags(&self) -> &FxHashSet<SmolStr> {
        &self.flags
    }

    pub fn keys(&self) -> &FxHashMap<SmolStr, CfgValue> {
        &self.keys
    }

    pub fn enable(&mut self, flag: impl Into<SmolStr>) {
        self.flags.insert(flag.into());
    }

    pub fn set(&mut self, key: impl Into<SmolStr>, value: CfgValue) {
        self.keys.insert(key.into(), value);
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self {
            flags: self.flags.union(&other.flags).cloned().collect(),
            keys: self
                .keys
                .iter()
                .chain(&other.keys)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        }
    }
}

impl Cfg {
    pub fn is_enabled(&self, opts: &CfgOptions) -> bool {
        opts.check(self) != Some(false)
    }

    pub fn fold(&self, check: &dyn Fn(&CfgAtom) -> bool) -> Option<bool> {
        match self {
            | Cfg::Invalid => None,
            | Cfg::Atom(atom) => Some(check(atom)),
            | Cfg::Any(cfg) => cfg.iter().try_fold(false, |s, c| Some(s || c.fold(check)?)),
            | Cfg::All(cfg) => cfg.iter().try_fold(true, |s, c| Some(s && c.fold(check)?)),
            | Cfg::Not(cfg) => cfg.fold(check).map(|s| !s),
        }
    }
}

use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;

use crate::attrs::{AttrInput, AttrInputGroup, Attrs};
use crate::expr::Literal;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CfgOptions {
    flags: FxHashSet<SmolStr>,
    keys: FxHashMap<SmolStr, Literal>,
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
    Key(SmolStr, Literal),
}

impl CfgOptions {
    pub fn check(&self, cfg: &Cfg) -> Option<bool> {
        cfg.fold(&|atom| match atom {
            | CfgAtom::Flag(flag) => self.flags.contains(flag),
            | CfgAtom::Key(key, value) => self.keys.get(key).map(|v| v == value).unwrap_or(false),
        })
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

    pub fn parse(attrs: &Attrs) -> Option<Self> {
        let mut cfgs = attrs.by_key("cfg").groups().map(Self::parse_attr);
        let a = cfgs.next()?;

        match cfgs.next() {
            | Some(b) => Some(Self::All([a, b].into_iter().chain(cfgs).collect())),
            | None => Some(a),
        }
    }

    fn parse_attr(group: &AttrInputGroup) -> Self {
        group.iter().find_map(Self::parse_expr).unwrap_or(Cfg::Invalid)
    }

    fn parse_expr(input: &AttrInput) -> Option<Self> {
        match input {
            | AttrInput::Ident(name) => Some(Cfg::Atom(CfgAtom::Flag(name.into()))),
            | AttrInput::Field(name, value) => match &**value {
                | AttrInput::Group(g) => match name.as_ref() {
                    | "not" => Some(Cfg::Not(Box::new(Self::parse_attr(g)))),
                    | "any" => Some(Cfg::Any(g.iter().filter_map(Self::parse_expr).collect())),
                    | "all" => Some(Cfg::All(g.iter().filter_map(Self::parse_expr).collect())),
                    | _ => None,
                },
                | _ => None,
            },
            | _ => None,
        }
    }
}

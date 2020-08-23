use crate::{Id, ItemId, Symbol};
use diagnostics::{Diagnostic, Reporter, Severity, Span};
use std::collections::HashMap;

#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct Resolver<'a> {
    #[derivative(Debug = "ignore")]
    reporter: &'a Reporter,
    pub(crate) current_module: ItemId,
    modules: HashMap<ItemId, PerNs<Vec<Rib>>>,
}

#[derive(Debug, Clone)]
pub struct PerNs<T> {
    modules: T,
    values: T,
    types: T,
    labels: T,
}

#[derive(Debug, Clone, Copy)]
pub enum Ns {
    Modules,
    Values,
    Types,
    Labels,
}

#[derive(Debug, Clone)]
pub struct Rib {
    names: HashMap<Symbol, Res>,
    kind: RibKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RibKind {
    Global,
    Block,
    Local,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum Res {
    Module(ItemId),
    Item(Id),
    Local(Id),
    Label(Id),
    PrimVal(PrimVal),
    PrimTy(PrimTy),
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum PrimVal {
    True,
    False,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum PrimTy {
    Never,
    Bool,
    Str,
    Int(u8, bool),
    Float(u8),
}

impl<'a> Resolver<'a> {
    pub fn new(reporter: &'a Reporter) -> Self {
        let mut resolver = Resolver {
            reporter,
            current_module: ItemId(0),
            modules: HashMap::new(),
        };

        resolver.add_root();
        resolver
    }

    pub fn add_module(&mut self, id: ItemId) {
        self.modules.insert(
            id,
            PerNs {
                modules: vec![Rib::new(RibKind::Global)],
                values: vec![Rib::new(RibKind::Global)],
                types: vec![Rib::new(RibKind::Global)],
                labels: Vec::new(),
            },
        );
    }

    pub fn set_module(&mut self, id: ItemId) {
        self.current_module = id;
    }

    fn module(&mut self) -> &mut PerNs<Vec<Rib>> {
        self.modules
            .get_mut(&self.current_module)
            .expect("undefined module")
    }

    pub fn define(&mut self, ns: Ns, name: Symbol, span: Span, res: Res) {
        if self.check_duplicate(ns, &name, span) {
            self.module()[ns].last_mut().unwrap().insert(name, res);
        }
    }

    fn check_duplicate(&self, ns: Ns, name: &Symbol, span: Span) -> bool {
        let last_rib = self.modules[&self.current_module][ns].last().unwrap();

        if last_rib.contains(name) {
            self.reporter.add(
                Diagnostic::new(
                    Severity::Error,
                    0003,
                    format!("duplicate declaration '{}'", name),
                )
                .label(Severity::Error, span, None::<String>),
            );

            false
        } else {
            true
        }
    }

    pub fn push_rib(&mut self, ns: Ns, kind: RibKind) {
        self.module()[ns].push(Rib::new(kind));
    }

    pub fn pop_rib(&mut self, ns: Ns) {
        self.module()[ns].pop().unwrap();
    }

    pub fn get_path(&self, ns: Ns, path: &syntax::ast::Path) -> Option<Res> {
        match self.get_path_inner(ns, path, self.current_module) {
            Some(res) => Some(res),
            None if path.root => None,
            None => self.get_path_inner(ns, path, ItemId(0)),
        }
    }

    fn get_path_inner(&self, ns: Ns, path: &syntax::ast::Path, module: ItemId) -> Option<Res> {
        let mut res = None;

        for (i, seg) in path.segs.iter().enumerate() {
            let last = i == path.segs.len() - 1;

            match res {
                None if i == 0 => {
                    res = self
                        .get_seg(
                            if !last { Ns::Modules } else { ns },
                            if path.root { ItemId(0) } else { module },
                            seg,
                        )
                        .0;
                }
                None => return res,
                Some(Res::Module(id)) => {
                    res = self
                        .get_seg(if !last { Ns::Modules } else { ns }, id, seg)
                        .0;
                }
                Some(Res::Item(_)) if last => break,
                Some(Res::Local(_)) if i == 0 => break,
                Some(Res::Label(_)) if i == 0 => break,
                Some(Res::PrimTy(_)) if i == 0 => break,
                Some(Res::Item(_)) => {
                    // TODO: error: {..seg} is not a module
                }
                Some(Res::Local(_)) => {
                    // TODO: error: a local can only be referenced directly
                }
                Some(Res::Label(_)) => {
                    // TODO: error: a label can only be referenced directly
                }
                Some(Res::PrimVal(_)) => unreachable!(),
                Some(Res::PrimTy(_)) => unreachable!(),
            }
        }

        res
    }

    pub fn get_seg(
        &self,
        ns: Ns,
        module: ItemId,
        seg: &syntax::ast::PathSeg,
    ) -> (Option<Res>, bool) {
        let ribs = &self.modules[&module][ns];
        let mut is_local = false;
        let mut res = None;

        for rib in ribs {
            if let Some(r) = rib.get(&seg.name.symbol) {
                res = Some(r);
                is_local = rib.kind == RibKind::Local;
            }
        }

        (res, is_local)
    }

    fn add_root(&mut self) {
        self.add_module(ItemId(0));
        self.set_module(ItemId(0));
        self.define(
            Ns::Values,
            Symbol::new("true"),
            Span::default(),
            Res::PrimVal(PrimVal::True),
        );
        self.define(
            Ns::Values,
            Symbol::new("false"),
            Span::default(),
            Res::PrimVal(PrimVal::False),
        );
        self.define(
            Ns::Types,
            Symbol::new("never"),
            Span::default(),
            Res::PrimTy(PrimTy::Never),
        );
        self.define(
            Ns::Types,
            Symbol::new("bool"),
            Span::default(),
            Res::PrimTy(PrimTy::Bool),
        );
        self.define(
            Ns::Types,
            Symbol::new("str"),
            Span::default(),
            Res::PrimTy(PrimTy::Str),
        );
        self.define(
            Ns::Types,
            Symbol::new("uint"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(255, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("int"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(255, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("float"),
            Span::default(),
            Res::PrimTy(PrimTy::Float(255)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u8"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(8, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u16"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(16, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u32"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(32, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u64"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(64, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("u128"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(128, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("usize"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(0, false)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i8"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(8, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i16"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(16, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i32"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(32, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i64"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(64, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("i128"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(128, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("isize"),
            Span::default(),
            Res::PrimTy(PrimTy::Int(0, true)),
        );
        self.define(
            Ns::Types,
            Symbol::new("f32"),
            Span::default(),
            Res::PrimTy(PrimTy::Float(32)),
        );
        self.define(
            Ns::Types,
            Symbol::new("f64"),
            Span::default(),
            Res::PrimTy(PrimTy::Float(64)),
        );
    }
}

impl Rib {
    pub fn new(kind: RibKind) -> Self {
        Rib {
            names: HashMap::new(),
            kind,
        }
    }

    pub fn insert(&mut self, name: Symbol, res: Res) {
        self.names.insert(name, res);
    }

    pub fn get(&self, name: &Symbol) -> Option<Res> {
        self.names.get(name).copied()
    }

    pub fn contains(&self, name: &Symbol) -> bool {
        self.names.contains_key(name)
    }
}

impl<T> std::ops::Index<Ns> for PerNs<T> {
    type Output = T;

    fn index(&self, ns: Ns) -> &Self::Output {
        match ns {
            Ns::Modules => &self.modules,
            Ns::Values => &self.values,
            Ns::Types => &self.types,
            Ns::Labels => &self.labels,
        }
    }
}

impl<T> std::ops::IndexMut<Ns> for PerNs<T> {
    fn index_mut(&mut self, ns: Ns) -> &mut Self::Output {
        match ns {
            Ns::Modules => &mut self.modules,
            Ns::Values => &mut self.values,
            Ns::Types => &mut self.types,
            Ns::Labels => &mut self.labels,
        }
    }
}

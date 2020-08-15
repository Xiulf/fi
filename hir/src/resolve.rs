use crate::{Id, Symbol};
use diagnostics::{Diagnostic, Reporter, Severity, Span};
use std::collections::HashMap;

pub struct Resolver<'a> {
    reporter: &'a Reporter,
    current_module: Id,
    modules: HashMap<Id, PerNs<Vec<Rib>>>,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Res {
    Module(Id),
    Item(Id),
    Local(Id),
    Err,
}

impl<'a> Resolver<'a> {
    pub fn new(reporter: &'a Reporter) -> Self {
        Resolver {
            reporter,
            current_module: Id(0),
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, id: Id) {
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

    pub fn set_module(&mut self, id: Id) {
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

    pub fn define_super(&mut self, ns: Ns, name: Symbol, res: Res) {
        let module = &mut self.module()[ns];
        let len = module.len();
        let rib = &mut module[len - 2];

        rib.insert(name, res);
    }

    fn check_duplicate(&self, ns: Ns, name: &Symbol, span: Span) -> bool {
        let last_rib = self.modules[&self.current_module][ns].last().unwrap();

        if last_rib.contains(name) {
            self.reporter.add(
                Diagnostic::new(
                    Severity::Error,
                    0002,
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

    pub fn get_path(&self, ns: Ns, path: &syntax::ast::Path) -> Res {
        let mut res = Res::Err;

        for (i, seg) in path.segs.iter().enumerate() {
            let last = i == path.segs.len() - 1;

            match res {
                Res::Err if i == 0 => {
                    res = self
                        .get_seg(
                            if !last { Ns::Modules } else { ns },
                            self.current_module,
                            seg,
                        )
                        .0;
                }
                Res::Err => return res,
                Res::Module(id) => {
                    res = self
                        .get_seg(if !last { Ns::Modules } else { ns }, id, seg)
                        .0;
                }
                Res::Item(_) if last => break,
                Res::Local(_) if i == 0 => break,
                Res::Item(_) => {
                    // TODO: error: {..seg} is not a module
                }
                Res::Local(_) => {
                    // TODO: error: a local can only be referenced directly
                }
            }
        }

        res
    }

    pub fn get_seg(&self, ns: Ns, module: Id, seg: &syntax::ast::PathSeg) -> (Res, bool) {
        let ribs = &self.modules[&module][ns];
        let mut is_local = false;
        let mut res = Res::Err;

        for rib in ribs {
            if let Some(r) = rib.get(&seg.name.symbol) {
                res = r;
                is_local = rib.kind == RibKind::Local;
            }
        }

        (res, is_local)
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

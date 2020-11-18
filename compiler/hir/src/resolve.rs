use crate::ir::{Ident, Res, Symbol};
use std::collections::HashMap;

pub struct Resolver<'db> {
    db: &'db dyn diagnostics::Diagnostics,
    file: source::FileId,
    ribs: PerNs<Vec<Rib>>,
}

pub struct Rib {
    names: HashMap<Symbol, Res>,
}

pub struct PerNs<T> {
    values: T,
    types: T,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ns {
    Values,
    Types,
}

impl<'db> Resolver<'db> {
    pub fn new(db: &'db dyn diagnostics::Diagnostics, file: source::FileId) -> Self {
        Resolver {
            db,
            file,
            ribs: PerNs {
                values: vec![Rib::new()],
                types: vec![Rib::new()],
            },
        }
    }

    pub fn push_rib(&mut self, ns: Ns) {
        self.ribs[ns].push(Rib::new());
    }

    pub fn pop_rib(&mut self, ns: Ns) {
        self.ribs[ns].pop().unwrap();
    }

    pub fn define(&mut self, ns: Ns, ident: Ident, res: Res) {
        if let Some(_) = self.get(ns, ident.symbol) {
            self.db
                .error(format!("Duplicate definition of '{}'", ident))
                .with_label(diagnostics::Label::primary(self.file, ident.span))
                .finish();
        } else {
            self.ribs[ns].last_mut().unwrap().insert(ident.symbol, res);
        }
    }

    pub fn get(&self, ns: Ns, symbol: Symbol) -> Option<Res> {
        self.ribs[ns].iter().filter_map(|r| r.get(symbol)).last()
    }

    pub fn iter<'a>(&'a self, ns: Ns) -> impl Iterator<Item = (Symbol, Res)> + 'a {
        self.ribs[ns]
            .last()
            .unwrap()
            .names
            .iter()
            .map(|(k, v)| (*k, *v))
    }
}

impl Rib {
    pub fn new() -> Self {
        Rib {
            names: HashMap::new(),
        }
    }

    pub fn get(&self, symbol: Symbol) -> Option<Res> {
        self.names.get(&symbol).copied()
    }

    pub fn insert(&mut self, symbol: Symbol, res: Res) {
        self.names.insert(symbol, res);
    }
}

impl<T> std::ops::Index<Ns> for PerNs<T> {
    type Output = T;

    fn index(&self, ns: Ns) -> &Self::Output {
        match ns {
            Ns::Values => &self.values,
            Ns::Types => &self.types,
        }
    }
}

impl<T> std::ops::IndexMut<Ns> for PerNs<T> {
    fn index_mut(&mut self, ns: Ns) -> &mut Self::Output {
        match ns {
            Ns::Values => &mut self.values,
            Ns::Types => &mut self.types,
        }
    }
}

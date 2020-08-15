use crate::*;
use diagnostics::{Diagnostic, Reporter, Severity};
use resolve::{Ns, Resolver, RibKind};
use std::collections::{BTreeMap, HashMap};
use syntax::ast;

pub struct Converter<'a> {
    reporter: &'a Reporter,
    resolver: Resolver<'a>,
    items: BTreeMap<Id, Item>,
    exprs: BTreeMap<Id, Expr>,
    types: BTreeMap<Id, Type>,
}

impl<'a> Converter<'a> {
    pub fn new(reporter: &'a Reporter) -> Self {
        Converter {
            reporter,
            resolver: Resolver::new(reporter),
            items: BTreeMap::new(),
            exprs: BTreeMap::new(),
            types: BTreeMap::new(),
        }
    }

    pub fn finish(self) -> Package {
        Package {
            items: self.items,
            exprs: self.exprs,
            types: self.types,
        }
    }

    pub fn trans_item(&mut self, item: &ast::Item, top_level: bool) -> Id {
        if let ast::ItemKind::Module { module } = &item.kind {
            let items = module
                .items
                .iter()
                .map(|item| self.trans_item(item, true))
                .collect::<Vec<_>>();

            Id::new(&items)
        } else {
            let kind = match &item.kind {};

            let id = Id::new(&kind);

            self.items.insert(
                id,
                Item {
                    span: item.span,
                    id,
                    name: item.name,
                    kind,
                },
            );

            id
        }
    }
}

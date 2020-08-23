use crate::visit::{Visitor, VisitorMut};
use crate::*;
use std::collections::HashMap;

pub fn optimize(package: &mut Package) {
    for (_, item) in &mut package.items {
        if let ItemKind::Body(body) = &mut item.kind {
            let mut usage = Usage::new();

            usage.visit_body(body);

            for (id, count) in usage.0 {
                if count == 0 && body.locals[&id].kind == LocalKind::Tmp {
                    body.locals.remove(&id);
                    Remover(id).visit_body(body);
                }
            }
        }
    }
}

struct Usage(HashMap<LocalId, usize>);

impl Usage {
    fn new() -> Self {
        Usage(HashMap::new())
    }
}

impl<'tcx> Visitor<'tcx> for Usage {
    fn visit_local(&mut self, local: &Local<'tcx>) {
        self.0.insert(local.id, 0);
    }

    fn visit_place(&mut self, place: &Place) {
        if let PlaceBase::Local(id) = &place.base {
            *self.0.get_mut(id).unwrap() += 1;
        }
    }
}

struct Remover(LocalId);

impl<'tcx> VisitorMut<'tcx> for Remover {
    fn visit_block(&mut self, block: &mut Block<'tcx>) {
        block.stmts.drain_filter(|stmt| {
            if let Stmt::Assign(place, _) = stmt {
                if let PlaceBase::Local(id) = &place.base {
                    return *id == self.0;
                }
            }

            false
        });
    }
}

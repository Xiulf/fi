use crate::visit::VisitorMut;
use crate::*;
use std::collections::HashMap;

pub fn optimize(package: &mut Package) {
    VarReplacer { repl: None }.visit_package(package);

    VarRemover {
        count: HashMap::new(),
    }
    .visit_package(package);
}

struct VarRemover {
    count: HashMap<LocalId, usize>,
}

struct VarReplacer {
    repl: Option<(LocalId, LocalId)>,
}

struct Replacer {
    with: HashMap<LocalId, LocalId>,
}

impl<'tcx> VisitorMut<'tcx> for VarRemover {
    fn visit_body(&mut self, body: &mut Body<'tcx>) {
        self.count.clear();
        self.count.insert(LocalId::RET, 1);

        for param in body.params() {
            self.count.insert(param.id, 1);
        }

        self.super_body(body);

        for (id, count) in &self.count {
            if *count == 0 {
                body.locals.remove(id);
            }
        }
    }

    #[inline]
    fn visit_local(&mut self, local: &mut Local<'tcx>) {
        if !self.count.contains_key(&local.id) {
            self.count.insert(local.id, 0);
        }
    }

    #[inline]
    fn visit_place(&mut self, place: &mut Place<'tcx>) {
        if let PlaceBase::Local(id) = &place.base {
            *self.count.get_mut(id).unwrap() += 1;
        }

        self.super_place(place);
    }
}

impl<'tcx> VisitorMut<'tcx> for VarReplacer {
    fn visit_body(&mut self, body: &mut Body<'tcx>) {
        self.super_body(body);

        while let Some((a, b)) = self.repl {
            Replacer {
                with: vec![(a, b)].into_iter().collect(),
            }
            .visit_body(body);

            self.repl = None;
            self.super_body(body);
        }
    }

    fn visit_block(&mut self, block: &mut Block<'tcx>) {
        let mut rem = None;

        for (i, stmt) in block.stmts.iter().enumerate() {
            if let None = self.repl {
                if let Stmt::Assign(place, rvalue) = stmt {
                    if let PlaceBase::Local(a) = &place.base {
                        if place.elems.is_empty() && *a != LocalId::RET {
                            if let RValue::Use(Operand::Place(r)) = rvalue {
                                if let PlaceBase::Local(b) = &r.base {
                                    if r.elems.is_empty() {
                                        self.repl = Some((*b, *a));
                                        rem = Some(i);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if let Some(i) = rem {
            block.stmts.remove(i);
        }
    }
}

impl<'tcx> VisitorMut<'tcx> for Replacer {
    #[inline]
    fn visit_place(&mut self, place: &mut Place<'tcx>) {
        if let PlaceBase::Local(id) = &mut place.base {
            if let Some(new) = self.with.get(id) {
                *id = *new;
            }
        }

        self.super_place(place);
    }
}

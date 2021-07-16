use crate::db::MirDatabase;
use crate::ir::*;
use crate::lower::builder::Builder;
use crate::ty::{Type, TypeVarKind};
use crate::visit::VisitorMut;
use hir::display::HirDisplay;
use hir::ty::TypeVar;
use rustc_hash::FxHashMap;

pub fn postprocess(db: &dyn MirDatabase, bodies: &mut Bodies) {
    let ids = bodies.bodies.iter().map(|(id, _)| id).collect::<Vec<_>>();

    for id in ids {
        process_body(db, bodies.builder(id));
    }

    // eprintln!("{}", bodies.display(db.upcast()));
}

struct PostCtx<'a> {
    db: &'a dyn MirDatabase,
    builder: Builder<'a>,
    type_vars: Vec<Option<Place>>,
    records: Vec<Place>,
}

fn process_body(db: &dyn MirDatabase, builder: Builder) {
    PostCtx {
        db,
        builder,
        type_vars: Vec::new(),
        records: Vec::new(),
    }
    .process();
}

impl<'a> PostCtx<'a> {
    fn process(&mut self) {
        let type_vars = self.builder.body().type_vars.clone();
        let records = self.builder.body().records.clone();

        for type_var in type_vars {
            let place = type_var.map(|type_var| {
                let ty = match type_var {
                    | TypeVarKind::Type => Type::type_info(self.db),
                    | TypeVarKind::Figure => Type::ptr_sized_int(self.db, true),
                    | TypeVarKind::Symbol => Type::str_slice(self.db),
                };

                let arg = self.builder.create_arg(ty);

                Place::new(arg)
            });

            self.type_vars.push(place);
        }

        for record in records {
            let ty = Type::ref_(record.to_type());
            let arg = self.builder.create_arg(ty);

            self.records.push(Place::new(arg));
        }

        let body = self.builder.body_mut() as *mut Body;
        let body = unsafe { &mut *body };

        self.visit_body(body);
    }
}

impl VisitorMut for PostCtx<'_> {
    fn visit_operand(&mut self, operand: &mut Operand) {
        if let Operand::Record(idx, ref field) = *operand {
            let record = &self.builder.body().records[idx];
            let place = self.records[idx].clone().deref();
            let place = record.field(place, field).unwrap();

            *operand = Operand::Place(place);
        }
    }
}

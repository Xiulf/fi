use crate::db::MirDatabase;
use crate::ir::{BodyId, Place};
use crate::ty::Type;
use hir::Name;
use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct InstanceRecord {
    pub items: BTreeMap<Name, Arc<Type>>,
    pub parent: Vec<Arc<InstanceRecord>>,
}

impl InstanceRecord {
    pub(crate) fn instance_record_query(db: &dyn MirDatabase, class: hir::Class) -> Arc<InstanceRecord> {
        let items = class
            .items(db.upcast())
            .into_iter()
            .map(|item| {
                let name = item.name(db.upcast());
                // let def = match item {
                //     | hir::AssocItem::Func(id) => hir::id::FuncId::from(id).into(),
                //     | hir::AssocItem::Static(id) => hir::id::StaticId::from(id).into(),
                // };
                //
                // let bodies = db.body_mir(def);
                // let main_id = bodies.main_id(def);
                let ptr = Type::ptr(Some(1));

                (name, ptr)
            })
            .collect();

        let parent = Vec::new();

        Arc::new(InstanceRecord { items, parent })
    }

    pub fn to_type(&self) -> Arc<Type> {
        Type::and(
            self.items
                .values()
                .cloned()
                .chain(self.parent.iter().map(|ir| Type::ref_(ir.to_type()))),
        )
    }

    pub fn field(&self, base: Place, name: &Name) -> Option<Place> {
        if let Some(idx) = self.items.iter().position(|(n, _)| n == name) {
            Some(base.field(idx))
        } else {
            for (i, parent) in self.parent.iter().enumerate() {
                if let Some(place) = parent.field(base.clone().field(i + self.items.len()), name) {
                    return Some(place);
                }
            }

            None
        }
    }
}

use crate::{AttrKind, Id, Package};

macro_rules! lang_items {
    ($($variant:ident, $name:literal, $method:ident;)*) => {
        #[derive(Debug, Clone, Copy)]
        pub enum LangItem {
            $($variant),*
        }

        pub struct LangItems {
            items: Vec<Option<Id>>,
        }

        impl LangItem {
            fn name(&self) -> &'static str {
                match self {
                    $(LangItem::$variant => $name),*
                }
            }
        }

        impl LangItems {
            pub fn collect(hir: &Package) -> Self {
                let lang_items = hir.items.iter().filter_map(|(id, item)| {
                    if let Some(l) = item.attrs.iter().filter_map(|a| if let AttrKind::Lang(l) = &a.kind { Some(l) } else { None }).next() {
                        Some((*id, l))
                    } else {
                        None
                    }
                }).collect::<Vec<_>>();

                let find_item = |lang_item: LangItem| -> Option<Id> {
                    lang_items
                        .iter()
                        .filter_map(|(id, l)| if &l.text == lang_item.name() {
                            Some(*id)
                        } else {
                            None
                        })
                        .next()
                };

                LangItems {
                    items: vec![$(find_item(LangItem::$variant)),*]
                }
            }

            $(
                #[doc = $name]
                pub fn $method(&self) -> Option<Id> {
                    self.items[LangItem::$variant as usize]
                }
            )*
        }
    };
}

lang_items! {
    TypeLayout, "type-layout", type_layout;
    Range     , "range"      , range      ;
}

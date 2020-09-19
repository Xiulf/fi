use crate::{AttrKind, Attribute, Id, Package};

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
                fn attr_filter(a: &Attribute) -> Option<&syntax::ast::StringLiteral> {
                    if let AttrKind::Lang(l) = &a.kind { Some(l) } else { None }
                }

                let lang_items = hir.items.iter().filter_map(|(id, item)| {
                    if let Some(l) = item.attrs.iter().filter_map(attr_filter).next() {
                        Some((*id, l))
                    } else {
                        None
                    }
                }).chain(hir.imports.0.iter().filter_map(|(id, import)| {
                    if let Some(l) = import.attrs.iter().filter_map(attr_filter).next() {
                        Some((*id, l))
                    } else {
                        None
                    }
                })).collect::<Vec<_>>();

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

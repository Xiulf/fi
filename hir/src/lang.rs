use crate::{Attribute, Id};

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
            fn name(&slef) -> &'static str {
                match self {
                    $($LangItem::$variant => $name),*
                }
            }
        }

        impl LangItems {
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
}

use crate::ir::HirId;

macro_rules! lang_items {
    ($($name:ident, $attr:literal, $variant:ident;)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum LangItem {
            $($variant),*
        }

        #[derive(Debug, PartialEq, Eq)]
        pub struct LangItems {
            ids: [Option<HirId>; lang_items!(@count $($name)*)],
        }

        impl LangItems {
            pub fn collect(db: &dyn crate::HirDatabase) -> std::sync::Arc<Self> {
                let lang_attr = syntax::symbol::Symbol::new("lang");
                let libs = db.libs();
                let find = |name| {
                    libs.iter()
                        .filter_map(|lib| db
                            .lib_files(*lib).iter()
                            .map(|file| db.module_hir(*file))
                            .filter_map(|hir| hir.items
                                .values()
                                .filter_map(|item| {
                                    if let Some(_) = item.attrs.iter()
                                        .find(|attr| attr.name.symbol == lang_attr && attr.str_arg() == Some(name))
                                    {
                                        Some(item.id)
                                    } else {
                                        None
                                    }
                                })
                                .next()
                            )
                            .next()
                        )
                        .next()
                };

                std::sync::Arc::new(LangItems {
                    ids: [$(find($attr)),*],
                })
            }

            pub fn get(&self, item: LangItem) -> Option<HirId> {
                self.ids[item as usize]
            }

            $(
                pub fn $name(&self) -> HirId {
                    self.get(LangItem::$variant).expect(concat!("language item '", $attr, "' not found"))
                }
            )*
        }
    };
    (@count) => {
        0
    };
    (@count $n:ident) => {
        1
    };
    (@count $n:ident $($r:ident)+) => {
        1 + lang_items!(@count $($r)+)
    };
}

lang_items! {
    uint         , "uint"         , Uint        ;
    char         , "char"         , Char        ;
    bool         , "bool"         , Bool        ;
    kind_type    , "type_kind"    , KindType    ;
    kind_figure  , "figure_kind"  , KindFigure  ;
    kind_symbol  , "symbol_kind"  , KindSymbol  ;
    kind_row     , "row_kind"     , KindRow     ;
    type_info    , "type_info"    , TypeInfo    ;
    vwt          , "vwt"          , Vwt         ;
    fn_ty        , "fn_ty"        , FnType      ;
    record_ty    , "record_ty"    , RecordType  ;
    ptr_ty       , "ptr_ty"       , PtrType     ;
    array_ty     , "array_ty"     , ArrayType   ;
    slice_ty     , "slice_ty"     , SliceType   ;
    integer_trait, "integer_trait", IntegerTrait;
    decimal_trait, "decimal_trait", DecimalTrait;
    // basic_copy, "BASIC_COPY", BasicCopy;
    // basic_drop, "BASIC_DROP", BasicDrop;
}

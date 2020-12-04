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
    uint8     , "uint8"     , Uint8    ;
    uint16    , "uint16"    , Uint16   ;
    uint32    , "uint32"    , Uint32   ;
    uint64    , "uint64"    , Uint64   ;
    uint128   , "uint128"   , Uint128  ;
    int8      , "int8"      , Int8     ;
    int16     , "int16"     , Int16    ;
    int32     , "int32"     , Int32    ;
    int64     , "int64"     , Int64    ;
    int128    , "int128"    , Int128   ;
    float32   , "float32"   , Float32  ;
    float64   , "float64"   , Float64  ;
    char      , "char"      , Char     ;
    bool      , "bool"      , Bool     ;
    kind_type , "type_kind" , KindType ;
    kind_int  , "int_kind"  , KindInt  ;
    type_info , "type_info" , TypeInfo ;
    ptr_ty    , "ptr_ty"    , PtrType  ;
    array_ty  , "array_ty"  , ArrayType;
    slice_ty  , "slice_ty"  , SliceType;
    basic_copy, "BASIC_COPY", BasicCopy;
    basic_drop, "BASIC_DROP", BasicDrop;
    __alloc   , "__alloc"   , Alloc    ;
    __free    , "__free"    , Free     ;
}

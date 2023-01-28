use parser::token::SyntaxKind::*;

use crate::ast_node;

ast_node!(SourceFile, SOURCE_FILE);

ast_node!(Item {
    Module(ItemModule, ITEM_MODULE),
    Import(ItemImport, ITEM_IMPORT),
    Fixity(ItemFixity, ITEM_FIXITY),
    Value(ItemValue, ITEM_VALUE),
    Type(ItemType, ITEM_TYPE),
    Trait(ItemTrait, ITEM_TRAIT),
    Impl(ItemImpl, ITEM_IMPL),
});

ast_node!(ImportItems, IMPORT_ITEMS);
ast_node!(ImportItem, IMPORT_ITEM);
ast_node!(ImportHiding, IMPORT_HIDING);

ast_node!(Ctor, CTOR);

ast_node!(Path, PATH);
ast_node!(PathSegment, PATH_SEGMENT);

ast_node!(Name, NAME);
ast_node!(NameRef, NAME_REF);

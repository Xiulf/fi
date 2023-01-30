use parser::token::SyntaxKind::*;

use crate::ast_node;

ast_node!(SourceFile, SOURCE_FILE);

ast_node!(Attr, ATTR);
ast_node!(AttrArgs, ATTR_ARGS);
ast_node!(AttrArg {
    Ident(AttrArgIdent, ATTR_ARG_IDENT),
    Call(AttrArgCall, ATTR_ARG_CALL),
    Equal(AttrArgEqual, ATTR_ARG_EQUAL),
    Literal(AttrArgLiteral, ATTR_ARG_LIT),
});

ast_node!(Item {
    Module(ItemModule, ITEM_MODULE),
    Import(ItemImport, ITEM_IMPORT),
    Fixity(ItemFixity, ITEM_FIXITY),
    Value(ItemValue, ITEM_VALUE),
    Type(ItemType, ITEM_TYPE),
    Trait(ItemTrait, ITEM_TRAIT),
    Impl(ItemImpl, ITEM_IMPL),
});

ast_node!(Exports, EXPORTS);
ast_node!(Export {
    Name(ExportName, EXPORT_NAME),
    Module(ExportModule, EXPORT_MODULE),
});

ast_node!(ImportItems, IMPORT_ITEMS);
ast_node!(ImportItem, IMPORT_ITEM);
ast_node!(ImportHiding, IMPORT_HIDING);

ast_node!(Ctor, CTOR);
ast_node!(CtorRecord, CTOR_RECORD);
ast_node!(CtorField, CTOR_FIELD);

ast_node!(Path, PATH);
ast_node!(PathSegment, PATH_SEGMENT);

ast_node!(Name, NAME);
ast_node!(NameRef, NAME_REF);

ast_node!(Literal {
    Int(LitInt, LIT_INT),
    Float(LitFloat, LIT_FLOAT),
    Char(LitChar, LIT_CHAR),
    String(LitString, LIT_STRING),
});

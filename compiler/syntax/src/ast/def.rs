use crate::ast_node;
use parser::syntax_kind::*;

ast_node!(Module, MODULE);
ast_node!(Exports, EXPORTS);

ast_node!(Export {
    Name(ExportName, EXPORT_NAME),
    Module(ExportModule, EXPORT_MODULE),
});

ast_node!(Attr, ATTR);
ast_node!(AttrArgs, ATTR_ARGS);

ast_node!(AttrArg {
    Ident(AttrArgIdent, IDENT),
    Call(AttrArgCall, ATTR_ARG_CALL),
    Equal(AttrArgEqual, ATTR_ARG_EQUAL),
    Literal(AttrArgLit, ATTR_ARG_LIT),
});

ast_node!(Item {
    Import(ItemImport, IMPORT),
    Fixity(ItemFixity, ITEM_FIXITY),
    Fun(ItemFun, ITEM_FUN),
    Static(ItemStatic, ITEM_STATIC),
    Const(ItemConst, ITEM_CONST),
    Type(ItemType, ITEM_TYPE),
    Class(ItemClass, ITEM_CLASS),
    Instance(ItemInstance, ITEM_INSTANCE),
});

ast_node!(ImportItems, IMPORT_ITEMS);
ast_node!(Ctor, ITEM_CTOR);
ast_node!(Instance, ITEM_INSTANCE);
ast_node!(FunDep, FUN_DEP);

ast_node!(@ AssocItem {
    Fun(ItemFun, ITEM_FUN),
    Static(ItemStatic, ITEM_STATIC),
});

ast_node!(Pat {
    Wildcard(PatWildcard, PAT_WILDCARD),
    Typed(PatTyped, PAT_TYPED),
    App(PatApp, PAT_APP),
    Ctor(PatCtor, PAT_CTOR),
    Bind(PatBind, PAT_BIND),
    Lit(PatLit, PAT_LITERAL),
    Tuple(PatTuple, PAT_TUPLE),
    Parens(PatParens, PAT_PARENS),
    Record(PatRecord, PAT_RECORD),
});

ast_node!(Type {
    Kinded(TypeKinded, TYPE_KINDED),
    Placeholder(TypePlaceholder, TYPE_PLACEHOLDER),
    Figure(TypeFigure, TYPE_FIGURE),
    Symbol(TypeSymbol, TYPE_SYMBOL),
    App(TypeApp, TYPE_APP),
    Path(TypePath, TYPE_PATH),
    Array(TypeArray, TYPE_ARRAY),
    Slice(TypeSlice, TYPE_SLICE),
    Ptr(TypePtr, TYPE_PTR),
    Fn(TypeFn, TYPE_FN),
    Rec(TypeRec, TYPE_REC),
    Row(TypeRow, TYPE_ROW),
    Tuple(TypeTuple, TYPE_TUPLE),
    Parens(TypeParens, TYPE_PARENS),
    For(TypeFor, TYPE_FOR),
    Ctnt(TypeCtnt, TYPE_CTNT),
});

ast_node!(RowField, ROW_FIELD);
ast_node!(RowTail, ROW_TAIL);

ast_node!(Sentinel, SENTINEL);
ast_node!(Generics, GENERICS);
ast_node!(TypeVar, TYPE_VAR);
ast_node!(Constraint, CONSTRAINT);

ast_node!(Stmt {
    Let(StmtLet, STMT_LET),
    Bind(StmtBind, STMT_BIND),
    Expr(StmtExpr, STMT_EXPR),
});

ast_node!(Expr {
    Typed(ExprTyped, EXPR_TYPED),
    Infix(ExprInfix, EXPR_INFIX),
    App(ExprApp, EXPR_APP),
    Field(ExprField, EXPR_FIELD),
    Index(ExprIndex, EXPR_INDEX),
    Path(ExprPath, EXPR_PATH),
    Lit(ExprLit, EXPR_LITERAL),
    Parens(ExprParens, EXPR_PARENS),
    Tuple(ExprTuple, EXPR_TUPLE),
    Record(ExprRecord, EXPR_RECORD),
    Array(ExprArray, EXPR_ARRAY),
    Do(ExprDo, EXPR_DO),
    Clos(ExprClos, EXPR_CLOS),
    If(ExprIf, EXPR_IF),
    Case(ExprCase, EXPR_CASE),
    While(ExprWhile, EXPR_WHILE),
    Loop(ExprLoop, EXPR_LOOP),
    Next(ExprNext, EXPR_NEXT),
    Break(ExprBreak, EXPR_BREAK),
    Yield(ExprYield, EXPR_YIELD),
    Return(ExprReturn, EXPR_RETURN),
});

ast_node!(Block, BLOCK);

ast_node!(Field {
    Normal(FieldNormal, FIELD_NORMAL),
    Pun(FieldPun, FIELD_PUN),
});

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

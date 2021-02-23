use super::*;
use crate::ast_node;
use parser::syntax_kind::*;

ast_node!(Module, MODULE);

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
    Foreign(ItemForeign, ITEM_FOREIGN),
    Fun(ItemFun, ITEM_FUN),
    Static(ItemStatic, ITEM_STATIC),
    Const(ItemConst, ITEM_CONST),
    Type(ItemType, ITEM_TYPE),
    Class(ItemClass, ITEM_CLASS),
    Instance(ItemInstance, ITEM_INSTANCE),
});

ast_node!(ImportItems, IMPORT_ITEMS);
ast_node!(Ctor, ITEM_CTOR);

ast_node!(AssocItem {
    Fun(AssocItemFun, ITEM_FUN),
    Static(AssocItemStatic, ITEM_STATIC),
});

ast_node!(Pat {
    Typed(PatTyped, PAT_TYPED),
    App(PatApp, PAT_APP),
    Ctor(PatCtor, PAT_CTOR),
    Bind(PatBind, PAT_BIND),
    Lit(PatLit, PAT_LITERAL),
});

ast_node!(Type {
    Kinded(TypeKinded, TYPE_KINDED),
    App(TypeApp, TYPE_APP),
    Path(TypePath, TYPE_PATH),
    Array(TypeArray, TYPE_ARRAY),
    Slice(TypeSlice, TYPE_SLICE),
    Ptr(TypePtr, TYPE_PTR),
    Fn(TypeFn, TYPE_FN),
    Rec(TypeRec, TYPE_REC),
    Tuple(TypeTuple, TYPE_TUPLE),
    Parens(TypeParens, TYPE_PARENS),
    For(TypeFor, TYPE_FOR),
});

ast_node!(Sentinel, SENTINEL);
ast_node!(Generics, GENERICS);
ast_node!(TypeVar, TYPE_VAR);
ast_node!(Constraint, CONSTRAINT);

ast_node!(Stmt {
    Let(StmtLet, STMT_LET),
    Expr(StmtExpr, STMT_EXPR),
});

ast_node!(Expr {
    Typed(ExprTyped, EXPR_TYPED),
    Infix(ExprInfix, EXPR_INFIX),
    App(ExprApp, EXPR_APP),
    Field(ExprField, EXPR_FIELD),
    Deref(ExprDeref, EXPR_DEREF),
    Cast(ExprCast, EXPR_CAST),
    Index(ExprIndex, EXPR_INDEX),
    Path(ExprPath, EXPR_PATH),
    Lit(ExprLit, EXPR_LITERAL),
    Parens(ExprParens, EXPR_PARENS),
    Tuple(ExprTuple, EXPR_TUPLE),
    Record(ExprRecord, EXPR_RECORD),
    Array(ExprArray, EXPR_ARRAY),
    Do(ExprDo, EXPR_DO),
    If(ExprIf, EXPR_IF),
});

ast_node!(Block, BLOCK);

ast_node!(Path, PATH);
ast_node!(PathSegment, PATH_SEGMENT);

ast_node!(Name, NAME);
ast_node!(NameRef, NAME_REF);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Int(LitInt),
    Float(LitFloat),
    Char(LitChar),
    String(LitString),
}

ast_node!(LitInt, LIT_INT);
ast_node!(LitFloat, LIT_FLOAT);
ast_node!(LitChar, LIT_CHAR);
ast_node!(LitString, LIT_STRING);

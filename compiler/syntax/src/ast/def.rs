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

ast_node!(TypeVars, TYPE_VARS);

ast_node!(Ctor, CTOR);
ast_node!(CtorRecord, CTOR_RECORD);
ast_node!(CtorField, CTOR_FIELD);

ast_node!(WhereClause, WHERE_CLAUSE);
ast_node!(WhereClauseItem {
    Constraint(WhereClauseConstraint, WHERE_CLAUSE_CONSTRAINT),
    VarKind(WhereClauseVarKind, WHERE_CLAUSE_VAR_KIND),
});

ast_node!(Type {
    Parens(TypeParens, TYPE_PARENS),
    Hole(TypeHole, TYPE_HOLE),
    Unit(TypeUnit, TYPE_UNIT),
    Var(TypeVar, TYPE_VAR),
    Path(TypePath, TYPE_PATH),
    List(TypeList, TYPE_LIST),
    App(TypeApp, TYPE_APP),
    Infix(TypeInfix, TYPE_INFIX),
    Func(TypeFunc, TYPE_FUNC),
});

ast_node!(TypeFuncEnv, TYPE_FUNC_ENV);

ast_node!(Pat {
    Typed(PatTyped, PAT_TYPED),
    Parens(PatParens, PAT_PARENS),
    Unit(PatUnit, PAT_UNIT),
    Wildcard(PatWildcard, PAT_WILDCARD),
    Literal(PatLiteral, PAT_LITERAL),
    Bind(PatBind, PAT_BIND),
    Path(PatPath, PAT_PATH),
    App(PatApp, PAT_APP),
    Infix(PatInfix, PAT_INFIX),
});

ast_node!(Expr {
    Typed(ExprTyped, EXPR_TYPED),
    Parens(ExprParens, EXPR_PARENS),
    Hole(ExprHole, EXPR_HOLE),
    Unit(ExprUnit, EXPR_UNIT),
    Path(ExprPath, EXPR_PATH),
    Literal(ExprLiteral, EXPR_LITERAL),
    Lambda(ExprLambda, EXPR_LAMBDA),
    Infix(ExprInfix, EXPR_INFIX),
    Prefix(ExprPrefix, EXPR_PREFIX),
    Postfix(ExprPostfix, EXPR_POSTFIX),
    App(ExprApp, EXPR_APP),
    Pipe(ExprPipe, EXPR_PIPE),
    Block(ExprBlock, EXPR_BLOCK),
    Do(ExprDo, EXPR_DO),
    If(ExprIf, EXPR_IF),
    Match(ExprMatch, EXPR_MATCH),
    Return(ExprReturn, EXPR_RETURN),
});

ast_node!(MatchArm, MATCH_ARM);
ast_node!(MatchGuard, MATCH_GUARD);

ast_node!(Stmt {
    Let(StmtLet, STMT_LET),
    Bind(StmtBind, STMT_BIND),
    Expr(StmtExpr, STMT_EXPR),
});

ast_node!(Literal {
    Int(LitInt, LIT_INT),
    Float(LitFloat, LIT_FLOAT),
    Char(LitChar, LIT_CHAR),
    String(LitString, LIT_STRING),
});

ast_node!(Path, PATH);
ast_node!(PathSegment, PATH_SEGMENT);

ast_node!(Name, NAME);
ast_node!(NameRef, NAME_REF);

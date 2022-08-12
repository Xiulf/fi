#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // Trivia
    TOMBSTONE,
    ERROR,
    EOF,
    WHITESPACE,
    COMMENT,

    // Tokens
    L_PAREN,
    R_PAREN,
    L_BRACE,
    R_BRACE,
    L_BRACKET,
    R_BRACKET,
    PATH_SEP,
    ARROW,
    LEFT_ARROW,
    DBL_COLON,
    DBL_DOT,
    DOT,
    COMMA,
    COLON,
    PIPE,
    EQUALS,
    AT,
    TICK,
    UNDERSCORE,
    OPERATOR,
    SYMBOL,
    IDENT,
    INT,
    FLOAT,
    STRING,
    CHAR,
    LYT_START,
    LYT_SEP,
    LYT_END,

    // Keywords
    MODULE_KW,
    IMPORT_KW,
    TYPE_KW,
    FOREIGN_KW,
    FN_KW,
    STATIC_KW,
    CONST_KW,
    CLASS_KW,
    MEMBER_KW,
    DERIVE_KW,
    INFIX_KW,
    INFIXL_KW,
    INFIXR_KW,
    POSTFIX_KW,
    PREFIX_KW,
    WHERE_KW,
    FORALL_KW,
    AS_KW,
    DO_KW,
    TRY_KW,
    IF_KW,
    THEN_KW,
    ELSE_KW,
    CASE_KW,
    OF_KW,
    LET_KW,
    RECUR_KW,
    RETURN_KW,

    // Syntax tree
    SOURCE_FILE,
    MODULE,
    EXPORTS,
    EXPORT_NAME,
    EXPORT_MODULE,
    EXPORT_GROUP,
    EXPORT_GROUP_ALL,
    EXPORT_GROUP_NAMED,

    ATTR,
    ATTR_ARG_CALL,
    ATTR_ARG_EQUAL,
    ATTR_ARG_LIT,
    ATTR_ARG_IDENT,
    ATTR_ARGS,

    IMPORT,
    IMPORT_ITEMS,

    ITEM_FIXITY,
    ITEM_FUN,
    ITEM_STATIC,
    ITEM_CONST,
    ITEM_CLASS,
    ITEM_MEMBER,
    ITEM_TYPE,
    ITEM_CTOR,

    TYPE_VARS,
    FUN_DEP,

    WHERE_CLAUSE,
    CONSTRAINT,
    TYPE_VAR_KIND,

    PAT_TYPED,
    PAT_INFIX,
    PAT_APP,
    PAT_CTOR,
    PAT_BIND,
    PAT_LITERAL,
    PAT_WILDCARD,
    PAT_PARENS,
    PAT_UNIT,
    PAT_RECORD,

    FIELD_NORMAL,
    FIELD_PUN,

    TYPE_HOLE,
    TYPE_INFIX,
    TYPE_FIGURE,
    TYPE_SYMBOL,
    TYPE_UNIT,
    TYPE_APP,
    TYPE_PATH,
    TYPE_REC,
    TYPE_ROW,
    TYPE_PARENS,
    TYPE_FORALL,
    TYPE_WHERE,

    ROW_FIELD,
    ROW_TAIL,

    STMT_LET,
    STMT_BIND,
    STMT_EXPR,

    EXPR_TYPED,
    EXPR_HOLE,
    EXPR_ASSIGN,
    EXPR_INFIX,
    EXPR_POSTFIX,
    EXPR_PREFIX,
    EXPR_APP,
    EXPR_FIELD,
    EXPR_METHOD,
    EXPR_IDENT,
    EXPR_LITERAL,
    EXPR_UNIT,
    EXPR_PARENS,
    EXPR_RECORD,
    EXPR_ARRAY,
    EXPR_CLOS,
    EXPR_DO,
    EXPR_TRY,
    EXPR_IF,
    EXPR_CASE,
    EXPR_RECUR,
    EXPR_RETURN,

    CASE_ARM,
    CASE_GUARD,

    LIT_INT,
    LIT_FLOAT,
    LIT_CHAR,
    LIT_STRING,

    BLOCK,

    PATH,
    PATH_SEGMENT,

    NAME,
    NAME_REF,

    __LAST,
}

impl SyntaxKind {
    pub fn is_trivia(self) -> bool {
        match self {
            | WHITESPACE | COMMENT => true,
            | _ => false,
        }
    }
}

pub use SyntaxKind::*;

impl From<u16> for SyntaxKind {
    fn from(src: u16) -> Self {
        assert!(src <= (__LAST as u16));
        unsafe { std::mem::transmute(src) }
    }
}

impl Into<u16> for SyntaxKind {
    fn into(self) -> u16 {
        self as u16
    }
}

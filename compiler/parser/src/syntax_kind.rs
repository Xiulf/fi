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
    FAT_ARROW,
    LEFT_ARROW,
    DBL_COLON,
    DBL_DOT,
    DOT,
    COMMA,
    COLON,
    PIPE,
    EQUALS,
    QMARK,
    AT,
    STAR,
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
    FUN_KW,
    STATIC_KW,
    CONST_KW,
    CLASS_KW,
    INSTANCE_KW,
    INFIX_KW,
    INFIXL_KW,
    INFIXR_KW,
    AS_KW,
    DO_KW,
    IF_KW,
    UNLESS_KW,
    THEN_KW,
    ELSE_KW,
    WHILE_KW,
    UNTIL_KW,
    LOOP_KW,
    BREAK_KW,
    NEXT_KW,
    YIELD_KW,
    RETURN_KW,
    CASE_KW,
    OF_KW,
    LET_KW,
    FOR_KW,

    // Syntax tree
    MODULE,
    EXPORTS,
    EXPORT_NAME,
    EXPORT_MODULE,

    ATTR,
    ATTR_ARG_CALL,
    ATTR_ARG_EQUAL,
    ATTR_ARG_LIT,
    ATTR_ARGS,

    IMPORT,
    IMPORT_ITEMS,

    ITEM_FIXITY,
    ITEM_FUN,
    ITEM_STATIC,
    ITEM_CONST,
    ITEM_CLASS,
    ITEM_INSTANCE,
    ITEM_TYPE,
    ITEM_CTOR,

    FUN_DEP,

    PAT_TYPED,
    PAT_APP,
    PAT_CTOR,
    PAT_BIND,
    PAT_LITERAL,
    PAT_WILDCARD,
    PAT_TUPLE,
    PAT_PARENS,
    PAT_RECORD,

    FIELD_NORMAL,
    FIELD_PUN,

    TYPE_KINDED,
    TYPE_PLACEHOLDER,
    TYPE_FIGURE,
    TYPE_SYMBOL,
    TYPE_APP,
    TYPE_PATH,
    TYPE_ARRAY,
    TYPE_SLICE,
    TYPE_PTR,
    TYPE_FN,
    TYPE_REC,
    TYPE_ROW,
    TYPE_TUPLE,
    TYPE_PARENS,
    TYPE_FOR,
    TYPE_CTNT,

    ROW_FIELD,
    ROW_TAIL,

    SENTINEL,
    GENERICS,
    TYPE_VAR,
    CONSTRAINT,

    STMT_LET,
    STMT_BIND,
    STMT_EXPR,

    EXPR_TYPED,
    EXPR_ASSIGN,
    EXPR_INFIX,
    EXPR_APP,
    EXPR_FIELD,
    EXPR_DEREF,
    EXPR_CAST,
    EXPR_INDEX,
    EXPR_PATH,
    EXPR_LITERAL,
    EXPR_PARENS,
    EXPR_TUPLE,
    EXPR_RECORD,
    EXPR_ARRAY,
    EXPR_CLOS,
    EXPR_DO,
    EXPR_IF,
    EXPR_CASE,
    EXPR_WHILE,
    EXPR_LOOP,
    EXPR_NEXT,
    EXPR_BREAK,
    EXPR_YIELD,
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

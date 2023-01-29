use std::fmt;

use text_size::TextSize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Token {
    pub kind: SyntaxKind,
    pub len: TextSize,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
#[repr(u16)]
pub enum SyntaxKind {
    ERROR,
    // EOF,
    WHITESPACE,
    COMMENT,

    LYT_START,
    LYT_SEP,
    LYT_END,

    IDENT,
    TYPE,
    SYMBOL,
    INT,
    FLOAT,
    CHAR,
    STRING,

    L_PAREN,
    R_PAREN,
    L_BRACE,
    R_BRACE,
    L_BRACKET,
    R_BRACKET,

    DOT,
    COMMA,
    EQUALS,
    UNDERSCORE,
    ARROW,
    LEFT_ARROW,
    DBL_COLON,
    DBL_DOT,
    PIPE,
    AT,
    TICK,

    MODULE_KW,
    IMPORT_KW,
    HIDING_KW,
    INFIXL_KW,
    INFIXR_KW,
    INFIX_KW,
    PREFIX_KW,
    POSTFIX_KW,
    FOREIGN_KW,
    CONST_KW,
    STATIC_KW,
    TYPE_KW,
    TRAIT_KW,
    IMPL_KW,
    AS_KW,
    DO_KW,
    TRY_KW,
    FN_KW,
    IF_KW,
    THEN_KW,
    ELSE_KW,
    MATCH_KW,
    WITH_KW,
    WHERE_KW,
    LET_KW,
    RECUR_KW,
    RETURN_KW,

    SOURCE_FILE,

    ATTR,
    ATTR_ARGS,
    ATTR_ARG_IDENT,
    ATTR_ARG_CALL,
    ATTR_ARG_EQUAL,
    ATTR_ARG_LIT,

    ITEM_MODULE,
    ITEM_IMPORT,
    ITEM_FIXITY,
    ITEM_VALUE,
    ITEM_TYPE,
    ITEM_TRAIT,
    ITEM_IMPL,

    EXPORTS,
    EXPORT_NAME,
    EXPORT_MODULE,

    IMPORT_ITEMS,
    IMPORT_ITEM,
    IMPORT_HIDING,

    CTOR,

    LIT_INT,
    LIT_FLOAT,
    LIT_CHAR,
    LIT_STRING,

    PATH,
    PATH_SEGMENT,

    NAME,
    NAME_REF,

    __LAST,
}

impl SyntaxKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::WHITESPACE | SyntaxKind::COMMENT)
    }
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            | Self::ERROR => "{error}",
            // | Self::EOF => "end of input",
            | Self::WHITESPACE => "whitespace",
            | Self::COMMENT => "comment",
            | Self::LYT_START => "indentation",
            | Self::LYT_SEP => "newline",
            | Self::LYT_END => "unindentation",
            | Self::IDENT => "identifier",
            | Self::SYMBOL => "symbol",
            | Self::INT => "int literal",
            | Self::FLOAT => "float literal",
            | Self::CHAR => "character literal",
            | Self::STRING => "string literal",
            | Self::L_PAREN => "(",
            | Self::R_PAREN => ")",
            | Self::L_BRACE => "{",
            | Self::R_BRACE => "}",
            | Self::L_BRACKET => "[",
            | Self::R_BRACKET => "]",
            | Self::DOT => ".",
            | Self::COMMA => ",",
            | Self::EQUALS => "=",
            | Self::UNDERSCORE => "_",
            | Self::ARROW => "->",
            | Self::LEFT_ARROW => "<-",
            | Self::DBL_COLON => "::",
            | Self::DBL_DOT => "..",
            | Self::PIPE => "|",
            | Self::AT => "@",
            | Self::TICK => "`",
            | Self::MODULE_KW => "'module'",
            | Self::IMPORT_KW => "'import'",
            | Self::HIDING_KW => "'hiding'",
            | Self::INFIXL_KW => "'infixl'",
            | Self::INFIXR_KW => "'infixr'",
            | Self::INFIX_KW => "'infix'",
            | Self::FOREIGN_KW => "'foreign'",
            | Self::CONST_KW => "'const'",
            | Self::STATIC_KW => "'static'",
            | Self::TYPE_KW => "'type'",
            | Self::TRAIT_KW => "'trait'",
            | Self::IMPL_KW => "'impl'",
            | Self::AS_KW => "'as'",
            | Self::DO_KW => "'do'",
            | Self::TRY_KW => "'try'",
            | Self::FN_KW => "'fn'",
            | Self::IF_KW => "'if'",
            | Self::THEN_KW => "'then'",
            | Self::ELSE_KW => "'else'",
            | Self::MATCH_KW => "'match'",
            | Self::WITH_KW => "'with'",
            | Self::WHERE_KW => "'where'",
            | Self::LET_KW => "'let'",
            | Self::RECUR_KW => "'recur'",
            | Self::RETURN_KW => "'return'",
            | Self::__LAST => unreachable!(),
            | _ => return write!(f, "{self:?}"),
        };

        f.write_str(s)
    }
}

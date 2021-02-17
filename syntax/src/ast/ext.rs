use super::*;
use parser::syntax_kind::*;

pub trait AttrsOwner: AstNode {
    fn attrs(&self) -> AstChildren<Attr> {
        support::children(self.syntax())
    }

    fn has_atom_attr(&self, atom: &str) -> bool {
        self.attrs().filter_map(|a| a.as_simple_atom()).any(|x| x == atom)
    }
}

pub trait NameOwner: AstNode {
    fn name(&self) -> Option<Name> {
        support::child(self.syntax())
    }
}

pub trait DocCommentsOwner: AstNode {
    fn doc_comments(&self) -> CommentIter {
        CommentIter {
            iter: self.syntax().children_with_tokens(),
        }
    }

    fn doc_comment_text(&self) -> Option<String> {
        self.doc_comments().doc_comment_text()
    }
}

pub struct CommentIter {
    iter: crate::SyntaxElementChildren,
}

impl CommentIter {
    pub fn from_syntax_node(syntax_node: &SyntaxNode) -> Self {
        CommentIter {
            iter: syntax_node.children_with_tokens(),
        }
    }

    pub fn doc_comment_text(self) -> Option<String> {
        let docs = self.filter_map(|cmt| cmt.doc_comment().map(ToOwned::to_owned)).collect::<Vec<_>>().join("\n");

        if docs.is_empty() {
            None
        } else {
            Some(docs)
        }
    }
}

impl Iterator for CommentIter {
    type Item = Comment;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.by_ref().find_map(|el| el.into_token().and_then(Comment::cast))
    }
}

impl SourceFile {
    pub fn imports(&self) -> AstChildren<Import> {
        support::children(&self.0)
    }

    pub fn items(&self) -> AstChildren<Item> {
        support::children(&self.0)
    }
}

impl Attr {
    pub fn as_simple_atom(&self) -> Option<smol_str::SmolStr> {
        self.name()
    }

    pub fn name(&self) -> Option<smol_str::SmolStr> {
        support::token(&self.0, IDENT).map(|t| t.text().into())
    }
}

impl AttrsOwner for Import {
}

impl Import {
    pub fn path(&self) -> Option<Path> {
        support::child(&self.0)
    }

    pub fn items(&self) -> Option<ImportItems> {
        support::child(&self.0)
    }
}

impl ImportItems {
    pub fn import_all(&self) -> bool {
        support::token(&self.0, STAR).is_some()
    }
}

impl IntoIterator for ImportItems {
    type IntoIter = AstChildren<NameRef>;
    type Item = NameRef;

    fn into_iter(self) -> Self::IntoIter {
        support::children(&self.0)
    }
}

impl AttrsOwner for Item {
}

impl AttrsOwner for ItemFixity {
}

impl NameOwner for ItemFixity {
}

impl ItemFixity {
    pub fn func(&self) -> Option<NameRef> {
        support::child(&self.0)
    }

    pub fn assoc(&self) -> Option<Assoc> {
        support::token(&self.0, INFIX_KW).map_or_else(
            || support::token(&self.0, INFIXL_KW).map_or_else(|| support::token(&self.0, INFIXR_KW).map(|_| Assoc::Right), |_| Some(Assoc::Left)),
            |_| Some(Assoc::None),
        )
    }

    pub fn prec(&self) -> Option<Prec> {
        let int = support::token(&self.0, INT)?;

        match int.text() {
            | "0" => Some(Prec::Zero),
            | "1" => Some(Prec::One),
            | "2" => Some(Prec::Two),
            | "3" => Some(Prec::Three),
            | "4" => Some(Prec::Four),
            | "5" => Some(Prec::Five),
            | "6" => Some(Prec::Six),
            | "7" => Some(Prec::Seven),
            | "8" => Some(Prec::Eight),
            | "9" => Some(Prec::Nine),
            | _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Assoc {
    None,
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prec {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
}

impl AttrsOwner for ItemForeign {
}

impl NameOwner for ItemForeign {
}

impl ItemForeign {
    pub fn kind(&self) -> Option<ForeignKind> {
        support::token(&self.0, DEF_KW).map_or_else(|| support::token(&self.0, STATIC_KW).map(|_| ForeignKind::Static), |_| Some(ForeignKind::Def))
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ForeignKind {
    Def,
    Static,
}

impl AttrsOwner for ItemDef {
}

impl NameOwner for ItemDef {
}

impl ItemDef {
    pub fn args(&self) -> AstChildren<Pat> {
        support::children(&self.0)
    }

    pub fn body(&self) -> Option<Block> {
        support::child(&self.0)
    }
}

impl AttrsOwner for ItemStatic {
}

impl NameOwner for ItemStatic {
}

impl ItemStatic {
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

impl AttrsOwner for ItemConst {
}

impl NameOwner for ItemConst {
}

impl ItemConst {
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

impl AttrsOwner for ItemType {
}

impl NameOwner for ItemType {
}

impl ItemType {
    pub fn vars(&self) -> AstChildren<TypeVar> {
        support::children(&self.0)
    }

    pub fn alias(&self) -> Option<Type> {
        support::child(&self.0)
    }

    pub fn ctors(&self) -> AstChildren<Ctor> {
        support::children(&self.0)
    }
}

impl AttrsOwner for Ctor {
}

impl NameOwner for Ctor {
}

impl Ctor {
    pub fn types(&self) -> AstChildren<Type> {
        support::children(&self.0)
    }
}

impl Path {
    pub fn segments(&self) -> AstChildren<PathSegment> {
        support::children(&self.0)
    }
}

impl PathSegment {
    pub fn name_ref(&self) -> Option<NameRef> {
        support::child(&self.0)
    }
}

impl Name {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, IDENT)
    }
}

impl NameRef {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, IDENT)
    }
}

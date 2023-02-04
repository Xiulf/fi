use cstree::interning::Resolver;
use parser::token::SyntaxKind;

use crate::ast::AstToken;
use crate::syntax_node::SyntaxToken;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Whitespace(pub(crate) SyntaxToken);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment(pub(crate) SyntaxToken);

pub enum CommentKind {
    Doc,
    Normal,
}

impl Comment {
    pub fn kind(&self, resolver: &dyn Resolver) -> CommentKind {
        CommentKind::from_text(self.text(resolver))
    }

    pub fn is_doc(&self, resolver: &dyn Resolver) -> bool {
        matches!(self.kind(resolver), CommentKind::Doc)
    }

    pub fn doc_comment<'a>(&self, resolver: &'a dyn Resolver) -> Option<&'a str> {
        let kind = self.kind(resolver);

        match kind {
            | CommentKind::Doc => {
                let text = &self.text(resolver)[2..];
                let ws = text.chars().next().filter(|c| c.is_whitespace());
                let text = ws.map_or(text, |ws| &text[ws.len_utf8()..]);

                Some(text)
            },
            | CommentKind::Normal => None,
        }
    }
}

impl CommentKind {
    pub fn from_text(text: &str) -> Self {
        if text.starts_with(";;") {
            CommentKind::Doc
        } else {
            CommentKind::Normal
        }
    }
}

impl AstToken for Whitespace {
    fn can_cast(token: SyntaxKind) -> bool {
        token == SyntaxKind::WHITESPACE
    }

    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if Self::can_cast(token.kind()) {
            Some(Whitespace(token))
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.0
    }

    fn text<'a>(&self, resolver: &'a dyn Resolver) -> &'a str {
        self.0.resolve_text(resolver)
    }
}

impl AstToken for Comment {
    fn can_cast(token: SyntaxKind) -> bool {
        token == SyntaxKind::COMMENT
    }

    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        if Self::can_cast(token.kind()) {
            Some(Comment(token))
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.0
    }

    fn text<'a>(&self, resolver: &'a dyn Resolver) -> &'a str {
        self.0.resolve_text(resolver)
    }
}

use crate::ast::AstToken;
use crate::syntax_node::SyntaxToken;
use parser::syntax_kind::SyntaxKind;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Whitespace(crate SyntaxToken);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment(crate SyntaxToken);

pub enum CommentKind {
    Doc,
    Normal,
}

impl Comment {
    pub fn kind(&self) -> CommentKind {
        CommentKind::from_text(self.text())
    }

    pub fn doc_comment(&self) -> Option<&str> {
        let kind = self.kind();

        match kind {
            | CommentKind::Doc => {
                let text = &self.text()[3..];
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
        if text.starts_with("--|") {
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

    fn text(&self) -> &str {
        self.0.text()
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

    fn text(&self) -> &str {
        self.0.text()
    }
}

impl fmt::Display for Whitespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

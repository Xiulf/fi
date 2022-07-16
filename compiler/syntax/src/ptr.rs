use std::hash::{Hash, Hasher};
use std::iter::successors;
use std::marker::PhantomData;

use crate::{AstNode, SyntaxKind, SyntaxNode, TextRange};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr {
    pub(crate) range: TextRange,
    kind: SyntaxKind,
}

#[derive(Debug)]
pub struct AstPtr<N: AstNode> {
    raw: SyntaxNodePtr,
    _marker: PhantomData<fn() -> N>,
}

impl SyntaxNodePtr {
    pub fn new(node: &SyntaxNode) -> Self {
        SyntaxNodePtr {
            range: node.text_range(),
            kind: node.kind(),
        }
    }

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn to_node(&self, root: &SyntaxNode) -> SyntaxNode {
        successors(Some(root.clone()), |node| {
            node.children().find(|it| it.text_range().contains_range(self.range))
        })
        .find(|it| it.text_range() == self.range && it.kind() == self.kind)
        .unwrap_or_else(|| panic!("can't resolve local ptr to SyntaxNode: {:?}", self))
    }

    pub fn cast<N: AstNode>(self) -> Option<AstPtr<N>> {
        if N::can_cast(self.kind) {
            Some(AstPtr {
                raw: self,
                _marker: PhantomData,
            })
        } else {
            None
        }
    }
}

impl<N: AstNode> AstPtr<N> {
    pub fn new(node: &N) -> Self {
        AstPtr {
            raw: SyntaxNodePtr::new(node.syntax()),
            _marker: PhantomData,
        }
    }

    pub fn to_node(&self, root: &SyntaxNode) -> N {
        let syntax_node = self.raw.to_node(root);

        N::cast(syntax_node).unwrap()
    }

    pub fn syntax_node_ptr(&self) -> SyntaxNodePtr {
        self.raw.clone()
    }

    pub fn cast<U: AstNode>(self) -> Option<AstPtr<U>> {
        if U::can_cast(self.raw.kind) {
            Some(AstPtr {
                raw: self.raw,
                _marker: PhantomData,
            })
        } else {
            None
        }
    }
}

impl<N: AstNode> Clone for AstPtr<N> {
    fn clone(&self) -> Self {
        AstPtr {
            raw: self.raw.clone(),
            _marker: PhantomData,
        }
    }
}

impl<N: AstNode> PartialEq for AstPtr<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> Eq for AstPtr<N> {
}

impl<N: AstNode> Hash for AstPtr<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

impl<N: AstNode> From<AstPtr<N>> for SyntaxNodePtr {
    fn from(ptr: AstPtr<N>) -> Self {
        ptr.raw
    }
}

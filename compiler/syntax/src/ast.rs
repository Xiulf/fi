mod def;
mod ext;
mod tokens;

use cstree::interning::Resolver;
pub use def::*;
pub use ext::*;
use parser::token::SyntaxKind;
pub use tokens::*;

use crate::{SyntaxNode, SyntaxToken};

pub trait AstNode {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: &SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

pub trait AstToken {
    fn can_cast(kind: SyntaxKind) -> bool;

    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text<'r>(&self, resolver: &'r dyn Resolver) -> &'r str {
        self.syntax().resolve_text(resolver)
    }
}

#[macro_export]
macro_rules! ast_node {
    ($name:ident, $($kind:ident)|*) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $name(pub(crate) $crate::syntax_node::SyntaxNode);

        impl $crate::ast::AstNode for $name {
            #[inline]
            fn can_cast(kind: ::parser::token::SyntaxKind) -> bool {
                matches!(kind, $($kind)|*)
            }

            #[inline]
            fn cast(syntax: &$crate::syntax_node::SyntaxNode) -> Option<Self> {
                if <Self as $crate::ast::AstNode>::can_cast(syntax.kind()) {
                    Some(Self(syntax.clone()))
                } else {
                    None
                }
            }

            #[inline]
            fn syntax(&self) -> &$crate::syntax_node::SyntaxNode {
                &self.0
            }
        }
    };

    ($name:ident { $($var:ident($varname:ident, $varkind:ident)),* $(,)? }) => {
        $crate::ast_node!(@ $name { $($var($varname, $varkind)),* });
        $($crate::ast_node!($varname, $varkind);)*
    };

    (@ $name:ident { $($var:ident($varname:ident, $varkind:ident)),* $(,)? }) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum $name {
            $($var($varname)),*
        }

        impl $crate::ast::AstNode for $name {
            #[inline]
            fn can_cast(kind: ::parser::token::SyntaxKind) -> bool {
                matches!(kind, $($varkind)|*)
            }

            fn cast(syntax: &$crate::syntax_node::SyntaxNode) -> Option<Self> {
                match syntax.kind() {
                    $(
                        | $varkind => Some(Self::$var($varname(syntax.clone()))),
                    )*
                    | _ => None,
                }
            }

            fn syntax(&self) -> &$crate::syntax_node::SyntaxNode {
                match self {
                    $(
                        Self::$var(n) => n.syntax(),
                    )*
                }
            }
        }

        $(
            impl From<$varname> for $name {
                fn from(src: $varname) -> Self {
                    Self::$var(src)
                }
            }
        )*
    };
}

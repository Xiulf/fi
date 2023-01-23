use super::*;

fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
    parent.children().find_map(N::cast)
}

fn children<'a, N: AstNode + 'a>(parent: &'a SyntaxNode) -> impl Iterator<Item = N> + 'a {
    parent.children().filter_map(N::cast)
}

fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<&SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == kind)
}

pub trait NameOwner: AstNode {
    fn name(&self) -> Option<Name>;
}

impl SourceFile {
    pub fn module(&self) -> Option<ItemModule> {
        child(self.syntax())
    }
}

impl NameOwner for ItemModule {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl ItemModule {
    pub fn items(&self) -> impl Iterator<Item = Item> + '_ {
        children(self.syntax())
    }
}

impl Name {
    pub fn ident_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::IDENT)
    }

    pub fn text<'a>(&self, resolver: &'a dyn Resolver) -> &'a str {
        self.ident_token().unwrap().resolve_text(resolver)
    }
}

impl NameRef {
    pub fn ident_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::IDENT)
    }

    pub fn text<'a>(&self, resolver: &'a dyn Resolver) -> &'a str {
        self.ident_token().unwrap().resolve_text(resolver)
    }
}

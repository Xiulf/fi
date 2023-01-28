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

impl ItemModule {
    pub fn name(&self) -> Option<Path> {
        child(self.syntax())
    }

    pub fn items(&self) -> impl Iterator<Item = Item> + '_ {
        children(self.syntax())
    }
}

impl ItemImport {
    pub fn module(&self) -> Option<Path> {
        child(self.syntax())
    }

    pub fn rename(&self) -> Option<Name> {
        child(self.syntax())
    }

    pub fn items(&self) -> Option<ImportItems> {
        child(self.syntax())
    }

    pub fn hiding(&self) -> Option<ImportHiding> {
        child(self.syntax())
    }
}

impl ImportItems {
    pub fn iter(&self) -> impl Iterator<Item = ImportItem> + '_ {
        children(self.syntax())
    }
}

impl ImportItem {
    pub fn name_ref(&self) -> Option<NameRef> {
        child(self.syntax())
    }

    pub fn rename(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl ImportHiding {
    pub fn iter(&self) -> impl Iterator<Item = NameRef> + '_ {
        children(self.syntax())
    }
}

impl NameOwner for ItemValue {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl Path {
    pub fn segments(&self) -> impl Iterator<Item = PathSegment> + '_ {
        children(self.syntax())
    }
}

impl PathSegment {
    pub fn text<'a>(&self, resolver: &'a dyn Resolver) -> &'a str {
        self.syntax().first_token().unwrap().resolve_text(resolver)
    }
}

impl Name {
    pub fn ident_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::IDENT)
    }

    pub fn symbol_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::SYMBOL)
    }

    pub fn text<'a>(&self, resolver: &'a dyn Resolver) -> &'a str {
        self.syntax().first_token().unwrap().resolve_text(resolver)
    }
}

impl NameRef {
    pub fn ident_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::IDENT)
    }

    pub fn symbol_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::SYMBOL)
    }

    pub fn text<'a>(&self, resolver: &'a dyn Resolver) -> &'a str {
        self.syntax().first_token().unwrap().resolve_text(resolver)
    }
}

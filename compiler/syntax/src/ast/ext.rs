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

impl Exports {
    pub fn iter(&self) -> impl Iterator<Item = Export> + '_ {
        children(self.syntax())
    }
}

impl ExportName {
    pub fn name_ref(&self) -> Option<NameRef> {
        child(self.syntax())
    }
}

impl ExportModule {
    pub fn name_ref(&self) -> Option<NameRef> {
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

    pub fn exports(&self) -> Option<Exports> {
        child(self.syntax())
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

impl NameOwner for ItemFixity {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl ItemFixity {
    pub fn value(&self) -> Option<Path> {
        child(self.syntax())
    }

    pub fn is_type(&self) -> bool {
        token(self.syntax(), SyntaxKind::TYPE_KW).is_some()
    }

    pub fn is_prefix(&self) -> bool {
        token(self.syntax(), SyntaxKind::PREFIX_KW).is_some()
    }

    pub fn is_postfix(&self) -> bool {
        token(self.syntax(), SyntaxKind::POSTFIX_KW).is_some()
    }

    pub fn assoc(&self) -> Option<Assoc> {
        token(self.syntax(), SyntaxKind::INFIX_KW).map_or_else(
            || {
                token(self.syntax(), SyntaxKind::INFIXL_KW).map_or_else(
                    || token(self.syntax(), SyntaxKind::INFIXR_KW).map(|_| Assoc::Right),
                    |_| Some(Assoc::Left),
                )
            },
            |_| Some(Assoc::None),
        )
    }

    pub fn prec(&self, resolver: &dyn Resolver) -> Option<Prec> {
        let int = token(self.syntax(), SyntaxKind::INT)?;
        let text = int.resolve_text(resolver);

        text.parse().map(Prec).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Assoc {
    None,
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prec(usize);

impl NameOwner for ItemValue {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl NameOwner for ItemType {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl ItemType {
    pub fn ctors(&self) -> impl Iterator<Item = Ctor> + '_ {
        children(self.syntax())
    }
}

impl NameOwner for Ctor {
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
    pub fn name_ref(&self) -> Option<NameRef> {
        child(self.syntax())
    }
}

impl Name {
    pub fn ident_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::IDENT).or_else(|| token(self.syntax(), SyntaxKind::TYPE))
    }

    pub fn symbol_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::SYMBOL)
    }
}

impl NameRef {
    pub fn ident_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::IDENT).or_else(|| token(self.syntax(), SyntaxKind::TYPE))
    }

    pub fn symbol_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::SYMBOL)
    }
}

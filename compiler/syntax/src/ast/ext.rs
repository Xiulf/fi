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
        let docs = self
            .filter_map(|cmt| cmt.doc_comment().map(ToOwned::to_owned))
            .collect::<Vec<_>>()
            .join("\n");

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
        self.iter
            .by_ref()
            .find_map(|el| el.into_token().and_then(Comment::cast))
    }
}

impl AttrsOwner for Module {
}

impl NameOwner for Module {
}

impl Module {
    pub fn exports(&self) -> Option<Exports> {
        support::child(&self.0)
    }

    pub fn imports(&self) -> AstChildren<ItemImport> {
        support::children(&self.0)
    }

    pub fn items(&self) -> AstChildren<Item> {
        support::children(&self.0)
    }
}

impl IntoIterator for Exports {
    type IntoIter = AstChildren<Export>;
    type Item = Export;

    fn into_iter(self) -> Self::IntoIter {
        support::children(&self.0)
    }
}

impl Export {
    pub fn name_ref(&self) -> Option<NameRef> {
        match self {
            | Export::Name(e) => e.name_ref(),
            | Export::Module(e) => e.name_ref(),
        }
    }
}

impl ExportName {
    pub fn name_ref(&self) -> Option<NameRef> {
        support::child(&self.0)
    }
}

impl ExportModule {
    pub fn name_ref(&self) -> Option<NameRef> {
        support::child(&self.0)
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

impl AttrsOwner for ItemImport {
}

impl ItemImport {
    pub fn path(&self) -> Option<Path> {
        support::child(&self.0)
    }

    pub fn qualify(&self) -> Option<Name> {
        support::child(&self.0)
    }

    pub fn items(&self) -> Option<ImportItems> {
        support::child(&self.0)
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
    pub fn func(&self) -> Option<Path> {
        support::child(&self.0)
    }

    pub fn assoc(&self) -> Option<Assoc> {
        support::token(&self.0, INFIX_KW).map_or_else(
            || {
                support::token(&self.0, INFIXL_KW).map_or_else(
                    || support::token(&self.0, INFIXR_KW).map(|_| Assoc::Right),
                    |_| Some(Assoc::Left),
                )
            },
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

impl AttrsOwner for ItemFun {
}

impl NameOwner for ItemFun {
}

impl ItemFun {
    pub fn args(&self) -> AstChildren<Pat> {
        support::children(&self.0)
    }

    pub fn body(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.0)
    }

    pub fn is_foreign(&self) -> bool {
        support::token(&self.0, FOREIGN_KW).is_some()
    }

    pub fn generics(&self) -> Option<Generics> {
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

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.0)
    }

    pub fn is_foreign(&self) -> bool {
        support::token(&self.0, FOREIGN_KW).is_some()
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

    pub fn kind(&self) -> Option<Type> {
        support::token(&self.0, DBL_COLON)?;
        support::child(&self.0)
    }

    pub fn alias(&self) -> Option<Type> {
        support::token(&self.0, EQUALS)?;
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

impl AttrsOwner for ItemClass {
}

impl NameOwner for ItemClass {
}

impl ItemClass {
    pub fn fundeps(&self) -> AstChildren<FunDep> {
        support::children(&self.0)
    }

    pub fn constraints(&self) -> AstChildren<Constraint> {
        support::children(&self.0)
    }

    pub fn items(&self) -> AstChildren<AssocItem> {
        support::children(&self.0)
    }
}

impl FunDep {
    pub fn determiners(&self) -> impl Iterator<Item = NameRef> {
        let mut end = false;

        self.0.children_with_tokens().filter_map(move |el| {
            if el.kind() == ARROW {
                end = true;
            }

            if end {
                return None;
            }

            NameRef::cast(el.into_node()?)
        })
    }

    pub fn determined(&self) -> impl Iterator<Item = NameRef> {
        let mut start = false;

        self.0.children_with_tokens().filter_map(move |el| {
            if el.kind() == ARROW {
                start = true;
            }

            if !start {
                return None;
            }

            NameRef::cast(el.into_node()?)
        })
    }
}

// impl AttrsOwner for ItemInstanceChain {
// }
//
// impl ItemInstanceChain {
//     pub fn instances(&self) -> AstChildren<Instance> {
//         support::children(&self.0)
//     }
// }

impl AttrsOwner for ItemInstance {
}

impl ItemInstance {
    pub fn class(&self) -> Option<Path> {
        support::child(&self.0)
    }

    pub fn types(&self) -> AstChildren<Type> {
        support::children(&self.0)
    }

    pub fn constraints(&self) -> AstChildren<Constraint> {
        support::children(&self.0)
    }

    pub fn items(&self) -> AstChildren<AssocItem> {
        support::children(&self.0)
    }
}

impl TypeKinded {
    pub fn ty(&self) -> Option<Type> {
        support::children(&self.0).nth(0)
    }

    pub fn kind(&self) -> Option<Type> {
        support::children(&self.0).nth(1)
    }
}

impl TypeApp {
    pub fn base(&self) -> Option<Type> {
        support::children(&self.0).nth(0)
    }

    pub fn arg(&self) -> Option<Type> {
        support::children(&self.0).nth(1)
    }
}

impl TypePath {
    pub fn path(&self) -> Option<Path> {
        support::child(&self.0)
    }
}

impl TypeArray {
    pub fn elem(&self) -> Option<Type> {
        support::child(&self.0)
    }

    pub fn len(&self) -> Option<usize> {
        let int = support::token(&self.0, INT)?;
        let text = int.text();

        text.parse().ok()
    }
}

impl TypeSlice {
    pub fn elem(&self) -> Option<Type> {
        support::child(&self.0)
    }
}

impl TypePtr {
    pub fn is_buf_ptr(&self) -> bool {
        support::token(&self.0, L_BRACKET).is_some()
    }

    pub fn elem(&self) -> Option<Type> {
        support::child(&self.0)
    }

    pub fn sentinel(&self) -> Option<Sentinel> {
        support::child(&self.0)
    }
}

impl TypeFn {
    pub fn param(&self) -> Option<Type> {
        support::children(&self.0).nth(0)
    }

    pub fn ret(&self) -> Option<Type> {
        support::children(&self.0).nth(1)
    }
}

impl TypeRec {
    // pub fn fields(&self) -> AstChildren<
}

impl TypeTuple {
    pub fn types(&self) -> AstChildren<Type> {
        support::children(&self.0)
    }
}

impl TypeParens {
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.0)
    }
}

impl TypeFor {
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.0)
    }

    pub fn vars(&self) -> AstChildren<TypeVar> {
        support::children(&self.0)
    }
}

impl TypeCtnt {
    pub fn ctnt(&self) -> Option<Constraint> {
        support::child(&self.0)
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.0)
    }
}

impl Sentinel {
    pub fn value(&self) -> i128 {
        let int = support::token(&self.0, INT).unwrap();
        let text = int.text();

        text.parse().unwrap()
    }
}

impl Generics {
    pub fn vars(&self) -> AstChildren<TypeVar> {
        support::children(&self.0)
    }

    pub fn constraints(&self) -> AstChildren<Constraint> {
        support::children(&self.0)
    }
}

impl NameOwner for TypeVar {
}

impl TypeVar {
    pub fn kind(&self) -> Option<Type> {
        support::child(&self.0)
    }
}

impl Constraint {
    pub fn class(&self) -> Option<Path> {
        support::child(&self.0)
    }

    pub fn types(&self) -> AstChildren<Type> {
        support::children(&self.0)
    }
}

impl PatBind {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.0)
    }

    pub fn subpat(&self) -> Option<Pat> {
        support::child(&self.0)
    }
}

impl ExprApp {
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    pub fn arg(&self) -> Option<Expr> {
        support::children(&self.0).nth(1)
    }
}

impl ExprDeref {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

impl ExprPath {
    pub fn path(&self) -> Option<Path> {
        support::child(&self.0)
    }
}

impl ExprLit {
    pub fn literal(&self) -> Option<Literal> {
        support::child(&self.0)
    }
}

impl ExprParens {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

impl ExprDo {
    pub fn block(&self) -> Option<Block> {
        support::child(&self.0)
    }
}

impl Block {
    pub fn statements(&self) -> AstChildren<Stmt> {
        support::children(&self.0)
    }
}

impl ExprIf {
    pub fn cond(&self) -> Option<Expr> {
        support::child(&self.0)
    }

    pub fn then(&self) -> Option<Expr> {
        support::children(&self.0).nth(1)
    }

    pub fn else_(&self) -> Option<Expr> {
        support::children(&self.0).nth(2)
    }

    pub fn is_unless(&self) -> bool {
        support::token(&self.0, UNLESS_KW).is_some()
    }
}

impl StmtLet {
    pub fn pat(&self) -> Option<Pat> {
        support::child(&self.0)
    }

    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

impl StmtBind {
    pub fn pat(&self) -> Option<Pat> {
        support::child(&self.0)
    }

    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
    }
}

impl StmtExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.0)
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

    pub fn text(&self) -> &str {
        self.0
            .green()
            .children()
            .next()
            .and_then(|it| it.into_token())
            .unwrap()
            .text()
    }
}

impl NameRef {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.0, IDENT)
    }

    pub fn text(&self) -> &str {
        self.0
            .green()
            .children()
            .next()
            .and_then(|it| it.into_token())
            .unwrap()
            .text()
    }

    pub fn as_tuple_field(&self) -> Option<usize> {
        self.text().parse().ok()
    }
}

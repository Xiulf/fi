use std::marker::PhantomData;

use cstree::SyntaxNodeChildren;

use super::*;

fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
    parent.children().find_map(N::cast)
}

fn children<'a, N: AstNode + 'a>(parent: &'a SyntaxNode) -> AstChildren<'a, N> {
    AstChildren {
        iter: parent.children(),
        _marker: PhantomData,
    }
}

fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<&SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == kind)
}

fn token2<'a>(parent: &'a SyntaxNode, kinds: &[SyntaxKind]) -> Option<&'a SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| kinds.contains(&it.kind()))
}

pub struct AstChildren<'a, T> {
    iter: SyntaxNodeChildren<'a, crate::syntax_node::Lang>,
    _marker: PhantomData<T>,
}

impl<'a, T: AstNode> Iterator for AstChildren<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.by_ref().find_map(T::cast)
    }
}

pub trait AttrsOwner: AstNode {
    fn attrs(&self) -> AstChildren<Attr> {
        children(self.syntax())
    }
}

// pub trait DocCommentsOwner: AstNode {
//     fn doc_comments(&self) -> CommentIter {
//         CommentIter {
//             iter: self.syntax().children_with_tokens(),
//         }
//     }

//     fn doc_comment_text(&self) -> Option<String> {
//         self.doc_comments().doc_comment_text()
//     }
// }

// pub struct CommentIter {
//     iter: crate::SyntaxElementChildren,
// }

// impl CommentIter {
//     pub fn from_syntax_node(syntax_node: &SyntaxNode) -> Self {
//         CommentIter {
//             iter: syntax_node.children_with_tokens(),
//         }
//     }

//     pub fn doc_comment_text(self) -> Option<String> {
//         let docs = self
//             .filter_map(|cmt| cmt.doc_comment().map(ToOwned::to_owned))
//             .collect::<Vec<_>>()
//             .join("\n");

//         if docs.is_empty() {
//             None
//         } else {
//             Some(docs)
//         }
//     }
// }

// impl Iterator for CommentIter {
//     type Item = Comment;

//     fn next(&mut self) -> Option<Self::Item> {
//         self.iter
//             .by_ref()
//             .find_map(|el| el.into_token().and_then(Comment::cast))
//     }
// }

pub trait NameOwner: AstNode {
    fn name(&self) -> Option<Name>;
}

impl SourceFile {
    pub fn module(&self) -> Option<ItemModule> {
        child(self.syntax())
    }
}

impl Attr {
    pub fn name(&self) -> Option<NameRef> {
        child(self.syntax())
    }

    pub fn value(&self) -> Option<Literal> {
        child(self.syntax())
    }

    pub fn args(&self) -> Option<AttrArgs> {
        child(self.syntax())
    }
}

impl AttrArgs {
    pub fn iter(&self) -> impl Iterator<Item = AttrArg> + '_ {
        children(self.syntax())
    }
}

impl AttrArgLiteral {
    pub fn literal(&self) -> Option<Literal> {
        child(self.syntax())
    }
}

impl AttrArgIdent {
    pub fn name_ref(&self) -> Option<NameRef> {
        child(self.syntax())
    }
}

impl AttrArgEqual {
    pub fn name_ref(&self) -> Option<NameRef> {
        child(self.syntax())
    }

    pub fn literal(&self) -> Option<Literal> {
        child(self.syntax())
    }
}

impl AttrArgCall {
    pub fn name_ref(&self) -> Option<NameRef> {
        child(self.syntax())
    }

    pub fn args(&self) -> Option<AttrArgs> {
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

impl AttrsOwner for Item {
}

impl AttrsOwner for ItemModule {
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

impl AttrsOwner for ItemImport {
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

impl AttrsOwner for ItemFixity {
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

impl Prec {
    pub const ZERO: Self = Self(0);
}

impl AttrsOwner for ItemValue {
}

impl NameOwner for ItemValue {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl ItemValue {
    pub fn params(&self) -> impl Iterator<Item = Pat> + '_ {
        children(self.syntax())
    }

    pub fn body(&self) -> Option<Expr> {
        child(self.syntax())
    }

    pub fn ty(&self) -> Option<Type> {
        child(self.syntax())
    }

    pub fn where_clause(&self) -> Option<WhereClause> {
        child(self.syntax())
    }

    pub fn foreign_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::FOREIGN_KW)
    }
}

impl AttrsOwner for ItemType {
}

impl NameOwner for ItemType {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl ItemType {
    pub fn type_vars(&self) -> Option<TypeVars> {
        child(self.syntax())
    }

    pub fn foreign_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::FOREIGN_KW)
    }

    pub fn kind(&self) -> Option<Type> {
        self.foreign_token().and_then(|_| child(self.syntax()))
    }

    pub fn ty(&self) -> Option<Type> {
        match self.foreign_token() {
            | None => child(self.syntax()),
            | Some(_) => None,
        }
    }

    pub fn ctors(&self) -> impl Iterator<Item = Ctor> + '_ {
        children(self.syntax())
    }
}

impl AttrsOwner for Ctor {
}

impl NameOwner for Ctor {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl Ctor {
    pub fn types(&self) -> impl Iterator<Item = Type> + '_ {
        children(self.syntax())
    }

    pub fn record(&self) -> Option<CtorRecord> {
        child(self.syntax())
    }
}

impl CtorRecord {
    pub fn fields(&self) -> impl Iterator<Item = CtorField> + '_ {
        children(self.syntax())
    }
}

impl AttrsOwner for CtorField {
}

impl NameOwner for CtorField {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl CtorField {
    pub fn ty(&self) -> Option<Type> {
        child(self.syntax())
    }
}

impl AttrsOwner for ItemTrait {
}

impl NameOwner for ItemTrait {
    fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl ItemTrait {
    pub fn type_vars(&self) -> Option<TypeVars> {
        child(self.syntax())
    }

    pub fn where_clause(&self) -> Option<WhereClause> {
        child(self.syntax())
    }

    pub fn items(&self) -> impl Iterator<Item = ItemValue> + '_ {
        children(self.syntax())
    }
}

impl AttrsOwner for ItemImpl {
}

impl ItemImpl {
    pub fn trait_(&self) -> Option<Path> {
        child(self.syntax())
    }

    pub fn types(&self) -> impl Iterator<Item = Type> + '_ {
        children(self.syntax())
    }

    pub fn where_clause(&self) -> Option<WhereClause> {
        child(self.syntax())
    }

    pub fn items(&self) -> impl Iterator<Item = ItemValue> + '_ {
        children(self.syntax())
    }
}

impl TypeVars {
    pub fn iter(&self) -> impl Iterator<Item = Name> + '_ {
        children(self.syntax())
    }
}

impl WhereClause {
    pub fn iter(&self) -> impl Iterator<Item = WhereClauseItem> + '_ {
        children(self.syntax())
    }
}

impl WhereClauseConstraint {
    pub fn path(&self) -> Option<Path> {
        child(self.syntax())
    }

    pub fn types(&self) -> impl Iterator<Item = Type> + '_ {
        children(self.syntax())
    }
}

impl TypeParens {
    pub fn ty(&self) -> Option<Type> {
        child(self.syntax())
    }
}

impl TypeLiteral {
    pub fn literal(&self) -> Option<Literal> {
        child(self.syntax())
    }
}

impl TypeVar {
    pub fn name(&self) -> Option<Name> {
        child(self.syntax())
    }
}

impl TypePath {
    pub fn path(&self) -> Option<Path> {
        child(self.syntax())
    }
}

impl TypeRef {
    pub fn lifetime(&self) -> Option<Type> {
        if children::<Type>(self.syntax()).count() == 1 {
            return None;
        }

        child(self.syntax())
    }

    pub fn ty(&self) -> Option<Type> {
        children(self.syntax()).last()
    }
}

impl TypeList {
    pub fn ty(&self) -> Option<Type> {
        child(self.syntax())
    }
}

impl TypeApp {
    pub fn base(&self) -> Option<Type> {
        child(self.syntax())
    }

    pub fn args(&self) -> impl Iterator<Item = Type> + '_ {
        children(self.syntax()).skip(1)
    }
}

impl TypeInfix {
    pub fn types(&self) -> impl Iterator<Item = Type> + '_ {
        children(self.syntax())
    }

    pub fn ops(&self) -> impl Iterator<Item = Path> + '_ {
        children(self.syntax())
    }
}

impl TypeFunc {
    pub fn env(&self) -> Option<TypeFuncEnv> {
        child(self.syntax())
    }

    pub fn args(&self) -> impl Iterator<Item = Type> + '_ {
        let len = children::<Type>(self.syntax()).count();
        let len = if len > 0 { len - 1 } else { len };
        children(self.syntax()).take(len)
    }

    pub fn ret(&self) -> Option<Type> {
        children(self.syntax()).last()
    }

    pub fn dbl_dot_token(&self) -> Option<&SyntaxToken> {
        token(self.syntax(), SyntaxKind::DBL_DOT)
    }
}

impl TypeFuncEnv {
    pub fn ty(&self) -> Option<Type> {
        child(self.syntax())
    }
}

impl PatParens {
    pub fn pat(&self) -> Option<Pat> {
        child(self.syntax())
    }
}

impl PatTyped {
    pub fn pat(&self) -> Option<Pat> {
        child(self.syntax())
    }

    pub fn ty(&self) -> Option<Type> {
        child(self.syntax())
    }
}

impl PatBind {
    pub fn name(&self) -> Option<Name> {
        child(self.syntax())
    }

    pub fn subpat(&self) -> Option<Pat> {
        child(self.syntax())
    }
}

impl PatLiteral {
    pub fn literal(&self) -> Option<Literal> {
        child(self.syntax())
    }
}

impl PatPath {
    pub fn path(&self) -> Option<Path> {
        child(self.syntax())
    }
}

impl PatInfix {
    pub fn pats(&self) -> impl Iterator<Item = Pat> + '_ {
        children(self.syntax())
    }

    pub fn ops(&self) -> impl Iterator<Item = Path> + '_ {
        children(self.syntax())
    }
}

impl PatApp {
    pub fn base(&self) -> Option<PatPath> {
        child(self.syntax())
    }

    pub fn args(&self) -> impl Iterator<Item = Pat> + '_ {
        children(self.syntax()).skip(1)
    }
}

impl ExprParens {
    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl ExprTyped {
    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }

    pub fn ty(&self) -> Option<Type> {
        child(self.syntax())
    }
}

impl ExprPath {
    pub fn path(&self) -> Option<Path> {
        child(self.syntax())
    }
}

impl ExprLiteral {
    pub fn literal(&self) -> Option<Literal> {
        child(self.syntax())
    }
}

impl ExprArray {
    pub fn exprs(&self) -> impl Iterator<Item = Expr> + '_ {
        children(self.syntax())
    }
}

impl ExprLambda {
    pub fn params(&self) -> impl Iterator<Item = Pat> + '_ {
        children(self.syntax())
    }

    pub fn body(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl ExprRef {
    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl ExprInfix {
    pub fn exprs(&self) -> impl Iterator<Item = Expr> + '_ {
        children(self.syntax())
    }

    pub fn ops(&self) -> impl Iterator<Item = Path> + '_ {
        children(self.syntax())
    }
}

impl ExprPipe {
    pub fn direction(&self) -> PipeDirection {
        if token(self.syntax(), SyntaxKind::PIPE_LEFT).is_some() {
            PipeDirection::Left
        } else if token(self.syntax(), SyntaxKind::PIPE_RIGHT).is_some() {
            PipeDirection::Right
        } else {
            PipeDirection::Method
        }
    }

    pub fn left(&self) -> Option<Expr> {
        child(self.syntax())
    }

    pub fn right(&self) -> Option<Expr> {
        children(self.syntax()).nth(1)
    }

    pub fn args(&self) -> impl Iterator<Item = Expr> + '_ {
        children(self.syntax()).skip(2)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PipeDirection {
    Left,
    Right,
    Method,
}

impl ExprApp {
    pub fn base(&self) -> Option<Expr> {
        child(self.syntax())
    }

    pub fn args(&self) -> impl Iterator<Item = Expr> + '_ {
        children(self.syntax()).skip(1)
    }
}

impl ExprBlock {
    pub fn statements(&self) -> impl Iterator<Item = Stmt> + '_ {
        children(self.syntax())
    }
}

impl ExprDo {
    pub fn statements(&self) -> impl Iterator<Item = Stmt> + '_ {
        children(self.syntax())
    }
}

impl ExprIf {
    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }

    pub fn then(&self) -> Option<Expr> {
        children(self.syntax()).nth(1)
    }

    pub fn else_(&self) -> Option<Expr> {
        children(self.syntax()).nth(2)
    }
}

impl ExprMatch {
    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }

    pub fn arms(&self) -> impl Iterator<Item = MatchArm> + '_ {
        children(self.syntax())
    }
}

impl ExprReturn {
    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl MatchArm {
    pub fn pat(&self) -> Option<Pat> {
        child(self.syntax())
    }

    pub fn guard(&self) -> Option<Expr> {
        child::<MatchGuard>(self.syntax()).and_then(|g| child(g.syntax()))
    }

    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl MatchGuard {
    pub fn is_else(&self) -> bool {
        token(self.syntax(), SyntaxKind::ELSE_KW).is_some()
    }

    pub fn guard(&self) -> Option<Expr> {
        child(self.syntax())
    }

    pub fn expr(&self) -> Option<Expr> {
        children(self.syntax()).nth(1)
    }
}

impl StmtBind {
    pub fn pat(&self) -> Option<Pat> {
        child(self.syntax())
    }

    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl StmtLet {
    pub fn pat(&self) -> Option<Pat> {
        child(self.syntax())
    }

    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl StmtExpr {
    pub fn expr(&self) -> Option<Expr> {
        child(self.syntax())
    }
}

impl LitInt {
    pub fn value(&self, resolver: &dyn Resolver) -> Option<i128> {
        let text = token(self.syntax(), SyntaxKind::INT)?.resolve_text(resolver);

        text.parse().ok()
    }
}

impl LitFloat {
    pub fn value(&self, resolver: &dyn Resolver) -> Option<u64> {
        let text = token(self.syntax(), SyntaxKind::FLOAT)?.resolve_text(resolver);

        text.parse::<f64>().ok().map(|f| f.to_bits())
    }
}

impl LitChar {
    pub fn value(&self, resolver: &dyn Resolver) -> Option<char> {
        let text = token(self.syntax(), SyntaxKind::CHAR)?.resolve_text(resolver);
        let mut chars = text[1..].chars();

        match chars.next()? {
            | '\\' => parse_escape_sequence(&mut chars),
            | ch => Some(ch),
        }
    }
}

impl LitString {
    pub fn value(&self, resolver: &dyn Resolver) -> Option<String> {
        let text = token(self.syntax(), SyntaxKind::STRING)?.resolve_text(resolver);
        let mut result = String::with_capacity(text.len() - 2);
        let mut chars = text[1..text.len() - 1].chars();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                result.push(parse_escape_sequence(&mut chars)?);
            } else {
                result.push(ch);
            }
        }

        Some(result)
    }
}

fn parse_escape_sequence(chars: &mut dyn Iterator<Item = char>) -> Option<char> {
    Some(match chars.next()? {
        | '0' => '\0',
        | 'r' => '\r',
        | 'n' => '\n',
        | '\'' => '\'',
        | '\"' => '\"',
        | _ => return None,
    })
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
        token2(self.syntax(), &[SyntaxKind::IDENT, SyntaxKind::CONST, SyntaxKind::TYPE])
    }

    pub fn symbol_token(&self) -> Option<&SyntaxToken> {
        token2(self.syntax(), &[
            SyntaxKind::SYMBOL,
            SyntaxKind::COMMA,
            SyntaxKind::PIPE,
            SyntaxKind::AT,
        ])
    }
}

impl NameRef {
    pub fn ident_token(&self) -> Option<&SyntaxToken> {
        token2(self.syntax(), &[SyntaxKind::IDENT, SyntaxKind::CONST, SyntaxKind::TYPE])
    }

    pub fn symbol_token(&self) -> Option<&SyntaxToken> {
        token2(self.syntax(), &[
            SyntaxKind::SYMBOL,
            SyntaxKind::COMMA,
            SyntaxKind::PIPE,
            SyntaxKind::AT,
        ])
    }
}

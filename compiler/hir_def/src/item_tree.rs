mod lower;

use crate::ast_id::FileAstId;
use crate::attrs::{Attrs, RawAttrs};
use crate::db::DefDatabase;
use crate::diagnostics::ItemTreeDiagnostic;
use crate::in_file::InFile;
use crate::name::Name;
use crate::path::Path;
use arena::{Arena, Idx};
use base_db::input::FileId;
use rustc_hash::FxHashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::{Index, Range};
use std::sync::Arc;
use syntax::ast::{self, AstNode};

#[derive(Debug, PartialEq, Eq)]
pub struct ItemTree {
    pub(crate) file: FileId,
    top_level: Vec<Vec<Item>>,
    data: ItemTreeData,
    attrs: FxHashMap<AttrOwner, RawAttrs>,
    pub diagnostics: Vec<ItemTreeDiagnostic>,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct ItemTreeData {
    imports: Arena<Import>,
    fixities: Arena<Fixity>,
    funcs: Arena<Func>,
    statics: Arena<Static>,
    consts: Arena<Const>,
    type_aliases: Arena<TypeAlias>,
    type_ctors: Arena<TypeCtor>,
    ctors: Arena<Ctor>,
    classes: Arena<Class>,
    members: Arena<Member>,
}

pub trait ItemTreeNode: Clone {
    type Source: AstNode + Into<ast::Item>;

    fn ast_id(&self) -> FileAstId<Self::Source>;

    fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self;

    fn id_from_item(item: Item) -> Option<LocalItemTreeId<Self>>;

    fn id_to_item(id: LocalItemTreeId<Self>) -> Item;
}

pub struct LocalItemTreeId<N: ItemTreeNode> {
    index: Idx<N>,
    _marker: PhantomData<N>,
}

pub type ItemTreeId<N> = InFile<LocalItemTreeId<N>>;

impl ItemTree {
    fn new(file: FileId) -> Self {
        Self {
            file,
            top_level: Vec::new(),
            data: ItemTreeData::default(),
            attrs: FxHashMap::default(),
            diagnostics: Vec::new(),
        }
    }

    pub fn item_tree_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<ItemTree> {
        let syntax = db.parse(file_id);
        let ctx = lower::Ctx::new(db, file_id);
        let item_tree = ctx.lower_modules(&syntax.tree());

        Arc::new(item_tree)
    }

    pub fn top_level(&self, module: usize) -> &[Item] {
        &self.top_level[module]
    }

    pub(crate) fn raw_attrs(&self, of: AttrOwner) -> &RawAttrs {
        self.attrs.get(&of).unwrap_or(&RawAttrs::EMPTY)
    }

    pub fn attrs(&self, of: AttrOwner) -> Attrs {
        Attrs(self.raw_attrs(of).clone())
    }

    pub fn source<N: ItemTreeNode>(&self, db: &dyn DefDatabase, item: LocalItemTreeId<N>) -> N::Source {
        let root = db.parse(self.file);
        let id = self[item].ast_id();
        let map = db.ast_id_map(self.file);
        let ptr = map.get(id);

        ptr.to_node(&root.syntax_node())
    }
}

impl<N: ItemTreeNode> Index<LocalItemTreeId<N>> for ItemTree {
    type Output = N;

    fn index(&self, id: LocalItemTreeId<N>) -> &N {
        N::lookup(self, id.index)
    }
}

impl Index<Idx<Ctor>> for ItemTree {
    type Output = Ctor;

    fn index(&self, index: Idx<Ctor>) -> &Self::Output {
        &self.data.ctors[index]
    }
}

macro_rules! items {
    ($($typ:ident in $f_id:ident -> $ast:ty),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Item {
            $($typ(LocalItemTreeId<$typ>),)*
        }

        $(
            impl From<LocalItemTreeId<$typ>> for Item {
                fn from(id: LocalItemTreeId<$typ>) -> Self {
                    Item::$typ(id)
                }
            }
        )*

        $(
            impl ItemTreeNode for $typ {
                type Source = $ast;

                fn ast_id(&self) -> FileAstId<Self::Source> {
                    self.ast_id
                }

                fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self {
                    &tree.data.$f_id[index]
                }

                fn id_from_item(item: Item) -> Option<LocalItemTreeId<Self>> {
                    if let Item::$typ(id) = item {
                        Some(id)
                    } else {
                        None
                    }
                }

                fn id_to_item(id: LocalItemTreeId<Self>) -> Item {
                    Item::$typ(id)
                }
            }

            impl Index<Idx<$typ>> for ItemTree {
                type Output = $typ;

                fn index(&self, index: Idx<$typ>) -> &Self::Output {
                    &self.data.$f_id[index]
                }
            }
        )*
    }
}

items! {
    Import in imports -> ast::ItemImport,
    Fixity in fixities -> ast::ItemFixity,
    Func in funcs -> ast::ItemFun,
    Static in statics -> ast::ItemStatic,
    Const in consts -> ast::ItemConst,
    TypeAlias in type_aliases -> ast::ItemType,
    TypeCtor in type_ctors -> ast::ItemType,
    Class in classes -> ast::ItemClass,
    Member in members -> ast::ItemMember,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub ast_id: FileAstId<ast::ItemImport>,
    pub path: Path,
    pub qualify: Option<Name>,
    pub alias: Option<Name>,
    pub is_glob: bool,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fixity {
    pub ast_id: FileAstId<ast::ItemFixity>,
    pub name: Name,
    pub func: Path,
    pub kind: FixityKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FixityKind {
    Infix { assoc: Assoc, prec: Prec },
    Postfix,
    Prefix,
}

pub use ast::{Assoc, Prec};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub ast_id: FileAstId<ast::ItemFun>,
    pub name: Name,
    pub has_body: bool,
    pub is_foreign: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Static {
    pub ast_id: FileAstId<ast::ItemStatic>,
    pub name: Name,
    pub is_foreign: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Const {
    pub ast_id: FileAstId<ast::ItemConst>,
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAlias {
    pub ast_id: FileAstId<ast::ItemType>,
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeCtor {
    pub ast_id: FileAstId<ast::ItemType>,
    pub name: Name,
    pub ctors: IdRange<Ctor>,
    pub is_foreign: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctor {
    pub ast_id: FileAstId<ast::Ctor>,
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub ast_id: FileAstId<ast::ItemClass>,
    pub name: Name,
    pub fundeps: Box<[FunDep]>,
    pub items: Box<[AssocItem]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDep {
    pub determiners: Box<[Name]>,
    pub determined: Box<[Name]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Member {
    pub ast_id: FileAstId<ast::ItemMember>,
    pub class: Path,
    pub items: Box<[AssocItem]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssocItem {
    Func(LocalItemTreeId<Func>),
    Static(LocalItemTreeId<Static>),
}

pub struct IdRange<T> {
    range: Range<u32>,
    _marker: PhantomData<T>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum AttrOwner {
    Item(Item),
    Ctor(Idx<Ctor>),
}

impl From<Item> for AttrOwner {
    fn from(it: Item) -> Self {
        Self::Item(it)
    }
}

impl From<Idx<Ctor>> for AttrOwner {
    fn from(it: Idx<Ctor>) -> Self {
        Self::Ctor(it)
    }
}

impl<T> IdRange<T> {
    fn new(range: Range<Idx<T>>) -> Self {
        IdRange {
            range: range.start.into_raw().into()..range.end.into_raw().into(),
            _marker: PhantomData,
        }
    }
}

impl<T> Iterator for IdRange<T> {
    type Item = Idx<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(|raw| Idx::from_raw(raw.into()))
    }
}

impl<T> DoubleEndedIterator for IdRange<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.range.next_back().map(|raw| Idx::from_raw(raw.into()))
    }
}

impl<N: ItemTreeNode> Clone for LocalItemTreeId<N> {
    fn clone(&self) -> Self {
        LocalItemTreeId {
            index: self.index,
            _marker: PhantomData,
        }
    }
}

impl<N: ItemTreeNode> Copy for LocalItemTreeId<N> {
}

impl<N: ItemTreeNode> PartialEq for LocalItemTreeId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<N: ItemTreeNode> Eq for LocalItemTreeId<N> {
}

impl<N: ItemTreeNode> Hash for LocalItemTreeId<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}

impl<N: ItemTreeNode> fmt::Debug for LocalItemTreeId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.index.fmt(f)
    }
}

impl<T> Clone for IdRange<T> {
    fn clone(&self) -> Self {
        IdRange {
            range: self.range.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> PartialEq for IdRange<T> {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range
    }
}

impl<T> Eq for IdRange<T> {
}

impl<T> fmt::Debug for IdRange<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(&format!("IdRange::<{}>", std::any::type_name::<T>()))
            .field(&self.range)
            .finish()
    }
}

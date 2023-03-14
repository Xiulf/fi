pub mod lower;

use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Index;

use arena::{Arena, Idx};
use base_db::input::File;
pub use lower::query;
use rustc_hash::FxHashMap;
use syntax::ast::{self, AstNode};
pub use syntax::ast::{Assoc, Prec};
use vfs::InFile;

use crate::ast_id::FileAstId;
use crate::attrs::{Attrs, RawAttrs};
use crate::name::Name;
use crate::path::Path;

#[derive(Debug, PartialEq, Eq)]
pub struct ItemTree {
    file: File,
    items: Vec<Item>,
    data: ItemTreeData,
    attrs: FxHashMap<AttrOwner, RawAttrs>,
}

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct ItemTreeData {
    modules: Arena<Module>,
    imports: Arena<Import>,
    fixities: Arena<Fixity>,
    values: Arena<Value>,
    type_aliases: Arena<TypeAlias>,
    type_ctors: Arena<TypeCtor>,
    ctors: Arena<Ctor>,
    fields: Arena<Field>,
    traits: Arena<Trait>,
    impls: Arena<Impl>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttrOwner {
    Item(Item),
    Ctor(Idx<Ctor>),
    Field(Idx<Field>),
}

impl ItemTree {
    pub fn items(&self) -> &[Item] {
        &self.items
    }

    pub fn attrs(&self, owner: AttrOwner) -> Attrs {
        Attrs(self.raw_attrs(owner).clone())
    }

    pub(crate) fn raw_attrs(&self, owner: AttrOwner) -> &RawAttrs {
        self.attrs.get(&owner).unwrap_or(&RawAttrs::EMPTY)
    }
}

impl<N: ItemTreeNode> Index<LocalItemTreeId<N>> for ItemTree {
    type Output = N;

    fn index(&self, id: LocalItemTreeId<N>) -> &Self::Output {
        N::lookup(self, id.index)
    }
}

impl Index<Idx<Ctor>> for ItemTree {
    type Output = Ctor;

    fn index(&self, id: Idx<Ctor>) -> &Self::Output {
        &self.data.ctors[id]
    }
}

impl Index<Idx<Field>> for ItemTree {
    type Output = Field;

    fn index(&self, id: Idx<Field>) -> &Self::Output {
        &self.data.fields[id]
    }
}

macro_rules! items {
    ($($typ:ident in $f_id:ident -> $ast:ty),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Item {
            $($typ(LocalItemTreeId<$typ>)),*
        }

        $(
            impl From<LocalItemTreeId<$typ>> for Item {
                fn from(id: LocalItemTreeId<$typ>) -> Self {
                    Self::$typ(id)
                }
            }

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
        )*
    };
}

items! {
    Module in modules -> ast::ItemModule,
    Import in imports -> ast::ItemImport,
    Fixity in fixities -> ast::ItemFixity,
    Value in values -> ast::ItemValue,
    TypeAlias in type_aliases -> ast::ItemType,
    TypeCtor in type_ctors -> ast::ItemType,
    Trait in traits -> ast::ItemTrait,
    Impl in impls -> ast::ItemImpl,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub ast_id: FileAstId<ast::ItemModule>,
    pub name: Name,
    pub items: Box<[Item]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub ast_id: FileAstId<ast::ItemImport>,
    pub path: Path,
    pub rename: Option<Name>,
    pub qualify: Option<Name>,
    pub hiding: Option<Box<[Name]>>,
    pub index: usize,
    pub all: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fixity {
    pub ast_id: FileAstId<ast::ItemFixity>,
    pub name: Name,
    pub value: Path,
    pub is_type: bool,
    pub kind: FixityKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FixityKind {
    Infix(Assoc, Prec),
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Value {
    pub ast_id: FileAstId<ast::ItemValue>,
    pub name: Name,
    pub is_foreign: bool,
    pub has_body: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAlias {
    pub ast_id: FileAstId<ast::ItemType>,
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeCtor {
    pub ast_id: FileAstId<ast::ItemType>,
    pub name: Name,
    pub ctors: Box<[Idx<Ctor>]>,
    pub is_foreign: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ctor {
    pub ast_id: FileAstId<ast::Ctor>,
    pub name: Name,
    pub fields: Option<Box<[Idx<Field>]>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub ast_id: FileAstId<ast::CtorField>,
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
    pub ast_id: FileAstId<ast::ItemTrait>,
    pub name: Name,
    pub items: Box<[LocalItemTreeId<Value>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    pub ast_id: FileAstId<ast::ItemImpl>,
    pub trait_: Path,
    pub items: Box<[LocalItemTreeId<Value>]>,
}

impl From<Item> for AttrOwner {
    fn from(value: Item) -> Self {
        Self::Item(value)
    }
}

impl From<Idx<Ctor>> for AttrOwner {
    fn from(value: Idx<Ctor>) -> Self {
        Self::Ctor(value)
    }
}

impl From<Idx<Field>> for AttrOwner {
    fn from(value: Idx<Field>) -> Self {
        Self::Field(value)
    }
}

impl<N: ItemTreeNode> Clone for LocalItemTreeId<N> {
    fn clone(&self) -> Self {
        Self {
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
        self.index.hash(state);
    }
}

impl<N: ItemTreeNode> fmt::Debug for LocalItemTreeId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LocalItemTreeId").field(&self.index).finish()
    }
}

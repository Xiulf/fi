use crate::db::DefDatabase;
use crate::expr::Literal;
use crate::id::{AttrDefId, Lookup};
use crate::in_file::InFile;
use crate::item_tree::{ItemTreeId, ItemTreeNode};
use crate::name::{AsName, Name};
use std::ops::Deref;
use std::sync::Arc;
use syntax::ast;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Attrs(pub(crate) RawAttrs);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawAttrs {
    entries: Option<Arc<[Attr]>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrsWithOwner {
    attrs: Attrs,
    owner: AttrDefId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub(crate) name: Name,
    pub(crate) input: Option<AttrInput>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttrInput {
    Literal(Literal),
}

#[derive(Debug, Clone, Copy)]
pub struct AttrQuery<'a> {
    attrs: &'a Attrs,
    key: &'static str,
}

impl Attrs {
    pub const EMPTY: Self = Self(RawAttrs::EMPTY);

    pub fn by_key(&self, key: &'static str) -> AttrQuery<'_> {
        AttrQuery { attrs: self, key }
    }
}

impl RawAttrs {
    pub(crate) const EMPTY: Self = Self { entries: None };

    pub(crate) fn new(owner: &dyn ast::AttrsOwner) -> Self {
        let entries = collect_attrs(owner)
            .filter_map(|attr| match attr {
                | Either::Left(attr) => Attr::from_src(attr),
                | Either::Right(comment) => comment.doc_comment().map(|doc| Attr {
                    input: Some(AttrInput::Literal(Literal::String(doc.into()))),
                    name: "doc".as_name(),
                }),
            })
            .collect::<Arc<_>>();

        Self {
            entries: if entries.is_empty() { None } else { Some(entries) },
        }
    }

    fn from_attrs_owner(owner: InFile<&dyn ast::AttrsOwner>) -> Self {
        Self::new(owner.value)
    }
}

impl AttrsWithOwner {
    pub(crate) fn attrs_query(db: &dyn DefDatabase, def: AttrDefId) -> Self {
        let raw_attrs = match def {
            | AttrDefId::ModuleId(id) => {
                let def_map = db.def_map(id.lib);
                let data = &def_map[id.local_id];
                let decl = data.origin.declaration(db, &def_map);

                RawAttrs::from_attrs_owner(decl.as_ref().map(|it| it as &dyn ast::AttrsOwner))
            },
            | AttrDefId::FixityId(id) => attrs_from_item_tree(id.lookup(db).id, db),
            | AttrDefId::FuncId(id) => attrs_from_item_tree(id.lookup(db).id, db),
            | AttrDefId::StaticId(id) => attrs_from_item_tree(id.lookup(db).id, db),
            | AttrDefId::ConstId(id) => attrs_from_item_tree(id.lookup(db).id, db),
            | AttrDefId::TypeAliasId(id) => attrs_from_item_tree(id.lookup(db).id, db),
            | AttrDefId::TypeCtorId(id) => attrs_from_item_tree(id.lookup(db).id, db),
            | AttrDefId::ClassId(id) => attrs_from_item_tree(id.lookup(db).id, db),
            | AttrDefId::InstanceId(id) => attrs_from_item_tree(id.lookup(db).id, db),
        };

        let attrs = Attrs(raw_attrs);

        Self { attrs, owner: def }
    }
}

impl<'a> AttrQuery<'a> {
    pub fn string_value(self) -> Option<&'a str> {
        self.attrs().find_map(Attr::string_value)
    }

    pub fn exists(self) -> bool {
        self.attrs().next().is_some()
    }

    pub fn attrs(self) -> impl Iterator<Item = &'a Attr> + Clone {
        let key = self.key;

        self.attrs.iter().filter(move |attr| attr.name.to_string() == key)
    }
}

impl Attr {
    fn from_src(ast: ast::Attr) -> Option<Self> {
        let name = ast.name()?.as_name();
        let input = if let Some(lit) = ast.value() {
            Some(AttrInput::Literal(Literal::from_src(lit)?))
        } else {
            None
        };

        Some(Self { name, input })
    }

    pub fn string_value(&self) -> Option<&str> {
        match self.input.as_ref()? {
            | AttrInput::Literal(Literal::String(s)) => Some(s),
            | _ => None,
        }
    }
}

impl Literal {
    fn from_src(ast: ast::Literal) -> Option<Self> {
        match ast {
            | ast::Literal::Int(i) => Some(Literal::Int(i.value()?)),
            | ast::Literal::Float(i) => Some(Literal::Float(i.value()?.to_bits())),
            | ast::Literal::Char(i) => Some(Literal::Char(i.value()?)),
            | ast::Literal::String(i) => Some(Literal::String(i.value()?)),
        }
    }
}

enum Either<L, R> {
    Left(L),
    Right(R),
}

fn attrs_from_item_tree<N: ItemTreeNode>(id: ItemTreeId<N>, db: &dyn DefDatabase) -> RawAttrs {
    let tree = db.item_tree(id.file_id);
    let item = N::id_to_item(id.value);

    tree.raw_attrs(item.into()).clone()
}

fn collect_attrs(owner: &dyn ast::AttrsOwner) -> impl Iterator<Item = Either<ast::Attr, ast::Comment>> {
    let attrs = owner.attrs().map(Either::Left);
    let docs = ast::CommentIter::from_syntax_node(owner.syntax())
        .filter(ast::Comment::is_doc)
        .map(Either::Right);

    docs.chain(attrs)
}

impl Deref for RawAttrs {
    type Target = [Attr];

    fn deref(&self) -> &Self::Target {
        match &self.entries {
            | Some(it) => &*it,
            | None => &[],
        }
    }
}

impl Deref for Attrs {
    type Target = [Attr];

    fn deref(&self) -> &Self::Target {
        match &self.0.entries {
            | Some(it) => &*it,
            | None => &[],
        }
    }
}

impl Deref for AttrsWithOwner {
    type Target = Attrs;

    fn deref(&self) -> &Self::Target {
        &self.attrs
    }
}

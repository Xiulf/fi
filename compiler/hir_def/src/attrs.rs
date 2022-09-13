use std::ops::Deref;
use std::sync::Arc;

use cfg::{Cfg, CfgAtom, CfgValue};
use either::Either;
use syntax::ast;

use crate::db::DefDatabase;
use crate::expr::Literal;
use crate::id::{AttrDefId, Lookup};
use crate::in_file::InFile;
use crate::item_tree::{ItemTreeId, ItemTreeNode};
use crate::name::{AsName, Name};

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
    Group(AttrInputGroup),
    Field(Name, Arc<AttrInput>),
    Ident(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrInputGroup(Arc<[AttrInput]>);

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

    pub fn cfg(&self) -> Option<Cfg> {
        cfg_parse(self)
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
            | AttrDefId::MemberId(id) => attrs_from_item_tree(id.lookup(db).id, db),
        };

        let attrs = Attrs(raw_attrs);

        Self { attrs, owner: def }
    }
}

impl<'a> AttrQuery<'a> {
    pub fn string_value(self) -> impl Iterator<Item = &'a str> {
        self.attrs().filter_map(Attr::string_value)
    }

    pub fn exists(self) -> bool {
        self.attrs().next().is_some()
    }

    pub fn groups(self) -> impl Iterator<Item = &'a AttrInputGroup> + Clone {
        self.attrs().filter_map(Attr::group)
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
        } else if let Some(args) = ast.args() {
            let group = args.filter_map(AttrInput::from_src).collect();

            Some(AttrInput::Group(AttrInputGroup(group)))
        } else {
            None
        };

        Some(Self { name, input })
    }

    pub fn string_value(&self) -> Option<&str> {
        match self.literal()? {
            | Literal::String(s) => Some(s),
            | _ => None,
        }
    }

    pub fn int_value(&self) -> Option<i128> {
        match self.literal()? {
            | Literal::Int(i) => Some(*i),
            | _ => None,
        }
    }

    pub fn literal(&self) -> Option<&Literal> {
        match self.input.as_ref()? {
            | AttrInput::Literal(lit) => Some(lit),
            | _ => None,
        }
    }

    pub fn group(&self) -> Option<&AttrInputGroup> {
        match self.input.as_ref()? {
            | AttrInput::Group(g) => Some(g),
            | _ => None,
        }
    }
}

impl AttrInput {
    fn from_src(ast: ast::AttrArg) -> Option<Self> {
        match ast {
            | ast::AttrArg::Literal(lit) => {
                let lit = Literal::from_src(lit.literal()?)?;

                Some(Self::Literal(lit))
            },
            | ast::AttrArg::Ident(id) => {
                let id = id.name_ref()?.as_name();

                Some(Self::Ident(id))
            },
            | ast::AttrArg::Equal(eq) => {
                let name = eq.name_ref()?.as_name();
                let val = Literal::from_src(eq.literal()?)?;
                let val = Self::Literal(val);

                Some(Self::Field(name, Arc::new(val)))
            },
            | ast::AttrArg::Call(c) => {
                let name = c.name_ref()?.as_name();
                let val = c.args()?.filter_map(Self::from_src).collect();
                let val = Self::Group(AttrInputGroup(val));

                Some(Self::Field(name, Arc::new(val)))
            },
        }
    }

    pub fn group(&self) -> Option<&AttrInputGroup> {
        match self {
            | AttrInput::Group(g) => Some(g),
            | _ => None,
        }
    }

    pub fn field(&self, name: &str) -> Option<&AttrInput> {
        match self {
            | AttrInput::Field(field, val) if field.to_string() == name => Some(val),
            | _ => None,
        }
    }

    pub fn literal(&self) -> Option<&Literal> {
        match self {
            | AttrInput::Literal(l) => Some(l),
            | _ => None,
        }
    }

    pub fn ident(&self, name: &str) -> bool {
        match self {
            | AttrInput::Ident(i) => i.to_string() == name,
            | _ => false,
        }
    }

    pub fn string(&self) -> Option<&str> {
        match self.literal()? {
            | Literal::String(s) => Some(s),
            | _ => None,
        }
    }

    pub fn int(&self) -> Option<i128> {
        match self.literal()? {
            | Literal::Int(i) => Some(*i),
            | _ => None,
        }
    }
}

impl AttrInputGroup {
    pub fn field(&self, name: &str) -> Option<&AttrInput> {
        self.0.iter().find_map(|i| i.field(name))
    }

    pub fn ident(&self, name: &str) -> bool {
        self.0.iter().any(|i| i.ident(name))
    }

    pub fn string(&self) -> Option<&str> {
        self.0.iter().find_map(AttrInput::string)
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a AttrInput> {
        self.0.iter()
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

fn cfg_parse(attrs: &Attrs) -> Option<Cfg> {
    let mut cfgs = attrs.by_key("cfg").groups().map(cfg_parse_attr);
    let a = cfgs.next()?;

    match cfgs.next() {
        | Some(b) => Some(Cfg::All([a, b].into_iter().chain(cfgs).collect())),
        | None => Some(a),
    }
}

fn cfg_parse_attr(group: &AttrInputGroup) -> Cfg {
    group.iter().find_map(cfg_parse_expr).unwrap_or(Cfg::Invalid)
}

fn cfg_parse_expr(input: &AttrInput) -> Option<Cfg> {
    match input {
        | AttrInput::Ident(name) => Some(Cfg::Atom(CfgAtom::Flag(name.into()))),
        | AttrInput::Field(name, value) => match &**value {
            | AttrInput::Group(g) => match name.as_ref() {
                | "not" => Some(Cfg::Not(Box::new(cfg_parse_attr(g)))),
                | "any" => Some(Cfg::Any(g.iter().filter_map(cfg_parse_expr).collect())),
                | "all" => Some(Cfg::All(g.iter().filter_map(cfg_parse_expr).collect())),
                | _ => None,
            },
            | AttrInput::Literal(l) => {
                let value = match l {
                    | Literal::Int(i) => CfgValue::Int(*i),
                    | Literal::String(s) => CfgValue::String(s.into()),
                    | _ => return None,
                };

                Some(Cfg::Atom(CfgAtom::Key(name.into(), value)))
            },
            | _ => None,
        },
        | _ => None,
    }
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

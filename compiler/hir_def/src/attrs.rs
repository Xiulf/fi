use std::ops::Deref;
use std::sync::Arc;

use cfg::{Cfg, CfgAtom, CfgValue};
use either::Either;
use smol_str::SmolStr;
use syntax::ast;
use vfs::InFile;

use crate::expr::Literal;
use crate::id::ItemId;
use crate::item_tree::{ItemTreeId, ItemTreeNode};
use crate::Db;

const CFG_ATTR: &'static str = "cfg";
const CFG_ATTR_ATTR: &'static str = "cfg_attr";
const DOC_ATTR: &'static str = "doc";

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Attrs(pub(crate) RawAttrs);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawAttrs {
    entries: Option<Arc<[Attr]>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrsWithOwner {
    attrs: Attrs,
    owner: ItemId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub(crate) name: SmolStr,
    pub(crate) input: Option<AttrInput>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttrInput {
    Literal(Literal),
    Group(AttrInputGroup),
    Field(SmolStr, Arc<AttrInput>),
    Ident(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrInputGroup(Arc<[AttrInput]>);

#[derive(Debug, Clone, Copy)]
pub struct AttrQuery<'a> {
    attrs: &'a Attrs,
    key: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Documentation(String);

impl Attrs {
    pub const EMPTY: Self = Self(RawAttrs::EMPTY);

    pub fn by_key(&self, key: &'static str) -> AttrQuery<'_> {
        AttrQuery { attrs: self, key }
    }

    pub fn cfg(&self) -> Option<Cfg> {
        cfg_parse(self)
    }

    pub fn docs(&self) -> Option<Documentation> {
        let docs = self.by_key(DOC_ATTR).attrs().filter_map(|attr| attr.string_value());
        let indent = doc_indent(self);
        let mut buf = String::new();

        for doc in docs {
            if !doc.is_empty() {
                buf.extend(
                    doc.lines()
                        .map(|line| {
                            line.char_indices()
                                .nth(indent)
                                .map_or(line, |(offset, _)| &line[offset..])
                                .trim_end()
                        })
                        .intersperse("\n"),
                );
            }
        }

        buf.pop();

        if buf.is_empty() {
            None
        } else {
            Some(Documentation::new(buf))
        }
    }
}

impl RawAttrs {
    pub(crate) const EMPTY: Self = Self { entries: None };

    pub(crate) fn new(db: &dyn Db, owner: &dyn ast::AttrsOwner) -> Self {
        let resolver = db.syntax_interner().read();
        let entries = collect_attrs(owner)
            .filter_map(|attr| match attr {
                | Either::Left(attr) => Attr::from_src(attr, &*resolver),
                | Either::Right(comment) => comment.doc_comment(&*resolver).map(|doc| Attr {
                    input: Some(AttrInput::Literal(Literal::String(doc.into()))),
                    name: "doc".into(),
                }),
            })
            .collect::<Arc<_>>();

        Self {
            entries: if entries.is_empty() { None } else { Some(entries) },
        }
    }

    fn _from_attrs_owner(db: &dyn Db, owner: InFile<&dyn ast::AttrsOwner>) -> Self {
        Self::new(db, owner.value)
    }
}

pub fn query(db: &dyn Db, item: ItemId) -> Attrs {
    let raw_attrs = match item {
        | ItemId::ModuleId(_id) => {
            // let def_map = crate::def_map::query(db, id.lib(db));
            // let data = &def_map[id];
            // let decl = data.origin.declaration(db, &def_map);

            // RawAttrs::from_attrs_owner(db, decl.as_ref().map(|it| it as &dyn ast::AttrsOwner))
            todo!()
        },
        | ItemId::FixityId(id) => attrs_from_item_tree(id.it(db), db),
        | ItemId::ValueId(id) => attrs_from_item_tree(id.it(db), db),
        | ItemId::TypeAliasId(id) => attrs_from_item_tree(id.it(db), db),
        | ItemId::TypeCtorId(id) => attrs_from_item_tree(id.it(db), db),
        | ItemId::CtorId(_id) => todo!(),
        | ItemId::FieldId(_id) => todo!(),
        | ItemId::TraitId(id) => attrs_from_item_tree(id.it(db), db),
        | ItemId::ImplId(id) => attrs_from_item_tree(id.it(db), db),
    };

    Attrs(raw_attrs.filter(db))
}

impl RawAttrs {
    fn filter(self, _db: &dyn Db) -> Self {
        if !self.iter().any(|a| a.name == CFG_ATTR_ATTR) {
            return self;
        }

        let attrs = self
            .iter()
            .filter_map(|attr| {
                if attr.name != CFG_ATTR_ATTR {
                    return Some(attr.clone());
                }

                let Some(group) = attr.group() else {
                    return Some(attr.clone());
                };

                let Some(cfg) = group.0.get(0).and_then(cfg_parse_expr) else {
                    return Some(attr.clone());
                };

                let Some(child) = group.0.get(1) else {
                    return Some(attr.clone());
                };

                let child = match child.clone() {
                    | AttrInput::Ident(name) => Attr { name, input: None },
                    | AttrInput::Field(name, input) => Attr {
                        name,
                        input: Some((*input).clone()),
                    },
                    | _ => return Some(attr.clone()),
                };

                let mut options = cfg::CfgOptions::default();
                options.enable("windows");
                if !cfg.is_enabled(&options) {
                    return None;
                }

                Some(child)
            })
            .collect::<Arc<[_]>>();

        Self { entries: Some(attrs) }
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
    fn from_src(ast: ast::Attr, resolver: &syntax::Interner) -> Option<Self> {
        let name = ast.name()?.ident_token()?.resolve_text(resolver);
        let name = SmolStr::new(name);
        let input = if let Some(lit) = ast.value() {
            Some(AttrInput::Literal(Literal::from_src(lit, resolver)?))
        } else if let Some(args) = ast.args() {
            let group = args.iter().filter_map(|a| AttrInput::from_src(a, resolver)).collect();

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
    fn from_src(ast: ast::AttrArg, resolver: &syntax::Interner) -> Option<Self> {
        match ast {
            | ast::AttrArg::Literal(lit) => {
                let lit = Literal::from_src(lit.literal()?, resolver)?;

                Some(Self::Literal(lit))
            },
            | ast::AttrArg::Ident(id) => {
                let id = id.name_ref()?.ident_token()?.resolve_text(resolver);
                let id = SmolStr::new(id);

                Some(Self::Ident(id))
            },
            | ast::AttrArg::Equal(eq) => {
                let name = eq.name_ref()?.ident_token()?.resolve_text(resolver);
                let name = SmolStr::new(name);
                let val = Literal::from_src(eq.literal()?, resolver)?;
                let val = Self::Literal(val);

                Some(Self::Field(name, Arc::new(val)))
            },
            | ast::AttrArg::Call(c) => {
                let name = c.name_ref()?.ident_token()?.resolve_text(resolver);
                let name = SmolStr::new(name);
                let val = c.args()?.iter().filter_map(|a| Self::from_src(a, resolver)).collect();
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
    fn from_src(ast: ast::Literal, resolver: &syntax::Interner) -> Option<Self> {
        match ast {
            | ast::Literal::Int(i) => Some(Literal::Int(i.value(resolver)?)),
            // | ast::Literal::Float(i) => Some(Literal::Float(i.value()?.to_bits())),
            // | ast::Literal::Char(i) => Some(Literal::Char(i.value()?)),
            | ast::Literal::String(i) => Some(Literal::String(i.value(resolver)?)),
            | _ => todo!(),
        }
    }
}

impl Documentation {
    pub fn new(s: String) -> Self {
        Self(s)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

fn doc_indent(attrs: &Attrs) -> usize {
    attrs
        .by_key(DOC_ATTR)
        .attrs()
        .filter_map(|attr| attr.string_value())
        .flat_map(|s| s.lines())
        .filter(|line| !line.chars().all(|c| c.is_whitespace()))
        .map(|line| line.chars().take_while(|c| c.is_whitespace()).count())
        .min()
        .unwrap_or(0)
}

impl From<Documentation> for String {
    fn from(d: Documentation) -> Self {
        d.0
    }
}

fn attrs_from_item_tree<N: ItemTreeNode>(id: ItemTreeId<N>, db: &dyn Db) -> RawAttrs {
    let tree = crate::item_tree::query(db, id.file);
    let item = N::id_to_item(id.value);

    tree.raw_attrs(item.into()).clone()
}

fn collect_attrs(owner: &dyn ast::AttrsOwner) -> impl Iterator<Item = Either<ast::Attr, ast::Comment>> + '_ {
    let attrs = owner.attrs().map(Either::Left);
    // let docs = ast::CommentIter::from_syntax_node(owner.syntax())
    //     .filter(ast::Comment::is_doc)
    //     .map(Either::Right);

    // docs.chain(attrs)
    attrs
}

fn cfg_parse(attrs: &Attrs) -> Option<Cfg> {
    let mut cfgs = attrs.by_key(CFG_ATTR).groups().map(cfg_parse_attr);
    let a = cfgs.next()?;

    match cfgs.next() {
        | Some(b) => Some(Cfg::All([a, b].into_iter().chain(cfgs).collect())),
        | None => Some(a),
    }
}

fn cfg_parse_attr(group: &AttrInputGroup) -> Cfg {
    group.iter().next().and_then(cfg_parse_expr).unwrap_or(Cfg::Invalid)
}

fn cfg_parse_expr(input: &AttrInput) -> Option<Cfg> {
    match input {
        | AttrInput::Ident(name) => Some(Cfg::Atom(CfgAtom::Flag(name.clone()))),
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

                Some(Cfg::Atom(CfgAtom::Key(name.clone(), value)))
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

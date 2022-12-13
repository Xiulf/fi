use hir::db::HirDatabase;
use hir::semantics::Semantics;
use hir::{ModuleDef, PathResolution};
use syntax::{ast, match_ast, AstNode, SyntaxNode, SyntaxToken};

#[derive(Debug)]
pub enum Symbol {
    Module(hir::Module),
    Fixity(hir::Fixity),
    Func(hir::Func),
    Static(hir::Static),
    Const(hir::Const),
    Ctor(hir::Ctor),
    TypeAlias(hir::TypeAlias),
    TypeCtor(hir::TypeCtor),
    Class(hir::Class),
    Member(hir::Member),
    TypeVar(hir::TypeVar),
    Local(hir::Local),
}

impl Symbol {
    pub fn lib(&self, db: &dyn HirDatabase) -> Option<hir::Lib> {
        match self {
            | Self::Module(m) => Some(m.lib()),
            | _ => self.module(db).map(hir::Module::lib),
        }
    }

    pub fn module(&self, db: &dyn HirDatabase) -> Option<hir::Module> {
        let module = match self {
            | Self::Module(it) => it.parent(db)?,
            | Self::Fixity(it) => it.module(db),
            | Self::Func(it) => it.module(db),
            | Self::Static(it) => it.module(db),
            | Self::Const(it) => it.module(db),
            | Self::Ctor(it) => it.module(db),
            | Self::TypeAlias(it) => it.module(db),
            | Self::TypeCtor(it) => it.module(db),
            | Self::Class(it) => it.module(db),
            | Self::Member(it) => it.module(db),
            | Self::TypeVar(it) => it.module(db),
            | Self::Local(it) => it.module(db),
        };

        Some(module)
    }

    pub fn name(&self, db: &dyn HirDatabase) -> Option<hir::Name> {
        let name = match self {
            | Self::Module(it) => it.name(db),
            | Self::Fixity(it) => it.name(db),
            | Self::Func(it) => it.name(db),
            | Self::Static(it) => it.name(db),
            | Self::Const(it) => it.name(db),
            | Self::Ctor(it) => it.name(db),
            | Self::TypeAlias(it) => it.name(db),
            | Self::TypeCtor(it) => it.name(db),
            | Self::Class(it) => it.name(db),
            | Self::Member(_) => return None,
            | Self::TypeVar(it) => it.name(db),
            | Self::Local(it) => it.name(db),
        };

        Some(name)
    }
}

pub fn classify_node<DB>(sema: &Semantics<DB>, node: &SyntaxNode) -> Option<Symbol>
where
    DB: HirDatabase,
{
    match_ast! {
        match node {
            Name(name) => classify_name(sema, &name),
            NameRef(name_ref) => classify_name_ref(sema, &name_ref),
            _ => None,
        }
    }
}

pub fn classify_token<DB>(sema: &Semantics<DB>, token: &SyntaxToken) -> Option<Symbol>
where
    DB: HirDatabase,
{
    let node = token.parent();
    classify_node(sema, &node)
}

fn classify_name<DB>(sema: &Semantics<DB>, name: &ast::Name) -> Option<Symbol>
where
    DB: HirDatabase,
{
    let parent = name.syntax().parent()?;

    match_ast! {
        match parent {
            Item(it) => classify_item(sema, it),
            OneFunc(it) => classify_item_group(sema, it.syntax()),
            TypeVars(_it) => None,
            PatBind(pat_bind) => classify_pat_bind(sema, pat_bind),
            _ => None,
        }
    }
}

fn classify_name_ref<DB>(sema: &Semantics<DB>, name_ref: &ast::NameRef) -> Option<Symbol>
where
    DB: HirDatabase,
{
    let parent = name_ref.syntax().parent()?;

    match_ast! {
        match parent {
            PathSegment(path_segment) => sema.resolve_path(&path_segment.parent_path()?).map(Into::into),
            ItemImport(_) => sema.resolve_ident(&name_ref).map(Into::into),
            ExprIdent(_) => sema.resolve_ident(&name_ref).map(Into::into),
            _ => None,
        }
    }
}

fn classify_item<DB>(sema: &Semantics<DB>, item: ast::Item) -> Option<Symbol>
where
    DB: HirDatabase,
{
    let sym = match item {
        | ast::Item::Module(it) => Symbol::Module(sema.to_def(&it)?),
        | ast::Item::Func(it) => Symbol::Func(sema.to_def(&it)?),
        | ast::Item::Class(it) => Symbol::Class(sema.to_def(&it)?),
        | _ => return None,
    };

    Some(sym)
}

fn classify_item_group<DB>(sema: &Semantics<DB>, item: &SyntaxNode) -> Option<Symbol>
where
    DB: HirDatabase,
{
    let parent = item.parent()?;

    match_ast! {
        match parent {
            Item(it) => classify_item(sema, it),
            _ => None,
        }
    }
}

fn classify_pat_bind<DB>(sema: &Semantics<DB>, pat_bind: ast::PatBind) -> Option<Symbol>
where
    DB: HirDatabase,
{
    if let Some(def) = sema.resolve_bind_pat_to_const(&pat_bind) {
        return Some(def.into());
    }

    let local = sema.to_def(&pat_bind)?;

    Some(Symbol::Local(local))
}

impl From<PathResolution> for Symbol {
    fn from(path_resolution: PathResolution) -> Self {
        match path_resolution {
            | PathResolution::Def(def) => def.into(),
            | PathResolution::Local(local) => Self::Local(local),
            | PathResolution::TypeVar(tvar) => Self::TypeVar(tvar),
        }
    }
}

impl From<ModuleDef> for Symbol {
    fn from(def: ModuleDef) -> Self {
        match def {
            | ModuleDef::Module(def) => Self::Module(def),
            | ModuleDef::Fixity(def) => Self::Fixity(def),
            | ModuleDef::Func(def) => Self::Func(def),
            | ModuleDef::Static(def) => Self::Static(def),
            | ModuleDef::Const(def) => Self::Const(def),
            | ModuleDef::Ctor(def) => Self::Ctor(def),
            | ModuleDef::TypeAlias(def) => Self::TypeAlias(def),
            | ModuleDef::TypeCtor(def) => Self::TypeCtor(def),
            | ModuleDef::Class(def) => Self::Class(def),
        }
    }
}

use std::fmt::Write;

use either::Either;
use hir::attrs::HasAttrs;
use hir::semantics::Semantics;
use hir::HirDisplay;
use syntax::ast;

use crate::analysis::classify::Symbol;
use crate::db::LspDatabase;

pub fn symbol(db: &LspDatabase, symbol: Symbol) -> Option<String> {
    let module = symbol.module(db).map(|m| m.name(db).to_string());
    let (label, docs) = match symbol {
        | Symbol::Module(it) => label_and_docs(db, it),
        | Symbol::Func(it) => label_and_docs(db, it),
        | Symbol::TypeAlias(it) => label_and_docs(db, it),
        | Symbol::TypeCtor(it) => label_and_docs(db, it),
        | Symbol::Ctor(it) => return ctor(db, it, module),
        | Symbol::Local(it) => return local(db, it),
        | Symbol::TypeVar(it) => return type_var(db, it),
        | _ => return None,
    };

    markup(docs.map(Into::into), label, module)
}

fn ctor(db: &LspDatabase, it: hir::Ctor, module: Option<String>) -> Option<String> {
    let type_ctor = it.type_ctor();
    let type_vars = type_ctor
        .type_vars(db)
        .into_iter()
        .map(|tv| format!(" {}", tv.name(db)))
        .collect::<Vec<_>>()
        .join("");

    let types = it
        .types(db)
        .into_iter()
        .map(|ty| format!(" {}", ty.display(db)))
        .collect::<Vec<_>>()
        .join("");

    let desc = format!(
        "type {}{} =\n\t| {}{}",
        type_ctor.name(db),
        type_vars,
        it.name(db),
        types
    );

    markup(None, desc, module)
}

fn local(db: &LspDatabase, it: hir::Local) -> Option<String> {
    let ty = it.ty(db);
    let ty = ty.display(db);
    let desc = match it.source(db) {
        | Either::Left(_) => format!("let {} :: {}", it.name(db), ty),
        | Either::Right(_) => format!("let _ :: {}", ty),
    };

    markup(None, desc, None)
}

fn type_var(db: &LspDatabase, it: hir::TypeVar) -> Option<String> {
    let desc = it.display(db).to_string();

    markup(None, desc, None)
}

pub fn type_info(sema: &Semantics<LspDatabase>, expr_or_pat: &Result<ast::Expr, ast::Pat>) -> Option<String> {
    let ty = match expr_or_pat {
        | Result::Ok(expr) => sema.type_of_expr(expr)?,
        | Result::Err(pat) => sema.type_of_pat(pat)?,
    };

    Some(format!("```shade\n{}\n```", ty.display(sema.db)))
}

pub fn kind_info(sema: &Semantics<LspDatabase>, ty: &ast::Type) -> Option<String> {
    let kind = sema.kind_of(ty)?;

    Some(format!("```shade\n{}\n```", kind.display(sema.db)))
}

fn label_and_docs<D>(db: &LspDatabase, def: D) -> (String, Option<hir::Documentation>)
where
    D: HasAttrs + HirDisplay,
{
    let label = def.display(db).to_string();
    let docs = def.attrs(db).docs();

    (label, docs)
}

fn markup(docs: Option<String>, desc: String, module: Option<String>) -> Option<String> {
    let mut buf = String::new();

    if let Some(module) = module {
        if !module.is_empty() {
            write!(buf, "```shade\nmodule {}\n```\n\n", module).ok()?;
        }
    }

    write!(buf, "```shade\n{}\n```", desc).ok()?;

    if let Some(docs) = docs {
        write!(buf, "\n___\n\n{}", docs).ok()?;
    }

    Some(buf)
}

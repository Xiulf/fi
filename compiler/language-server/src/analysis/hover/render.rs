use std::fmt::Write;

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
        | _ => return None,
    };

    markup(docs.map(Into::into), label, module)
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

mod lower;

use arena::{Arena, ArenaMap};
pub use lower::query;
use rustc_hash::FxHashMap;
use syntax::ptr::AstPtr;
use vfs::File;

use crate::expr::{Expr, ExprId, Stmt};
use crate::id::ValueId;
use crate::pat::{Pat, PatId};
use crate::Db;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Body {
    exprs: Arena<Expr>,
    pats: Arena<Pat>,
    params: Box<[PatId]>,
    body_expr: ExprId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BodySourceMap {
    file: File,
    expr_to_src: ArenaMap<ExprId, ExprSrc>,
    src_to_expr: FxHashMap<AstPtr<syntax::ast::Expr>, ExprId>,
    pat_to_src: ArenaMap<PatId, PatSrc>,
    src_to_pat: FxHashMap<AstPtr<syntax::ast::Pat>, PatId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprSrc {
    Single(AstPtr<syntax::ast::Expr>),
    Infix(AstPtr<syntax::ast::Expr>, AstPtr<syntax::ast::Expr>),
    Operator(AstPtr<syntax::ast::Path>),
    Synthetic(ValueId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PatSrc {
    Single(AstPtr<syntax::ast::Pat>),
    Infix(AstPtr<syntax::ast::Pat>, AstPtr<syntax::ast::Pat>),
    Operator(AstPtr<syntax::ast::Path>),
    Synthetic(ValueId),
}

impl std::ops::Index<ExprId> for Body {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

impl std::ops::Index<PatId> for Body {
    type Output = Pat;

    fn index(&self, index: PatId) -> &Self::Output {
        &self.pats[index]
    }
}

impl Body {
    pub fn params(&self) -> &[PatId] {
        &self.params
    }

    pub fn body_expr(&self) -> ExprId {
        self.body_expr
    }

    pub fn debug(&self, db: &dyn Db) -> String {
        use ra_ap_stdx::format_to;
        let mut out = String::new();

        for (id, pat) in self.pats.iter() {
            format_to!(out, "\n${:0>2}: ", u32::from(id.into_raw()));

            match pat {
                | Pat::Missing => format_to!(out, "Missing"),
                | Pat::Wildcard => format_to!(out, "Wildcard"),
                | Pat::Bind { name, subpat } => {
                    format_to!(out, "Bind {}", name.display(db));
                    if let Some(subpat) = subpat {
                        format_to!(out, ", ${:0>2}", u32::from(subpat.into_raw()));
                    }
                },
                | Pat::Ctor { path, ctor, args } => {
                    format_to!(out, "Ctor {}, {ctor:?}", path.display(db));
                    for arg in args.iter() {
                        format_to!(out, ", ${:0>2}", u32::from(arg.into_raw()));
                    }
                },
            }
        }

        for (id, expr) in self.exprs.iter() {
            format_to!(out, "\n#{:0>3}: ", u32::from(id.into_raw()));

            match expr {
                | Expr::Missing => format_to!(out, "Missing"),
                | Expr::Hole(_) => format_to!(out, "Hole"),
                | Expr::Lit { lit } => format_to!(out, "Lit {lit}"),
                | Expr::Path { path, def } => format_to!(out, "Path {}, {def:?}", path.display(db)),
                | Expr::App { base, args } => {
                    format_to!(out, "App #{:0>3}", u32::from(base.into_raw()));
                    for arg in args.iter() {
                        format_to!(out, ", #{:0>3}", u32::from(arg.into_raw()));
                    }
                },
                | Expr::Block { stmts, expr } => {
                    format_to!(out, "Block ");
                    if let Some(expr) = expr {
                        format_to!(out, "#{:0>3}", u32::from(expr.into_raw()));
                    }
                    for stmt in stmts.iter() {
                        format_to!(out, "\n      └ ");
                        match stmt {
                            | Stmt::Let(pat, expr) => {
                                format_to!(
                                    out,
                                    "Let ${:0>2}, #{:0>3}",
                                    u32::from(pat.into_raw()),
                                    u32::from(expr.into_raw())
                                )
                            },
                            | Stmt::Expr(expr) => format_to!(out, "Expr #{:0>3}", u32::from(expr.into_raw())),
                        }
                    }
                },
                | _ => todo!("{expr:?}"),
            }
        }

        out
    }
}

impl ExprSrc {
    pub fn as_expr_ptr(self, lhs: bool) -> AstPtr<syntax::ast::Expr> {
        match self {
            | Self::Single(ptr) => ptr,
            | Self::Infix(ptr, _) if lhs => ptr,
            | Self::Infix(_, ptr) => ptr,
            | _ => unreachable!(),
        }
    }
}

impl PatSrc {
    pub fn as_pat_ptr(self, lhs: bool) -> AstPtr<syntax::ast::Pat> {
        match self {
            | Self::Single(ptr) => ptr,
            | Self::Infix(ptr, _) if lhs => ptr,
            | Self::Infix(_, ptr) => ptr,
            | _ => unreachable!(),
        }
    }
}

use std::sync::Arc;

use arena::Arena;
use diagnostics::Diagnostics;
use either::Either;
use ra_ap_stdx::hash::NoHashHashMap;
use syntax::ast;
use syntax::ptr::AstPtr;
use vfs::File;

use super::{Body, BodySourceMap, ExprSrc, SyntheticSugar};
use crate::def_map::DefMap;
use crate::diagnostics::UnresolvedPath;
use crate::expr::{Expr, ExprId, Literal, Stmt};
use crate::id::{HasModule, ValueDefId, ValueId};
use crate::name::Name;
use crate::pat::PatId;
use crate::path::Path;
use crate::per_ns::Namespace;
use crate::source::HasSource;
use crate::Db;

#[salsa::tracked]
pub fn query(db: &dyn Db, id: ValueId) -> Arc<Body> {
    let src = id.source(db);
    let mut ctx = Ctx::new(db, id, src.file);

    ctx.lower(src.value);

    Arc::new(ctx.body)
}

struct Ctx<'db> {
    db: &'db dyn Db,
    def_map: DefMap,
    value: ValueId,
    body: Body,
    src_map: BodySourceMap,
    scopes: Vec<Scope>,
}

struct Scope {
    names: NoHashHashMap<Name, PatId>,
}

impl<'db> Ctx<'db> {
    fn new(db: &'db dyn Db, value: ValueId, file: File) -> Self {
        let lib = value.container(db).module(db).lib(db);
        let def_map = crate::def_map::query(db, lib);

        Self {
            db,
            value,
            def_map,
            body: Body {
                exprs: Arena::default(),
                pats: Arena::default(),
                params: Box::default(),
                body_expr: ExprId::DUMMY,
            },
            src_map: BodySourceMap {
                file,
                expr_to_src: Default::default(),
                src_to_expr: Default::default(),
                pat_to_src: Default::default(),
                src_to_pat: Default::default(),
            },
            scopes: Vec::new(),
        }
    }

    fn lower(&mut self, item: ast::ItemValue) {
        let expr = self.lower_expr_opt(item.body());
        self.body.body_expr = expr;
    }

    fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr<ast::Expr>) -> ExprId {
        let id = self.make_expr(expr, Either::Left(ptr));

        self.src_map.src_to_expr.insert(ptr, id);
        id
    }

    fn alloc_expr_desugared(&mut self, expr: Expr) -> ExprId {
        self.make_expr(expr, Either::Right(SyntheticSugar(self.value)))
    }

    fn make_expr(&mut self, expr: Expr, src: ExprSrc) -> ExprId {
        let id = self.body.exprs.alloc(expr);

        self.src_map.expr_to_src.insert(id, src);
        id
    }

    fn missing_expr(&mut self) -> ExprId {
        self.alloc_expr_desugared(Expr::Missing)
    }

    fn lower_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        expr.map(|e| self.lower_expr(e)).unwrap_or_else(|| self.missing_expr())
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> ExprId {
        self.maybe_lower_expr(expr).unwrap_or_else(|| self.missing_expr())
    }

    fn maybe_lower_expr(&mut self, expr: ast::Expr) -> Option<ExprId> {
        let syntax_ptr = AstPtr::new(&expr);

        Some(match expr {
            | ast::Expr::Literal(e) => {
                let resolver = self.db.syntax_interner().read().unwrap();
                let lit = match e.literal()? {
                    | ast::Literal::Int(l) => Literal::Int(l.value(&*resolver)?),
                    | _ => todo!(),
                };

                self.alloc_expr(Expr::Lit { lit }, syntax_ptr)
            },
            | ast::Expr::Path(e) => {
                let epath = e.path()?;
                let path = Path::from_ast(self.db, epath.clone());
                let def = self.resolve_path(&path, &epath);

                self.alloc_expr(Expr::Path { path, def }, syntax_ptr)
            },
            | ast::Expr::Block(e) => {
                let (stmts, expr) = self.lower_block(e.statements());

                self.alloc_expr(Expr::Block { stmts, expr }, syntax_ptr)
            },
            | e => todo!("{e:?}"),
        })
    }

    fn lower_block(&mut self, mut stmts: impl Iterator<Item = ast::Stmt>) -> (Box<[Stmt]>, Option<ExprId>) {
        let mut curr = stmts.next();
        let mut next = stmts.next();
        let mut out = Vec::new();

        while let Some(stmt) = curr {
            let stmt = self.lower_stmt(stmt);
            if let (None, &Stmt::Expr(e)) = (&next, &stmt) {
                return (out.into_boxed_slice(), Some(e));
            }

            out.push(stmt);
            curr = next;
            next = stmts.next();
        }

        (out.into_boxed_slice(), None)
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> Stmt {
        match stmt {
            | ast::Stmt::Let(_) => todo!(),
            | ast::Stmt::Bind(_) => todo!(),
            | ast::Stmt::Expr(s) => Stmt::Expr(self.lower_expr_opt(s.expr())),
        }
    }

    fn resolve_path(&self, path: &Path, ast: &ast::Path) -> Option<ValueDefId> {
        if let Some(name) = path.as_name() {
            for scope in self.scopes.iter().rev() {
                if let Some(&pat) = scope.names.get(&name) {
                    return Some(ValueDefId::PatId(pat));
                }
            }
        }

        Diagnostics::emit(self.db, UnresolvedPath {
            file: self.src_map.file,
            ast: AstPtr::new(ast),
            path: path.clone(),
            ns: Namespace::Values,
        });

        None
    }
}

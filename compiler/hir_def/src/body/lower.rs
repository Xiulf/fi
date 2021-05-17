use crate::arena::Arena;
use crate::ast_id::{AstIdMap, FileAstId};
use crate::body::{Body, BodySourceMap, ExprPtr, ExprSource, PatPtr, PatSource, SyntheticSyntax};
use crate::db::DefDatabase;
use crate::def_map::DefMap;
use crate::expr::{dummy_expr_id, Expr, ExprId, Literal, RecordField, Stmt};
use crate::id::{LocalModuleId, ModuleDefId, ModuleId};
use crate::in_file::InFile;
use crate::name::{AsName, Name};
use crate::pat::{Pat, PatId};
use crate::path::Path;
use crate::type_ref::{TypeMap, TypeMapBuilder};
use base_db::input::FileId;
use std::sync::Arc;
use syntax::{ast, AstPtr};

pub struct LowerCtx {
    file_id: FileId,
    source_ast_id_map: Arc<AstIdMap>,
}

impl LowerCtx {
    pub fn new(db: &dyn DefDatabase, file_id: FileId) -> Self {
        LowerCtx {
            file_id,
            source_ast_id_map: db.ast_id_map(file_id),
        }
    }

    pub(crate) fn file_id(&self) -> FileId {
        self.file_id
    }

    pub(crate) fn lower_path(&self, ast: ast::Path) -> Path {
        Path::lower(ast)
    }

    pub(crate) fn ast_id<N: ast::AstNode>(&self, item: &N) -> FileAstId<N> {
        self.source_ast_id_map.ast_id(item)
    }
}

pub(super) fn lower(
    db: &dyn DefDatabase,
    params: Option<ast::AstChildren<ast::Pat>>,
    body: Option<ast::Expr>,
    file_id: FileId,
    module: ModuleId,
) -> (Body, BodySourceMap) {
    ExprCollector {
        db,
        file_id,
        module: module.local_id,
        def_map: db.def_map(module.lib),
        source_map: BodySourceMap::default(),
        body: Body {
            exprs: Arena::default(),
            pats: Arena::default(),
            params: Vec::new(),
            body_expr: dummy_expr_id(),
            type_map: TypeMap::default(),
        },
        type_builder: TypeMapBuilder::default(),
    }
    .collect(params, body)
}

struct ExprCollector<'a> {
    db: &'a dyn DefDatabase,
    body: Body,
    source_map: BodySourceMap,
    def_map: Arc<DefMap>,
    module: LocalModuleId,
    file_id: FileId,
    type_builder: TypeMapBuilder,
}

impl<'a> ExprCollector<'a> {
    fn collect(mut self, params: Option<ast::AstChildren<ast::Pat>>, body: Option<ast::Expr>) -> (Body, BodySourceMap) {
        if let Some(params) = params {
            for param in params {
                let pat = self.collect_pat(param);

                self.body.params.push(pat);
            }
        }

        self.body.body_expr = self.collect_expr_opt(body);

        let (type_map, type_source_map) = self.type_builder.finish();

        self.body.type_map = type_map;
        self.source_map.type_source_map = type_source_map;

        (self.body, self.source_map)
    }

    fn ctx(&self) -> LowerCtx {
        LowerCtx::new(self.db, self.file_id)
    }

    fn to_source<T>(&mut self, value: T) -> InFile<T> {
        InFile::new(self.file_id, value)
    }

    fn alloc_expr(&mut self, expr: Expr, ptr: ExprPtr) -> ExprId {
        let src = self.to_source(ptr);
        let id = self.make_expr(expr, Ok(src.clone()));

        self.source_map.expr_map.insert(src, id);
        id
    }

    fn alloc_expr_desugared(&mut self, expr: Expr) -> ExprId {
        self.make_expr(expr, Err(SyntheticSyntax))
    }

    fn missing_expr(&mut self) -> ExprId {
        self.alloc_expr_desugared(Expr::Missing)
    }

    fn make_expr(&mut self, expr: Expr, src: Result<ExprSource, SyntheticSyntax>) -> ExprId {
        let id = self.body.exprs.alloc(expr);

        self.source_map.expr_map_back.insert(id, src);
        id
    }

    fn alloc_pat(&mut self, pat: Pat, ptr: PatPtr) -> PatId {
        let src = self.to_source(ptr);
        let id = self.make_pat(pat, Ok(src.clone()));

        self.source_map.pat_map.insert(src, id);
        id
    }

    fn alloc_pat_desugared(&mut self, pat: Pat) -> PatId {
        self.make_pat(pat, Err(SyntheticSyntax))
    }

    fn missing_pat(&mut self) -> PatId {
        self.make_pat(Pat::Missing, Err(SyntheticSyntax))
    }

    fn make_pat(&mut self, pat: Pat, src: Result<PatSource, SyntheticSyntax>) -> PatId {
        let id = self.body.pats.alloc(pat);

        self.source_map.pat_map_back.insert(id, src);
        id
    }

    fn collect_expr(&mut self, expr: ast::Expr) -> ExprId {
        self.maybe_collect_expr(expr).unwrap_or_else(|| self.missing_expr())
    }

    fn maybe_collect_expr(&mut self, expr: ast::Expr) -> Option<ExprId> {
        let syntax_ptr = AstPtr::new(&expr);

        Some(match expr {
            | ast::Expr::Typed(e) => {
                let expr = self.collect_expr_opt(e.expr());
                let ty = self.type_builder.alloc_type_ref_opt(e.ty());

                self.alloc_expr(Expr::Typed { expr, ty }, syntax_ptr)
            },
            | ast::Expr::App(e) => {
                let base = self.collect_expr_opt(e.base());
                let arg = self.collect_expr_opt(e.arg());

                self.alloc_expr(Expr::App { base, arg }, syntax_ptr)
            },
            | ast::Expr::Field(e) => {
                let base = self.collect_expr_opt(e.base());
                let field = e.field()?.as_name();

                self.alloc_expr(Expr::Field { base, field }, syntax_ptr)
            },
            | ast::Expr::Deref(e) => {
                let expr = self.collect_expr_opt(e.expr());

                self.alloc_expr(Expr::Deref { expr }, syntax_ptr)
            },
            | ast::Expr::Path(e) => {
                let path = e
                    .path()
                    .map(Path::lower)
                    .map(|path| Expr::Path { path })
                    .unwrap_or(Expr::Missing);

                self.alloc_expr(path, syntax_ptr)
            },
            | ast::Expr::Lit(e) => {
                let lit = match e.literal()? {
                    | ast::Literal::Int(l) => Literal::Int(l.value()?),
                    | ast::Literal::Float(l) => Literal::Float(l.value()?.to_bits()),
                    | ast::Literal::Char(l) => Literal::Char(l.value()?),
                    | ast::Literal::String(l) => Literal::String(l.value()?),
                };

                self.alloc_expr(Expr::Lit { lit }, syntax_ptr)
            },
            | ast::Expr::Infix(e) => {
                if let Some(path) = e.path() {
                    let path = Path::lower(path);
                    let lhs = self.collect_expr_opt(e.lhs());
                    let rhs = self.collect_expr_opt(e.rhs());
                    let base = self.alloc_expr(Expr::Path { path }, syntax_ptr.clone());
                    let base = self.alloc_expr(Expr::App { base, arg: lhs }, syntax_ptr.clone());

                    self.alloc_expr(Expr::App { base, arg: rhs }, syntax_ptr)
                } else {
                    let op = e.op()?.as_name();
                    let op = Path::from(op);
                    let lhs = self.collect_expr_opt(e.lhs());
                    let rhs = self.collect_expr_opt(e.rhs());

                    self.alloc_expr(Expr::Infix { op, lhs, rhs }, syntax_ptr)
                }
            },
            | ast::Expr::Parens(e) => {
                let inner = self.collect_expr_opt(e.expr());
                let src = self.to_source(syntax_ptr);

                self.source_map.expr_map.insert(src, inner);
                inner
            },
            | ast::Expr::Tuple(e) => {
                let exprs = e.exprs().map(|e| self.collect_expr(e)).collect();

                self.alloc_expr(Expr::Tuple { exprs }, syntax_ptr)
            },
            | ast::Expr::Record(e) => {
                let fields = e
                    .fields()
                    .filter_map(|f| {
                        Some(match f {
                            | ast::Field::Normal(f) => RecordField {
                                name: f.name()?.as_name(),
                                val: self.collect_expr_opt(f.expr()),
                            },
                            | ast::Field::Pun(f) => {
                                let name = f.name()?.as_name();
                                let path = Path::from(name.clone());
                                let val = self.alloc_expr(Expr::Path { path }, syntax_ptr.clone());

                                RecordField { name, val }
                            },
                        })
                    })
                    .collect();

                self.alloc_expr(Expr::Record { fields }, syntax_ptr)
            },
            | ast::Expr::Array(e) => {
                let exprs = e.exprs().map(|e| self.collect_expr(e)).collect();

                self.alloc_expr(Expr::Array { exprs }, syntax_ptr)
            },
            | ast::Expr::Do(e) => {
                let stmts = e.block()?.statements().map(|s| self.collect_stmt(s)).collect();

                self.alloc_expr(Expr::Do { stmts }, syntax_ptr)
            },
            | ast::Expr::Clos(e) => {
                let pats = e.params().map(|p| self.collect_pat(p)).collect();
                let stmts = e.block()?.statements().map(|s| self.collect_stmt(s)).collect();

                self.alloc_expr(Expr::Clos { pats, stmts }, syntax_ptr)
            },
            | ast::Expr::If(e) => {
                let cond = self.collect_expr_opt(e.cond());
                let then = self.collect_expr_opt(e.then());
                let else_ = e.else_().map(|e| self.collect_expr(e));

                self.alloc_expr(
                    Expr::If {
                        cond,
                        then,
                        else_,
                        inverse: e.is_unless(),
                    },
                    syntax_ptr,
                )
            },
            | ast::Expr::While(e) => {
                let cond = self.collect_expr_opt(e.cond());
                let body = self.collect_expr_opt(e.body());

                self.alloc_expr(
                    Expr::While {
                        cond,
                        body,
                        inverse: e.is_until(),
                    },
                    syntax_ptr,
                )
            },
            | ast::Expr::Loop(e) => {
                let body = self.collect_expr_opt(e.body());

                self.alloc_expr(Expr::Loop { body }, syntax_ptr)
            },
            | ast::Expr::Next(e) => {
                let expr = e.expr().map(|e| self.collect_expr(e));

                self.alloc_expr(Expr::Next { expr }, syntax_ptr)
            },
            | ast::Expr::Break(e) => {
                let expr = e.expr().map(|e| self.collect_expr(e));

                self.alloc_expr(Expr::Break { expr }, syntax_ptr)
            },
            | ast::Expr::Yield(e) => {
                let exprs = e.exprs().map(|e| self.collect_expr(e)).collect();

                self.alloc_expr(Expr::Yield { exprs }, syntax_ptr)
            },
            | ast::Expr::Return(e) => {
                let expr = e.expr().map(|e| self.collect_expr(e));

                self.alloc_expr(Expr::Return { expr }, syntax_ptr)
            },
            | _ => unimplemented!("{:?}", expr),
        })
    }

    fn collect_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = expr {
            self.collect_expr(expr)
        } else {
            self.missing_expr()
        }
    }

    fn collect_stmt(&mut self, stmt: ast::Stmt) -> Stmt {
        match stmt {
            | ast::Stmt::Let(stmt) => {
                let pat = self.collect_pat_opt(stmt.pat());
                let val = self.collect_expr_opt(stmt.expr());

                Stmt::Let { pat, val }
            },
            | ast::Stmt::Bind(stmt) => {
                let pat = self.collect_pat_opt(stmt.pat());
                let val = self.collect_expr_opt(stmt.expr());

                Stmt::Bind { pat, val }
            },
            | ast::Stmt::Expr(stmt) => {
                let expr = self.collect_expr_opt(stmt.expr());

                Stmt::Expr { expr }
            },
        }
    }

    fn collect_pat(&mut self, pat: ast::Pat) -> PatId {
        let ptr = AstPtr::new(&pat);
        let pattern = match pat {
            | ast::Pat::Typed(p) => {
                let pat = self.collect_pat_opt(p.pat());
                let ty = self.type_builder.alloc_type_ref_opt(p.ty());

                Pat::Typed { pat, ty }
            },
            | ast::Pat::Wildcard(_) => Pat::Wildcard,
            | ast::Pat::Bind(pat) => {
                let name = pat.name().map(|n| n.as_name()).unwrap_or_else(Name::missing);
                let subpat = pat.subpat().map(|sp| self.collect_pat(sp));

                if let None = subpat {
                    let (resolved, _) = self.def_map.resolve_path(self.db, self.module, &name.clone().into());

                    match resolved.values {
                        | Some(ModuleDefId::ConstId(_)) | Some(ModuleDefId::CtorId(_)) => {
                            Pat::Path { path: name.into() }
                        },
                        | _ => Pat::Bind { name, subpat },
                    }
                } else {
                    Pat::Bind { name, subpat }
                }
            },
            | ast::Pat::Parens(p) => {
                let inner = self.collect_pat_opt(p.pat());
                let src = self.to_source(ptr);

                self.source_map.pat_map.insert(src, inner);

                return inner;
            },
            | ast::Pat::Tuple(p) => {
                let pats = p.pats().map(|p| self.collect_pat(p)).collect();

                Pat::Tuple { pats }
            },
            | ast::Pat::Record(p) => {
                let fields = p
                    .fields()
                    .filter_map(|f| {
                        Some(match f {
                            | ast::Field::Normal(f) => RecordField {
                                name: f.name()?.as_name(),
                                val: self.collect_pat_opt(f.pat()),
                            },
                            | ast::Field::Pun(f) => {
                                let name = f.name()?.as_name();
                                let val = self.alloc_pat(
                                    Pat::Bind {
                                        name: name.clone(),
                                        subpat: None,
                                    },
                                    ptr.clone(),
                                );

                                RecordField { name, val }
                            },
                        })
                    })
                    .collect();

                Pat::Record { fields }
            },
            | _ => unimplemented!("{:?}", pat),
        };

        self.alloc_pat(pattern, ptr)
    }

    fn collect_pat_opt(&mut self, pat: Option<ast::Pat>) -> PatId {
        if let Some(pat) = pat {
            self.collect_pat(pat)
        } else {
            self.missing_pat()
        }
    }
}

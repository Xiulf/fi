use arena::Arena;
use diagnostics::Diagnostics;
use ra_ap_stdx::hash::NoHashHashMap;
use syntax::ast::{self, Assoc, Prec};
use syntax::ptr::AstPtr;
use triomphe::Arc;
use vfs::File;

use super::{Body, BodySourceMap, ExprSrc, PatSrc};
use crate::def_map::DefMap;
use crate::diagnostics::{UnmatchedPattern, UnreachableBranch, UnresolvedPath};
use crate::expr::{Expr, ExprId, Literal, Stmt};
use crate::id::{CtorId, HasModule, ItemId, TypedItemId, ValueDefId, ValueId};
use crate::item_tree::FixityKind;
use crate::name::{AsName, Name};
use crate::pat::{Constructor, DecisionTree, Pat, PatId, PatternMatrix, PatternStack, VariantTag};
use crate::path::Path;
use crate::per_ns::Namespace;
use crate::source::HasSource;
use crate::type_ref::TypeSourceMap;
use crate::Db;

#[salsa::tracked]
pub fn query(db: &dyn Db, id: ValueId) -> (Arc<Body>, Arc<BodySourceMap>) {
    let src = id.source(db);
    let mut ctx = Ctx::new(db, id, src.file);

    ctx.lower(src.value);
    tracing::debug!("{}", ctx.body.debug(db));

    (Arc::new(ctx.body), Arc::new(ctx.src_map))
}

struct Ctx<'db> {
    db: &'db dyn Db,
    def_map: Arc<DefMap>,
    value: ValueId,
    body: Body,
    src_map: BodySourceMap,
    typ_map: Arc<TypeSourceMap>,
    scopes: Vec<Scope>,
}

#[derive(Default)]
struct Scope {
    boundary: bool,
    names: NoHashHashMap<Name, PatId>,
}

impl<'db> Ctx<'db> {
    fn new(db: &'db dyn Db, value: ValueId, file: File) -> Self {
        let lib = value.container(db).module(db).lib(db);
        let def_map = crate::def_map::query(db, lib);
        let (_, typ_map, _) = TypedItemId::from(value).type_map(db);

        Self {
            db,
            value,
            def_map,
            typ_map,
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
        // use syntax::ast::AstNode;
        // let resolver = self.db.syntax_interner().read().unwrap();
        // tracing::debug!("{}", item.syntax().debug(&*resolver, true));

        self.scopes.push(Scope::default());
        let params = item.params().map(|p| self.lower_pat(p)).collect();
        let expr = self.lower_expr_opt(item.body());

        self.body.params = params;
        self.body.body_expr = expr;
    }

    fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr<ast::Expr>) -> ExprId {
        let id = self.make_expr(expr, ExprSrc::Single(ptr));

        self.src_map.src_to_expr.insert(ptr, id);
        id
    }

    fn alloc_expr_desugared(&mut self, expr: Expr) -> ExprId {
        self.make_expr(expr, ExprSrc::Synthetic(self.value))
    }

    fn make_expr(&mut self, expr: Expr, src: ExprSrc) -> ExprId {
        let id = self.body.exprs.alloc(expr);

        self.src_map.expr_to_src.insert(id, src);
        id
    }

    fn missing_expr(&mut self) -> ExprId {
        self.alloc_expr_desugared(Expr::Missing)
    }

    fn alloc_pat(&mut self, pat: Pat, ptr: AstPtr<ast::Pat>) -> PatId {
        let id = self.make_pat(pat, PatSrc::Single(ptr));

        self.src_map.src_to_pat.insert(ptr, id);
        id
    }

    fn alloc_pat_desugared(&mut self, pat: Pat) -> PatId {
        self.make_pat(pat, PatSrc::Synthetic(self.value))
    }

    fn make_pat(&mut self, pat: Pat, src: PatSrc) -> PatId {
        let id = self.body.pats.alloc(pat);

        self.src_map.pat_to_src.insert(id, src);
        id
    }

    fn missing_pat(&mut self) -> PatId {
        self.alloc_pat_desugared(Pat::Missing)
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
            | ast::Expr::Parens(e) => self.lower_expr_opt(e.expr()),
            | ast::Expr::Typed(e) => {
                let expr = self.lower_expr_opt(e.expr());
                let ty = e.ty().and_then(|t| self.typ_map.typ_for_src(AstPtr::new(&t)));

                if let Some(ty) = ty {
                    self.alloc_expr(Expr::Typed { expr, ty }, syntax_ptr)
                } else {
                    expr
                }
            },
            | ast::Expr::Unit(_) => {
                let lib = self.value.container(self.db).module(self.db).lib(self.db);
                let unit = crate::lang_item::query(self.db, lib, "unit-type")?;
                let unit = unit.as_type_ctor()?;
                let it = unit.it(self.db);
                let item_tree = crate::item_tree::query(self.db, it.file);
                let data = &item_tree[it.value];
                let unit = CtorId::new(self.db, unit, data.ctors[0]);

                self.alloc_expr(
                    Expr::Path {
                        path: Path::default(),
                        def: Some(ValueDefId::CtorId(unit)),
                    },
                    syntax_ptr,
                )
            },
            | ast::Expr::Literal(e) => {
                let resolver = self.db.syntax_interner().read();
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
            | ast::Expr::Lambda(_) => {
                self.scopes.push(Scope {
                    boundary: true,
                    ..Scope::default()
                });

                let _ = self.scopes.last().unwrap().boundary;
                todo!()
            },
            | ast::Expr::App(e) => {
                let base = self.lower_expr_opt(e.base());
                let args = e.args().map(|e| self.lower_expr(e)).collect();

                self.alloc_expr(Expr::App { base, args }, syntax_ptr)
            },
            | ast::Expr::Infix(e) => {
                let exprs = e.exprs().map(|e| self.lower_expr(e)).collect::<Vec<_>>();
                let ops = e
                    .ops()
                    .map(|p| (Path::from_ast(self.db, p.clone()), p))
                    .collect::<Vec<_>>();

                self.lower_infix(
                    exprs,
                    ops,
                    |ctx| ctx.missing_expr(),
                    |ctx, def, path, path_src, lhs, rhs| {
                        let base = ctx.make_expr(Expr::Path { path, def }, ExprSrc::Operator(AstPtr::new(&path_src)));
                        let args = Box::new([lhs, rhs]);
                        let lhs_ptr = ctx.src_map.expr_to_src[lhs].as_expr_ptr(true);
                        let rhs_ptr = ctx.src_map.expr_to_src[rhs].as_expr_ptr(false);

                        ctx.make_expr(Expr::App { base, args }, ExprSrc::Infix(lhs_ptr, rhs_ptr))
                    },
                )
            },
            | ast::Expr::Block(e) => {
                let (stmts, expr) = self.lower_block(e.statements());

                self.alloc_expr(Expr::Block { stmts, expr }, syntax_ptr)
            },
            | ast::Expr::Match(e) => {
                let (expr, branches, decision_tree) = self.lower_match(e);

                self.alloc_expr(
                    Expr::Match {
                        expr,
                        branches,
                        decision_tree,
                    },
                    syntax_ptr,
                )
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
            | ast::Stmt::Let(s) => {
                self.scopes.push(Scope::default());
                let pat = self.lower_pat_opt(s.pat());
                let val = self.lower_expr_opt(s.expr());

                Stmt::Let(pat, val)
            },
            | ast::Stmt::Bind(_) => todo!(),
            | ast::Stmt::Expr(s) => Stmt::Expr(self.lower_expr_opt(s.expr())),
        }
    }

    fn lower_match(&mut self, e: ast::ExprMatch) -> (ExprId, Box<[(PatId, ExprId)]>, DecisionTree) {
        let expr = self.lower_expr_opt(e.expr());
        let mut matrix = self.lower_pattern_matrix(e.arms());
        let branches = e
            .arms()
            .zip(matrix.rows.iter())
            .map(|(a, r)| ((r.0).0[0].1, self.lower_expr_opt(a.expr())))
            .collect::<Box<[_]>>();
        let result = matrix.compile(self.db);

        if result.reachable_branches.len() != branches.len() {
            for (i, arm) in e.arms().enumerate() {
                if !result.reachable_branches.contains(&i) {
                    Diagnostics::emit(self.db, UnreachableBranch {
                        file: self.src_map.file,
                        ast: AstPtr::new(&arm),
                    });
                }
            }
        }

        if result.missed_case_count > 0 {
            Diagnostics::emit(self.db, UnmatchedPattern {
                file: self.src_map.file,
                ast: AstPtr::new(&e),
            });
        }

        (expr, branches, result.tree)
    }

    fn lower_pattern_matrix(&mut self, arms: impl Iterator<Item = ast::MatchArm>) -> PatternMatrix {
        let rows = arms
            .enumerate()
            .map(|(i, arm)| (self.lower_pattern_stack_opt(arm.pat()), i))
            .collect();

        PatternMatrix { rows }
    }

    fn lower_pattern_stack_opt(&mut self, pat: Option<ast::Pat>) -> PatternStack {
        pat.map(|p| self.lower_pattern_stack(p)).unwrap_or_default()
    }

    fn lower_pattern_stack(&mut self, pat: ast::Pat) -> PatternStack {
        self.maybe_lower_pattern_stack(pat).unwrap_or_default()
    }

    fn maybe_lower_pattern_stack(&mut self, pat: ast::Pat) -> Option<PatternStack> {
        let syntax_ptr = AstPtr::new(&pat);

        Some(match pat {
            | ast::Pat::Typed(p) => {
                let ty = p.ty().and_then(|t| self.typ_map.typ_for_src(AstPtr::new(&t)));
                let mut stack = self.lower_pattern_stack_opt(p.pat());

                if let Some(ty) = ty {
                    let pat = stack.head()?.1;
                    let id = self.alloc_pat(Pat::Typed { pat, ty }, syntax_ptr);

                    stack.set_id(id);
                }

                stack
            },
            | ast::Pat::Wildcard(_) => {
                let id = self.alloc_pat(Pat::Wildcard, syntax_ptr);

                PatternStack(vec![(Constructor::MatchAll, id)])
            },
            | ast::Pat::Literal(p) => {
                let resolver = self.db.syntax_interner().read();
                let lit = match p.literal()? {
                    | ast::Literal::Int(l) => Literal::Int(l.value(&*resolver)?),
                    | _ => todo!(),
                };

                let id = self.alloc_pat(Pat::Lit { lit: lit.clone() }, syntax_ptr);
                let tag = VariantTag::Literal(lit);
                let fields = PatternStack(vec![(Constructor::MatchAll, id)]);

                PatternStack(vec![(Constructor::Variant(tag, fields), id)])
            },
            | ast::Pat::Bind(p) => {
                let name = p.name()?.as_name(self.db);
                let subpat = p.subpat().map(|p| self.lower_pat(p)); // TODO: lower to pattern stack
                let id = self.alloc_pat(Pat::Bind { name, subpat }, syntax_ptr);

                self.scopes.last_mut().unwrap().names.insert(name, id);

                PatternStack(vec![(Constructor::MatchAll, id)])
            },
            | ast::Pat::Path(p) => {
                let epath = p.path()?;
                let path = Path::from_ast(self.db, epath.clone());
                let def = self.resolve_path(&path, &epath);
                let ctor = self.lower_pat_ctor(def);
                let id = self.alloc_pat(
                    Pat::Ctor {
                        path,
                        ctor,
                        args: Box::new([]),
                    },
                    syntax_ptr,
                );

                if let None = ctor {
                    return Some(PatternStack(vec![(Constructor::MatchAll, id)]));
                }

                PatternStack(vec![(
                    Constructor::Variant(VariantTag::Ctor(ctor.unwrap()), PatternStack(Vec::new())),
                    id,
                )])
            },
            | ast::Pat::App(p) => {
                let base = p.base()?;
                let epath = base.path()?;
                let path = Path::from_ast(self.db, epath.clone());
                let def = self.resolve_path(&path, &epath);
                let ctor = self.lower_pat_ctor(def);
                let fields = p.args().collect::<Vec<_>>().into_iter();
                let fields = fields
                    .flat_map(|p| self.lower_pattern_stack(p))
                    .rev()
                    .collect::<Vec<_>>();
                let args = fields.iter().map(|f| f.1).collect();
                let id = self.alloc_pat(Pat::Ctor { path, ctor, args }, syntax_ptr);

                if let None = ctor {
                    return Some(PatternStack(vec![(Constructor::MatchAll, id)]));
                }

                PatternStack(vec![(
                    Constructor::Variant(VariantTag::Ctor(ctor.unwrap()), PatternStack(fields)),
                    id,
                )])
            },
            | p => todo!("{p:?}"),
        })
    }

    fn lower_pat_opt(&mut self, pat: Option<ast::Pat>) -> PatId {
        pat.map(|p| self.lower_pat(p)).unwrap_or_else(|| self.missing_pat())
    }

    fn lower_pat(&mut self, pat: ast::Pat) -> PatId {
        self.maybe_lower_pat(pat).unwrap_or_else(|| self.missing_pat())
    }

    fn maybe_lower_pat(&mut self, pat: ast::Pat) -> Option<PatId> {
        let syntax_ptr = AstPtr::new(&pat);

        Some(match pat {
            | ast::Pat::Parens(p) => self.lower_pat_opt(p.pat()),
            | ast::Pat::Typed(p) => {
                let pat = self.lower_pat_opt(p.pat());
                let ty = p.ty().and_then(|t| self.typ_map.typ_for_src(AstPtr::new(&t)));

                if let Some(ty) = ty {
                    self.alloc_pat(Pat::Typed { pat, ty }, syntax_ptr)
                } else {
                    pat
                }
            },
            | ast::Pat::Unit(_) => {
                let lib = self.value.container(self.db).module(self.db).lib(self.db);
                let unit = crate::lang_item::query(self.db, lib, "unit-type")?;
                let unit = unit.as_type_ctor()?;
                let it = unit.it(self.db);
                let item_tree = crate::item_tree::query(self.db, it.file);
                let data = &item_tree[it.value];
                let unit = CtorId::new(self.db, unit, data.ctors[0]);

                self.alloc_pat(
                    Pat::Ctor {
                        path: Path::default(),
                        ctor: Some(unit),
                        args: Box::new([]),
                    },
                    syntax_ptr,
                )
            },
            | ast::Pat::Literal(p) => {
                let resolver = self.db.syntax_interner().read();
                let lit = match p.literal()? {
                    | ast::Literal::Int(l) => Literal::Int(l.value(&*resolver)?),
                    | _ => todo!(),
                };

                self.alloc_pat(Pat::Lit { lit }, syntax_ptr)
            },
            | ast::Pat::Bind(p) => {
                let name = p.name()?.as_name(self.db);
                let subpat = p.subpat().map(|p| self.lower_pat(p));
                let id = self.alloc_pat(Pat::Bind { name, subpat }, syntax_ptr);

                self.scopes.last_mut().unwrap().names.insert(name, id);
                id
            },
            | ast::Pat::Path(p) => {
                let epath = p.path()?;
                let path = Path::from_ast(self.db, epath.clone());
                let def = self.resolve_path(&path, &epath);
                let ctor = self.lower_pat_ctor(def);

                self.alloc_pat(
                    Pat::Ctor {
                        path,
                        ctor,
                        args: Box::new([]),
                    },
                    syntax_ptr,
                )
            },
            | ast::Pat::App(p) => {
                let base = p.base()?;
                let epath = base.path()?;
                let path = Path::from_ast(self.db, epath.clone());
                let def = self.resolve_path(&path, &epath);
                let ctor = self.lower_pat_ctor(def);
                let args = p.args().map(|p| self.lower_pat(p)).collect();

                self.alloc_pat(Pat::Ctor { path, ctor, args }, syntax_ptr)
            },
            | ast::Pat::Infix(p) => {
                let pats = p.pats().map(|e| self.lower_pat(e)).collect::<Vec<_>>();
                let ops = p
                    .ops()
                    .map(|p| (Path::from_ast(self.db, p.clone()), p))
                    .collect::<Vec<_>>();

                self.lower_infix(
                    pats,
                    ops,
                    |ctx| ctx.missing_pat(),
                    |ctx, def, path, _, lhs, rhs| {
                        let lhs_ptr = ctx.src_map.pat_to_src[lhs].as_pat_ptr(true);
                        let rhs_ptr = ctx.src_map.pat_to_src[rhs].as_pat_ptr(false);
                        let args = Box::new([lhs, rhs]);
                        let ctor = ctx.lower_pat_ctor(def);

                        ctx.make_pat(Pat::Ctor { path, ctor, args }, PatSrc::Infix(lhs_ptr, rhs_ptr))
                    },
                )
            },
            | p => todo!("{p:?}"),
        })
    }

    fn lower_pat_ctor(&self, def: Option<ValueDefId>) -> Option<CtorId> {
        match def? {
            | ValueDefId::CtorId(id) => Some(id),
            | _ => None, // TODO: report error
        }
    }

    fn lower_infix<T>(
        &mut self,
        items: Vec<T>,
        ops: Vec<(Path, ast::Path)>,
        mut missing: impl FnMut(&mut Self) -> T,
        mut mk_app: impl FnMut(&mut Self, Option<ValueDefId>, Path, ast::Path, T, T) -> T,
    ) -> T {
        let fixities = ops
            .iter()
            .map(|(op, src)| {
                let resolved = self.resolve_path(op, src);
                let (prec, assoc) = resolved
                    .map(|def| match def {
                        | ValueDefId::FixityId(id) => {
                            let data = crate::data::fixity_data(self.db, id);
                            match data.kind(self.db) {
                                | FixityKind::Infix(assoc, prec) => (prec, assoc),
                                | _ => (Prec::ZERO, Assoc::Left), // TODO: report error
                            }
                        },
                        | _ => (Prec::ZERO, Assoc::Left),
                    })
                    .unwrap_or((Prec::ZERO, Assoc::Left));

                (resolved, prec, assoc)
            })
            .collect::<Vec<_>>();

        return go(
            self,
            ops.into_iter(),
            fixities.into_iter().peekable(),
            items.into_iter(),
            &mut missing,
            &mut mk_app,
        );

        use std::iter::{once, Peekable};

        fn go<'a, T>(
            ctx: &mut Ctx<'a>,
            mut ops: impl Iterator<Item = (Path, ast::Path)>,
            mut fixities: Peekable<impl Iterator<Item = (Option<ValueDefId>, Prec, Assoc)>>,
            mut items: impl Iterator<Item = T>,
            missing: &mut dyn FnMut(&mut Ctx<'a>) -> T,
            mk_app: &mut dyn FnMut(&mut Ctx<'a>, Option<ValueDefId>, Path, ast::Path, T, T) -> T,
        ) -> T {
            if let Some((op, op_src)) = ops.next() {
                let (id, prec, assoc) = fixities.next().unwrap();
                let left = if let Some(&(id2, prec2, _)) = fixities.peek() {
                    if id == id2 {
                        match assoc {
                            | Assoc::Left => true,
                            | Assoc::Right => false,
                            | Assoc::None => todo!(),
                        }
                    } else if prec >= prec2 {
                        true
                    } else {
                        false
                    }
                } else {
                    let lhs = items.next().unwrap_or_else(|| missing(ctx));
                    let rhs = items.next().unwrap_or_else(|| missing(ctx));

                    return mk_app(ctx, id, op, op_src, lhs, rhs);
                };

                if left {
                    let lhs = items.next().unwrap_or_else(|| missing(ctx));
                    let rhs = items.next().unwrap_or_else(|| missing(ctx));
                    let item = mk_app(ctx, id, op, op_src, lhs, rhs);
                    let items = once(item).chain(items).collect::<Vec<_>>();

                    go(ctx, ops, fixities, items.into_iter(), missing, mk_app)
                } else {
                    let lhs = items.next().unwrap_or_else(|| missing(ctx));
                    let rhs = go(ctx, ops, fixities, items, missing, mk_app);

                    mk_app(ctx, id, op, op_src, lhs, rhs)
                }
            } else {
                items.next().unwrap_or_else(|| missing(ctx))
            }
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

        let module = self.value.container(self.db).module(self.db);

        if let Some(item) = self.def_map.resolve_path(self.db, path, module) {
            if let None = item.values {
                // TODO: report error: not a value
                return None;
            }

            let id = match item.values.unwrap() {
                | ItemId::FixityId(id) => ValueDefId::FixityId(id),
                | ItemId::ValueId(id) => ValueDefId::ValueId(id),
                | ItemId::CtorId(id) => ValueDefId::CtorId(id),
                | ItemId::FieldId(id) => ValueDefId::FieldId(id),
                | _ => unreachable!(),
            };

            return Some(id);
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

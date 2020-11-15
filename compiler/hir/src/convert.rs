use crate::ir;
use crate::resolve::{Ns, Resolver};
use crate::HirDatabase;
use std::collections::BTreeMap;
use std::sync::Arc;
use syntax::ast;

pub fn convert(db: &dyn HirDatabase, file: source::FileId) -> Arc<ir::Module> {
    let ast = db.parse(file);
    let lib = db.file_lib(file);
    let mut converter = Converter::new(db, lib, file, ast.name);

    converter.convert_ast(&ast);

    if db.has_errors() {
        db.print_and_exit();
    } else {
        db.print();
    }

    converter.finish(&ast)
}

pub struct Converter<'db> {
    db: &'db (dyn HirDatabase + 'db),
    resolver: Resolver<'db>,
    lib: source::LibId,
    module_name: ir::Ident,
    id_counter: u32,
    current_item: ir::DefId,
    items: BTreeMap<ir::HirId, ir::Item>,
    iface_items: BTreeMap<ir::IfaceItemId, ir::IfaceItem>,
    impl_items: BTreeMap<ir::ImplItemId, ir::ImplItem>,
    bodies: BTreeMap<ir::BodyId, ir::Body>,
}

impl<'db> Converter<'db> {
    pub fn new(
        db: &'db (dyn HirDatabase + 'db),
        lib: source::LibId,
        file: source::FileId,
        module_name: ir::Ident,
    ) -> Self {
        Converter {
            db,
            lib,
            module_name,
            resolver: Resolver::new(db.to_diag_db(), file),
            id_counter: 0,
            current_item: ir::DefId::dummy(),
            items: BTreeMap::new(),
            iface_items: BTreeMap::new(),
            impl_items: BTreeMap::new(),
            bodies: BTreeMap::new(),
        }
    }

    pub fn finish(self, ast: &ast::Module) -> Arc<ir::Module> {
        Arc::new(ir::Module {
            id: ir::ModuleId::from_name(ast.name.symbol),
            span: ast.span,
            attrs: ast.attrs.clone(),
            name: ast.name,
            body_ids: self.bodies.keys().copied().collect(),
            items: self.items,
            iface_items: self.iface_items,
            impl_items: self.impl_items,
            bodies: self.bodies,
        })
    }

    fn next_id(&mut self) -> ir::HirId {
        let local_id = ir::LocalId(self.id_counter);

        self.id_counter += 1;

        ir::HirId {
            owner: self.current_item,
            local_id,
        }
    }

    pub fn convert_ast(&mut self, ast: &ast::Module) {
        self.register_decls(ast.decl_groups());
        self.convert_decls(ast.decl_groups());
    }

    fn register_decls(&mut self, decl_groups: ast::DeclGroups) {
        for (_, group) in decl_groups {
            self.register_decl(group);
        }
    }

    fn register_decl(&mut self, group: &[ast::Decl]) {
        let (defpath, defkind, ns) = match &group[0].kind {
            ast::DeclKind::FuncTy { .. } | ast::DeclKind::Func { .. } => (
                ir::DefPath::Value(group[0].name.symbol),
                ir::DefKind::Func,
                Ns::Values,
            ),
            ast::DeclKind::ConstTy { .. } | ast::DeclKind::Const { .. } => (
                ir::DefPath::Value(group[0].name.symbol),
                ir::DefKind::Const,
                Ns::Values,
            ),
            ast::DeclKind::StaticTy { .. } | ast::DeclKind::Static { .. } => (
                ir::DefPath::Value(group[0].name.symbol),
                ir::DefKind::Static,
                Ns::Values,
            ),
            ast::DeclKind::AliasKind { .. } | ast::DeclKind::Alias { .. } => (
                ir::DefPath::Type(group[0].name.symbol),
                ir::DefKind::Alias,
                Ns::Types,
            ),
            ast::DeclKind::DataKind { .. } | ast::DeclKind::Data { .. } => (
                ir::DefPath::Type(group[0].name.symbol),
                ir::DefKind::Data,
                Ns::Types,
            ),
            ast::DeclKind::Iface { .. } => (
                ir::DefPath::Type(group[0].name.symbol),
                ir::DefKind::Iface,
                Ns::Types,
            ),
            ast::DeclKind::ImplChain { .. } => return,
        };

        let defindex = ir::DefIndex::from_path(self.module_name.symbol, &[defpath]);
        let defid = ir::DefId::new(self.lib, defindex);

        self.resolver
            .define(ns, group[0].name, ir::Res::Def(defkind, defid));

        for decl in group {
            match &decl.kind {
                ast::DeclKind::Data {
                    body: Some(ctors), ..
                } => {
                    for ctor in ctors {
                        let defindex = ir::DefIndex::from_path(
                            self.module_name.symbol,
                            &[defpath, ir::DefPath::Value(ctor.name.symbol)],
                        );

                        let defid = ir::DefId::new(self.lib, defindex);

                        self.resolver.define(
                            Ns::Values,
                            ctor.name,
                            ir::Res::Def(ir::DefKind::Ctor, defid),
                        );
                    }
                }
                ast::DeclKind::Iface {
                    body: Some(body), ..
                } => {
                    for decl in &body.decls {
                        let defindex = ir::DefIndex::from_path(
                            self.module_name.symbol,
                            &[defpath, ir::DefPath::Value(decl.name.symbol)],
                        );

                        let defid = ir::DefId::new(self.lib, defindex);

                        self.resolver.define(
                            Ns::Values,
                            decl.name,
                            ir::Res::Def(ir::DefKind::Func, defid),
                        );
                    }
                }
                _ => {}
            }
        }
    }

    fn convert_decls(&mut self, groups: ast::DeclGroups) {
        for (kind, decls) in groups {
            self.convert_decl(kind, decls);
        }
    }

    fn convert_decl(&mut self, kind: ast::DeclGroupKind, decls: &[ast::Decl]) {
        let first = &decls[0];
        let defindex = ir::DefIndex::from_path(
            self.module_name.symbol,
            &[match kind {
                ast::DeclGroupKind::Func
                | ast::DeclGroupKind::Const
                | ast::DeclGroupKind::Static => ir::DefPath::Value(first.name.symbol),
                ast::DeclGroupKind::Alias
                | ast::DeclGroupKind::Data
                | ast::DeclGroupKind::Iface => ir::DefPath::Type(first.name.symbol),
                ast::DeclGroupKind::Impl => return,
            }],
        );

        self.current_item = ir::DefId::new(self.lib, defindex);
        self.id_counter = 0;

        let id = self.next_id();
        let span = first.span.merge(decls.last().unwrap().span);
        let kind = match kind {
            ast::DeclGroupKind::Func => {
                let mut ty = None;
                let mut params = None::<Vec<ir::Param>>;
                let mut arms = Vec::new();
                let body_id = ir::BodyId(self.next_id());

                for decl in decls {
                    match &decl.kind {
                        ast::DeclKind::FuncTy { ty: fty } => {
                            if let None = ty {
                                ty = Some(self.convert_type(fty));
                            }
                        }
                        ast::DeclKind::Func { pats, val } => {
                            if let None = params {
                                params = Some(
                                    (0..pats.len())
                                        .map(|_| ir::Param { id: self.next_id() })
                                        .collect(),
                                );
                            }

                            arms.push(ir::CaseArm {
                                id: self.next_id(),
                                span: if pats.is_empty() {
                                    decl.name.span
                                } else {
                                    pats[0].span.merge(pats.last().unwrap().span)
                                },
                                pats: pats.iter().map(|p| self.convert_pat(p)).collect(),
                                val: self.convert_guarded(val),
                            });
                        }
                        _ => unreachable!(),
                    }
                }

                let ty = ty.unwrap_or_else(|| ir::Type {
                    id: self.next_id(),
                    span: first.name.span,
                    kind: ir::TypeKind::Infer,
                });

                let value = ir::Expr {
                    id: self.next_id(),
                    span,
                    kind: ir::ExprKind::Case {
                        arms,
                        pred: params
                            .as_ref()
                            .unwrap()
                            .iter()
                            .map(|p| ir::Expr {
                                id: self.next_id(),
                                span,
                                kind: ir::ExprKind::Ident {
                                    res: ir::Res::Local(p.id),
                                },
                            })
                            .collect(),
                    },
                };

                self.bodies.insert(
                    body_id,
                    ir::Body {
                        id: body_id,
                        params: params.unwrap(),
                        value,
                    },
                );

                ir::ItemKind::Func { ty, body: body_id }
            }
            _ => return,
        };

        let item = ir::Item {
            id,
            span,
            attrs: decls.iter().flat_map(|d| d.attrs.iter().cloned()).collect(),
            name: first.name,
            kind,
        };

        self.items.insert(id, item);
    }

    fn convert_pat(&mut self, pat: &ast::Pat) -> ir::Pat {
        if let ast::PatKind::Parens { inner } = &pat.kind {
            return self.convert_pat(inner);
        }

        let id = self.next_id();
        let kind = match pat.kind {
            ast::PatKind::Parens { .. } => unreachable!(),
            ast::PatKind::Wildcard => ir::PatKind::Wildcard,
            ast::PatKind::Int { val } => ir::PatKind::Int { val },
            ast::PatKind::Float { bits } => ir::PatKind::Float { bits },
            ast::PatKind::Char { val } => ir::PatKind::Char { val },
            ast::PatKind::Str { ref val } => ir::PatKind::Str { val: val.clone() },
            ast::PatKind::Ident { name } => {
                self.resolver.define(Ns::Values, name, ir::Res::Local(id));

                ir::PatKind::Bind { name, sub: None }
            }
            ast::PatKind::Named { name, ref pat } => {
                self.resolver.define(Ns::Values, name, ir::Res::Local(id));

                ir::PatKind::Bind {
                    name,
                    sub: Some(Box::new(self.convert_pat(pat))),
                }
            }
            ast::PatKind::Ctor { name, ref pats } => {
                match self.resolver.get(Ns::Values, name.symbol) {
                    Some(ir::Res::Def(ir::DefKind::Ctor, ctor)) => ir::PatKind::Ctor {
                        ctor,
                        pats: pats.iter().map(|p| self.convert_pat(p)).collect(),
                    },
                    Some(res) => ir::PatKind::Error,
                    None => ir::PatKind::Error,
                }
            }
            ast::PatKind::Array { ref pats } => ir::PatKind::Array {
                pats: pats.iter().map(|p| self.convert_pat(p)).collect(),
            },
            ast::PatKind::Tuple { ref pats } => ir::PatKind::Tuple {
                pats: pats.iter().map(|p| self.convert_pat(p)).collect(),
            },
            ast::PatKind::Record { ref fields } => ir::PatKind::Record {
                fields: fields.iter().map(|f| self.convert_field_pat(f)).collect(),
            },
            ast::PatKind::Typed { ref pat, ref ty } => ir::PatKind::Typed {
                pat: Box::new(self.convert_pat(pat)),
                ty: self.convert_type(ty),
            },
        };

        ir::Pat {
            id,
            span: pat.span,
            kind,
        }
    }

    fn convert_field_pat(
        &mut self,
        field: &ast::RecordField<ast::Pat>,
    ) -> ir::RecordField<ir::Pat> {
        match *field {
            ast::RecordField::Field { name, ref val } => ir::RecordField {
                id: self.next_id(),
                span: name.span.merge(val.span),
                name,
                val: self.convert_pat(val),
            },
            ast::RecordField::Pun { name } => {
                let field_id = self.next_id();
                let pat_id = self.next_id();

                self.resolver
                    .define(Ns::Values, name, ir::Res::Local(pat_id));

                ir::RecordField {
                    id: field_id,
                    span: name.span,
                    name,
                    val: ir::Pat {
                        id: pat_id,
                        span: name.span,
                        kind: ir::PatKind::Bind { name, sub: None },
                    },
                }
            }
        }
    }

    fn convert_guarded(&mut self, guarded: &ast::Guarded) -> ir::Guarded {
        match guarded {
            ast::Guarded::Unconditional(expr) => {
                ir::Guarded::Unconditional(self.convert_expr(expr))
            }
            ast::Guarded::Guarded(guards) => {
                ir::Guarded::Guarded(guards.iter().map(|g| self.convert_guard(g)).collect())
            }
        }
    }

    fn convert_guard(&mut self, guard: &ast::GuardedExpr) -> ir::GuardedExpr {
        ir::GuardedExpr {
            id: self.next_id(),
            span: guard.span,
            guard: self.convert_expr(&guard.guard),
            val: self.convert_expr(&guard.val),
        }
    }

    fn convert_expr(&mut self, expr: &ast::Expr) -> ir::Expr {
        if let ast::ExprKind::Parens { inner } = &expr.kind {
            return self.convert_expr(inner);
        }

        let id = self.next_id();
        let kind = match expr.kind {
            ast::ExprKind::Parens { .. } => unreachable!(),
            ast::ExprKind::Hole { name } => ir::ExprKind::Hole { name },
            ast::ExprKind::Ident { name } => match self.resolver.get(Ns::Values, name.symbol) {
                Some(res) => ir::ExprKind::Ident { res },
                None => ir::ExprKind::Error,
            },
            ast::ExprKind::Int { val } => ir::ExprKind::Int { val },
            ast::ExprKind::App { ref base, ref args } => ir::ExprKind::App {
                base: Box::new(self.convert_expr(base)),
                args: args.iter().map(|e| self.convert_expr(e)).collect(),
            },
            _ => unimplemented!(),
        };

        ir::Expr {
            id,
            span: expr.span,
            kind,
        }
    }

    fn convert_type(&mut self, ty: &ast::Type) -> ir::Type {
        if let ast::TypeKind::Parens { inner } = &ty.kind {
            return self.convert_type(inner);
        }

        let id = self.next_id();
        let kind = match ty.kind {
            ast::TypeKind::Parens { .. } => unreachable!(),
            ast::TypeKind::Hole { name } => ir::TypeKind::Hole { name },
            ast::TypeKind::Ident { name } => match self.resolver.get(Ns::Types, name.symbol) {
                Some(res) => ir::TypeKind::Ident { res },
                None => ir::TypeKind::Error,
            },
            ast::TypeKind::Int { val } => ir::TypeKind::Int { val },
            ast::TypeKind::App { ref base, ref args } => ir::TypeKind::App {
                base: Box::new(self.convert_type(base)),
                args: args.iter().map(|t| self.convert_type(t)).collect(),
            },
            ast::TypeKind::Tuple { ref tys } => ir::TypeKind::Tuple {
                tys: tys.iter().map(|t| self.convert_type(t)).collect(),
            },
            ast::TypeKind::Record { ref row } => ir::TypeKind::Record {
                row: self.convert_row(row),
            },
            ast::TypeKind::Func {
                ref params,
                ref ret,
            } => ir::TypeKind::Func {
                params: params.iter().map(|t| self.convert_type(t)).collect(),
                ret: Box::new(self.convert_type(ret)),
            },
            ast::TypeKind::Forall { ref vars, ref ret } => ir::TypeKind::Forall {
                vars: vars.iter().map(|v| self.convert_type_var(v)).collect(),
                ty: Box::new(self.convert_type(ret)),
            },
            ast::TypeKind::Cons { ref cs, ref ty } => match self.convert_constraint(cs) {
                Some(cs) => ir::TypeKind::Cons {
                    cs,
                    ty: Box::new(self.convert_type(ty)),
                },
                None => ir::TypeKind::Error,
            },
            ast::TypeKind::Kinded { ref ty, ref kind } => ir::TypeKind::Kinded {
                ty: Box::new(self.convert_type(ty)),
                kind: Box::new(self.convert_type(kind)),
            },
        };

        ir::Type {
            id,
            span: ty.span,
            kind,
        }
    }

    fn convert_row(&mut self, row: &ast::Row) -> ir::Row {
        ir::Row {
            id: self.next_id(),
            span: row.span,
            fields: row
                .fields
                .iter()
                .map(|f| self.convert_row_field(f))
                .collect(),
            tail: row.tail.as_deref().map(|t| Box::new(self.convert_type(t))),
        }
    }

    fn convert_row_field(&mut self, field: &ast::RowField) -> ir::RowField {
        ir::RowField {
            id: self.next_id(),
            span: field.span,
            name: field.name,
            ty: self.convert_type(&field.ty),
        }
    }

    fn convert_type_var(&mut self, var: &ast::TypeVar) -> ir::TypeVar {
        match *var {
            ast::TypeVar::Kind { name, ref kind } => {
                let id = self.next_id();

                self.resolver.define(Ns::Types, name, ir::Res::Local(id));

                ir::TypeVar {
                    id,
                    span: name.span.merge(kind.span),
                    name,
                    kind: self.convert_type(kind),
                }
            }
            ast::TypeVar::Name { name } => {
                let id = self.next_id();

                self.resolver.define(Ns::Types, name, ir::Res::Local(id));

                ir::TypeVar {
                    id,
                    span: name.span,
                    name,
                    kind: ir::Type {
                        id: self.next_id(),
                        span: name.span,
                        kind: ir::TypeKind::Infer,
                    },
                }
            }
        }
    }

    fn convert_constraint(&mut self, cs: &ast::Constraint) -> Option<ir::Constraint> {
        match cs {
            ast::Constraint::CS { iface, tys } => Some(ir::Constraint {
                id: self.next_id(),
                span: iface.span.merge(if tys.is_empty() {
                    iface.span
                } else {
                    tys.last().unwrap().span
                }),
                iface: match self.resolver.get(Ns::Types, iface.symbol) {
                    Some(ir::Res::Def(ir::DefKind::Iface, iface)) => iface,
                    Some(res) => return None,
                    None => return None,
                },
                tys: tys.iter().map(|t| self.convert_type(t)).collect(),
            }),
            ast::Constraint::Parens { inner } => self.convert_constraint(inner),
        }
    }
}

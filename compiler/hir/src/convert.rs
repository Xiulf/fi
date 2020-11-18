use crate::ir;
use crate::resolve::{Ns, Resolver};
use crate::HirDatabase;
use std::collections::BTreeMap;
use std::sync::Arc;
use syntax::{ast, group};

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
    file: source::FileId,
    module_name: ir::Ident,
    id_counter: u32,
    current_item: ir::DefId,
    exports: Vec<ir::Export>,
    items: BTreeMap<ir::HirId, ir::Item>,
    trait_items: BTreeMap<ir::TraitItemId, ir::TraitItem>,
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
            file,
            module_name,
            resolver: Resolver::new(db.to_diag_db(), file),
            id_counter: 0,
            current_item: ir::DefId::dummy(),
            exports: Vec::new(),
            items: BTreeMap::new(),
            trait_items: BTreeMap::new(),
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
            exports: self.exports,
            body_ids: self.bodies.keys().copied().collect(),
            items: self.items,
            trait_items: self.trait_items,
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
        self.register_imports(&ast.imports);
        self.convert_decls(ast.decl_groups());
        self.register_exports(&ast.exports);
    }

    fn register_decls(&mut self, decl_groups: group::DeclGroups) {
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
            ast::DeclKind::Trait { .. } => (
                ir::DefPath::Type(group[0].name.symbol),
                ir::DefKind::Trait,
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
                ast::DeclKind::Trait {
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

    fn register_exports(&mut self, exports: &ast::Exports) {
        match exports {
            ast::Exports::All => {
                for (_, item) in &self.items {
                    Self::register_single_export(&mut self.exports, self.file, item);
                }
            }
            ast::Exports::Some(exports) => {
                for export in exports {
                    match &export.kind {
                        ast::ExportKind::Module => unimplemented!(),
                        _ => {
                            if let Some(item) = self
                                .items
                                .values()
                                .find(|i| i.name.symbol == export.name.symbol)
                            {
                                if let ast::ExportKind::Group(grp) = &export.kind {
                                    Self::register_single_export_grp(
                                        self.db,
                                        &mut self.exports,
                                        self.file,
                                        item,
                                        export.name.span,
                                        grp,
                                    );
                                } else {
                                    Self::register_single_export(
                                        &mut self.exports,
                                        self.file,
                                        item,
                                    );
                                }
                            } else {
                                self.db
                                    .to_diag_db()
                                    .error(format!(
                                        "module '{}' does not contain '{}'",
                                        self.module_name, export.name
                                    ))
                                    .with_label(diagnostics::Label::primary(
                                        self.file,
                                        export.name.span,
                                    ))
                                    .finish();
                            }
                        }
                    }
                }
            }
        }
    }

    fn register_single_export(
        exports: &mut Vec<ir::Export>,
        file: source::FileId,
        item: &ir::Item,
    ) {
        match &item.kind {
            ir::ItemKind::Func { .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Func, item.id.owner),
                    module: file,
                    ns: Ns::Values,
                    group: None,
                });
            }
            ir::ItemKind::Const { .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Const, item.id.owner),
                    module: file,
                    ns: Ns::Values,
                    group: None,
                });
            }
            ir::ItemKind::Static { .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Static, item.id.owner),
                    module: file,
                    ns: Ns::Values,
                    group: None,
                });
            }
            ir::ItemKind::Alias { .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Alias, item.id.owner),
                    module: file,
                    ns: Ns::Types,
                    group: None,
                });
            }
            ir::ItemKind::Data { body, .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Data, item.id.owner),
                    module: file,
                    ns: Ns::Types,
                    group: Some(
                        body.iter()
                            .map(|ctor| ir::Export {
                                name: ctor.name.symbol,
                                res: ir::Res::Def(ir::DefKind::Ctor, ctor.id.owner),
                                module: file,
                                ns: Ns::Values,
                                group: None,
                            })
                            .collect(),
                    ),
                });
            }
            ir::ItemKind::Trait { body, .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Trait, item.id.owner),
                    module: file,
                    ns: Ns::Types,
                    group: Some(
                        body.items
                            .iter()
                            .map(|item| ir::Export {
                                name: item.name.symbol,
                                res: ir::Res::Def(ir::DefKind::Func, item.id.0.owner),
                                module: file,
                                ns: Ns::Values,
                                group: None,
                            })
                            .collect(),
                    ),
                });
            }
            ir::ItemKind::Impl { .. } => {}
        }
    }

    fn register_single_export_grp(
        db: &dyn HirDatabase,
        exports: &mut Vec<ir::Export>,
        file: source::FileId,
        item: &ir::Item,
        span: ir::Span,
        grp: &ast::ExportGroup,
    ) {
        match &item.kind {
            ir::ItemKind::Data { body, .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Data, item.id.owner),
                    module: file,
                    ns: Ns::Types,
                    group: Some(if let ast::ExportGroup::Some(names) = grp {
                        names
                            .iter()
                            .filter_map(|name| {
                                if let Some(ctor) =
                                    body.iter().find(|c| c.name.symbol == name.symbol)
                                {
                                    Some(ir::Export {
                                        name: ctor.name.symbol,
                                        res: ir::Res::Def(ir::DefKind::Ctor, ctor.id.owner),
                                        module: file,
                                        ns: Ns::Values,
                                        group: None,
                                    })
                                } else {
                                    db.to_diag_db()
                                        .error(format!(
                                            "'{}' does not have constructor '{}'",
                                            item.name, name
                                        ))
                                        .with_label(diagnostics::Label::primary(file, name.span))
                                        .finish();

                                    None
                                }
                            })
                            .collect()
                    } else {
                        body.iter()
                            .map(|ctor| ir::Export {
                                name: ctor.name.symbol,
                                res: ir::Res::Def(ir::DefKind::Ctor, ctor.id.owner),
                                module: file,
                                ns: Ns::Values,
                                group: None,
                            })
                            .collect()
                    }),
                });
            }
            ir::ItemKind::Trait { body, .. } => {
                exports.push(ir::Export {
                    name: item.name.symbol,
                    res: ir::Res::Def(ir::DefKind::Trait, item.id.owner),
                    module: file,
                    ns: Ns::Types,
                    group: Some(if let ast::ExportGroup::Some(names) = grp {
                        names
                            .iter()
                            .filter_map(|name| {
                                if let Some(decl) =
                                    body.items.iter().find(|d| d.name.symbol == name.symbol)
                                {
                                    Some(ir::Export {
                                        name: decl.name.symbol,
                                        res: ir::Res::Def(ir::DefKind::Func, decl.id.0.owner),
                                        module: file,
                                        ns: Ns::Values,
                                        group: None,
                                    })
                                } else {
                                    db.to_diag_db()
                                        .error(format!(
                                            "'{}' does not have method '{}'",
                                            item.name, name
                                        ))
                                        .with_label(diagnostics::Label::primary(file, name.span))
                                        .finish();

                                    None
                                }
                            })
                            .collect()
                    } else {
                        body.items
                            .iter()
                            .map(|item| ir::Export {
                                name: item.name.symbol,
                                res: ir::Res::Def(ir::DefKind::Func, item.id.0.owner),
                                module: file,
                                ns: Ns::Values,
                                group: None,
                            })
                            .collect()
                    }),
                });
            }
            _ => db
                .to_diag_db()
                .error(format!("'{}' is not a datatype or trait", item.name))
                .with_label(diagnostics::Label::primary(file, span))
                .finish(),
        }
    }

    fn register_imports(&mut self, imports: &[ast::ImportDecl]) {
        for import in imports {
            self.register_import(import);
        }
    }

    fn register_import(&mut self, import: &ast::ImportDecl) {
        let module_tree = self.db.module_tree(self.lib);
        let module_data = module_tree.find(import.module.symbol).unwrap();
        let module = self.db.module_hir(module_data.file);

        if let Some(alias) = &import.qual {
            self.import_qual(&module, &import.names, *alias, import.span);
        } else {
            self.import_normal(&module, &import.names, import.span);
        }
    }

    fn import_qual(
        &mut self,
        imp_mod: &ir::Module,
        imports: &Option<(bool, Vec<ast::Import>)>,
        alias: ir::Ident,
        span: ir::Span,
    ) {
        let exports = self.collect_exports(imp_mod, imports, span);
    }

    fn import_normal(
        &mut self,
        imp_mod: &ir::Module,
        imports: &Option<(bool, Vec<ast::Import>)>,
        span: ir::Span,
    ) {
        let exports = self.collect_exports(imp_mod, imports, span);

        for export in exports {
            self.register_single_import(span, export);
        }
    }

    fn register_single_import(&mut self, span: ir::Span, export: ir::Export) {
        self.resolver.define(
            export.ns,
            ir::Ident {
                symbol: export.name,
                span,
            },
            export.res,
        );

        if let Some(group) = export.group {
            for exp in group {
                self.register_single_import(span, exp);
            }
        }
    }

    fn collect_exports(
        &self,
        module: &ir::Module,
        imports: &Option<(bool, Vec<ast::Import>)>,
        span: ir::Span,
    ) -> Vec<ir::Export> {
        let mut exports = module.exports.clone();

        if let Some((hiding, imports)) = imports {
            if *hiding {
                for import in imports {
                    self.find_export(module, &mut exports, import);
                }

                exports
            } else {
                imports
                    .iter()
                    .filter_map(|import| self.find_export(module, &mut exports, import))
                    .collect()
            }
        } else {
            exports
        }
    }

    fn find_export(
        &self,
        module: &ir::Module,
        exports: &mut Vec<ir::Export>,
        import: &ast::Import,
    ) -> Option<ir::Export> {
        if let Some(idx) = exports.iter().position(|e| e.name == import.name.symbol) {
            let mut export = exports.swap_remove(idx);

            if let ast::ImportKind::Group(igrp) = &import.kind {
                if let Some(egrp) = &mut export.group {
                    if let ast::ImportGroup::Some(igrp) = igrp {
                        *egrp = igrp
                            .iter()
                            .filter_map(|name| {
                                if let Some(idx) = egrp.iter().position(|e| e.name == name.symbol) {
                                    Some(egrp.swap_remove(idx))
                                } else {
                                    self.db
                                        .to_diag_db()
                                        .error(format!(
                                            "module '{}' does not export '{}'",
                                            module.name, name
                                        ))
                                        .with_label(diagnostics::Label::primary(
                                            self.file, name.span,
                                        ))
                                        .finish();

                                    None
                                }
                            })
                            .collect();
                    }
                } else {
                    self.db
                        .to_diag_db()
                        .error(format!("'{}' is not a group export", import.name))
                        .with_label(diagnostics::Label::primary(self.file, import.name.span))
                        .finish();
                }
            }

            Some(export)
        } else {
            self.db
                .to_diag_db()
                .error(format!(
                    "module '{}' does not export '{}'",
                    module.name, import.name
                ))
                .with_label(diagnostics::Label::primary(self.file, import.name.span))
                .finish();

            None
        }
    }

    fn convert_decls(&mut self, groups: group::DeclGroups) {
        for (kind, decls) in groups {
            self.convert_decl(kind, decls);
        }
    }

    fn convert_decl(&mut self, kind: group::DeclGroupKind, decls: &[ast::Decl]) {
        let first = &decls[0];
        let defpath = match kind {
            group::DeclGroupKind::Func(_)
            | group::DeclGroupKind::Const(_)
            | group::DeclGroupKind::Static(_) => ir::DefPath::Value(first.name.symbol),
            group::DeclGroupKind::Alias(_)
            | group::DeclGroupKind::Data(_)
            | group::DeclGroupKind::Trait => ir::DefPath::Type(first.name.symbol),
            group::DeclGroupKind::Impl => return self.convert_impl_chain(first),
        };

        let defindex = ir::DefIndex::from_path(self.module_name.symbol, &[defpath]);

        self.current_item = ir::DefId::new(self.lib, defindex);
        self.id_counter = 0;

        let id = self.next_id();
        let span = first.span.merge(decls.last().unwrap().span);
        let kind = match kind {
            group::DeclGroupKind::Func(_) => {
                let mut ty = None;
                let mut params = None::<Vec<ir::Param>>;
                let mut arms = Vec::new();
                let body_id = ir::BodyId(self.next_id());

                for decl in decls {
                    match &decl.kind {
                        ast::DeclKind::FuncTy { ty: fty } => {
                            ty = Some(self.convert_type(fty));
                        }
                        ast::DeclKind::Func { pats, val } => {
                            if let None = params {
                                params = Some(
                                    (0..pats.len())
                                        .map(|_| ir::Param { id: self.next_id() })
                                        .collect(),
                                );
                            }

                            self.resolver.push_rib(Ns::Values);

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

                            self.resolver.pop_rib(Ns::Values);
                        }
                        _ => unreachable!(),
                    }
                }

                let ty = ty.unwrap_or_else(|| ir::Type {
                    id: self.next_id(),
                    span: first.name.span,
                    kind: ir::TypeKind::Infer,
                });

                let params = if let Some(p) = params {
                    p
                } else {
                    self.db
                        .to_diag_db()
                        .error(format!("function '{}' has no body", first.name))
                        .with_label(diagnostics::Label::primary(self.file, first.name.span))
                        .finish();

                    Vec::new()
                };

                let value = ir::Expr {
                    id: self.next_id(),
                    span,
                    kind: ir::ExprKind::Case {
                        arms,
                        pred: params
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
                        params,
                        value,
                    },
                );

                ir::ItemKind::Func { ty, body: body_id }
            }
            group::DeclGroupKind::Const(_) => {
                let mut ty = None;
                let mut val = None;
                let body_id = ir::BodyId(self.next_id());

                for decl in decls {
                    match &decl.kind {
                        ast::DeclKind::ConstTy { ty: ty2 } => {
                            ty = Some(self.convert_type(ty2));
                        }
                        ast::DeclKind::Const { val: val2 } => {
                            val = Some(self.convert_expr(val2));
                        }
                        _ => unreachable!(),
                    }
                }

                let ty = ty.unwrap_or_else(|| ir::Type {
                    id: self.next_id(),
                    span: first.name.span,
                    kind: ir::TypeKind::Infer,
                });

                let val = match val {
                    Some(v) => v,
                    None => {
                        return;
                    }
                };

                self.bodies.insert(
                    body_id,
                    ir::Body {
                        id: body_id,
                        params: Vec::new(),
                        value: val,
                    },
                );

                ir::ItemKind::Const { ty, body: body_id }
            }
            group::DeclGroupKind::Static(_) => {
                let mut ty = None;
                let mut val = None;
                let body_id = ir::BodyId(self.next_id());

                for decl in decls {
                    match &decl.kind {
                        ast::DeclKind::StaticTy { ty: ty2 } => {
                            ty = Some(self.convert_type(ty2));
                        }
                        ast::DeclKind::Static { val: val2 } => {
                            val = Some(self.convert_expr(val2));
                        }
                        _ => unreachable!(),
                    }
                }

                let ty = ty.unwrap_or_else(|| ir::Type {
                    id: self.next_id(),
                    span: first.name.span,
                    kind: ir::TypeKind::Infer,
                });

                let val = match val {
                    Some(v) => v,
                    None => {
                        return;
                    }
                };

                self.bodies.insert(
                    body_id,
                    ir::Body {
                        id: body_id,
                        params: Vec::new(),
                        value: val,
                    },
                );

                ir::ItemKind::Static { ty, body: body_id }
            }
            group::DeclGroupKind::Alias(_) => {
                let mut kind = None;
                let mut value = None;
                let mut vars = Vec::new();

                for decl in decls {
                    match &decl.kind {
                        ast::DeclKind::AliasKind { kind: kind2 } => {
                            kind = Some(self.convert_type(kind2));
                        }
                        ast::DeclKind::Alias { vars: vars2, ty } => {
                            self.resolver.push_rib(Ns::Types);

                            vars.extend(vars2.iter().map(|v| self.convert_type_var(v)));
                            value = Some(self.convert_type(ty));

                            self.resolver.pop_rib(Ns::Types);
                        }
                        _ => unreachable!(),
                    }
                }

                let kind = kind.unwrap_or_else(|| ir::Type {
                    id: self.next_id(),
                    span: first.name.span,
                    kind: ir::TypeKind::Infer,
                });

                let value = match value {
                    Some(v) => v,
                    None => {
                        return;
                    }
                };

                ir::ItemKind::Alias { kind, vars, value }
            }
            group::DeclGroupKind::Data(_) => {
                let mut kind = None;
                let mut has_def = false;
                let mut vars = Vec::new();
                let mut body = Vec::new();
                let head_id = self.next_id();

                for decl in decls {
                    match &decl.kind {
                        ast::DeclKind::DataKind { kind: kind2 } => {
                            kind = Some(self.convert_type(kind2));
                        }
                        ast::DeclKind::Data { head, body: body2 } => {
                            if !has_def {
                                has_def = true;
                                vars.extend(head.vars.iter().map(|v| self.convert_type_var(v)));

                                if let Some(body2) = body2 {
                                    body.extend(
                                        body2.iter().map(|c| self.convert_data_ctor(defpath, c)),
                                    );
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                let kind = kind.unwrap_or_else(|| ir::Type {
                    id: self.next_id(),
                    span: first.name.span,
                    kind: ir::TypeKind::Infer,
                });

                let head = ir::DataHead {
                    id: head_id,
                    span: first.span,
                    vars,
                    kind,
                };

                ir::ItemKind::Data { head, body }
            }
            group::DeclGroupKind::Trait => {
                if let ast::DeclKind::Trait { head, body } = &first.kind {
                    self.resolver.push_rib(Ns::Types);

                    let head_id = self.next_id();
                    let vars = head.vars.iter().map(|v| self.convert_type_var(v)).collect();
                    let parent = if let Some(parent) = &head.parent {
                        parent
                            .iter()
                            .filter_map(|c| self.convert_constraint(c))
                            .collect()
                    } else {
                        Vec::new()
                    };

                    let head = ir::TraitHead {
                        id: head_id,
                        span: head.span,
                        parent,
                        vars,
                    };

                    let body_id = self.next_id();
                    let body = if let Some(body) = body {
                        ir::TraitBody {
                            id: body_id,
                            span: body.span,
                            items: body
                                .decls
                                .iter()
                                .map(|d| self.convert_trait_decl(first.name.symbol, d))
                                .collect(),
                        }
                    } else {
                        ir::TraitBody {
                            id: body_id,
                            span: first.span,
                            items: Vec::new(),
                        }
                    };

                    self.resolver.pop_rib(Ns::Types);

                    ir::ItemKind::Trait { head, body }
                } else {
                    unreachable!();
                }
            }
            group::DeclGroupKind::Impl => unreachable!(),
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

    fn convert_impl_chain(&mut self, decl: &ast::Decl) {
        if let ast::DeclKind::ImplChain { impls } = &decl.kind {
            let mut chain = Vec::new();

            for imp in impls {
                let defindex = ir::DefIndex::from_path(
                    self.module_name.symbol,
                    &[ir::DefPath::Type(imp.head.name.symbol)],
                );

                let defid = ir::DefId::new(self.lib, defindex);

                chain.push(ir::HirId {
                    owner: defid,
                    local_id: ir::LocalId(0),
                });
            }

            for (i, imp) in impls.iter().enumerate() {
                let id = chain[i];

                self.current_item = id.owner;
                self.id_counter = 1;

                let head_id = self.next_id();
                let cs = if let Some(cs) = &imp.head.cs {
                    cs.iter()
                        .filter_map(|c| self.convert_constraint(c))
                        .collect()
                } else {
                    Vec::new()
                };

                let tys = imp.head.tys.iter().map(|t| self.convert_type(t)).collect();
                let trait_ = match self.resolver.get(Ns::Types, imp.head.iface.symbol) {
                    Some(ir::Res::Def(ir::DefKind::Trait, iface)) => iface,
                    Some(_) => {
                        self.db
                            .to_diag_db()
                            .error(format!("'{}' is not a trait", imp.head.iface))
                            .with_label(diagnostics::Label::primary(self.file, imp.head.iface.span))
                            .finish();

                        continue;
                    }
                    None => {
                        self.db
                            .to_diag_db()
                            .error(format!("unknown trait '{}'", imp.head.iface))
                            .with_label(diagnostics::Label::primary(self.file, imp.head.iface.span))
                            .finish();

                        continue;
                    }
                };

                let head = ir::ImplHead {
                    id: head_id,
                    span: imp.head.span,
                    cs,
                    trait_,
                    tys,
                };

                let body_id = self.next_id();
                let body = if let Some(body) = &imp.body {
                    ir::ImplBody {
                        id: body_id,
                        span: body.span,
                        items: group::ImplDeclGroups::new(&body.decls)
                            .map(|(g, d)| self.convert_impl_decl(imp.head.name.symbol, g, d))
                            .collect(),
                    }
                } else {
                    ir::ImplBody {
                        id: body_id,
                        span: imp.span,
                        items: Vec::new(),
                    }
                };

                self.items.insert(
                    id,
                    ir::Item {
                        id,
                        span: imp.span,
                        attrs: decl.attrs.clone(),
                        name: imp.head.name,
                        kind: ir::ItemKind::Impl {
                            chain: chain.clone(),
                            index: i,
                            head,
                            body,
                        },
                    },
                );
            }
        }
    }

    fn convert_data_ctor(&mut self, parent: ir::DefPath, ctor: &ast::DataCtor) -> ir::DataCtor {
        let old_id = (self.current_item, self.id_counter);
        let defindex = ir::DefIndex::from_path(
            self.module_name.symbol,
            &[parent, ir::DefPath::Value(ctor.name.symbol)],
        );

        self.current_item = ir::DefId::new(self.lib, defindex);
        self.id_counter = 0;

        let id = self.next_id();
        let tys = ctor.tys.iter().map(|t| self.convert_type(t)).collect();

        self.current_item = old_id.0;
        self.id_counter = old_id.1;

        ir::DataCtor {
            id,
            span: ctor.span,
            name: ctor.name,
            tys,
        }
    }

    fn convert_trait_decl(&mut self, iface: ir::Symbol, decl: &ast::TraitDecl) -> ir::TraitItemRef {
        let old_id = (self.current_item, self.id_counter);
        let defindex = ir::DefIndex::from_path(
            self.module_name.symbol,
            &[
                ir::DefPath::Type(iface),
                ir::DefPath::Value(decl.name.symbol),
            ],
        );

        self.current_item = ir::DefId::new(self.lib, defindex);
        self.id_counter = 0;

        let id = self.next_id();
        let kind = match &decl.kind {
            ast::TraitDeclKind::FuncTy { ty } => ir::IfaceItemKind::Func {
                ty: self.convert_type(ty),
            },
        };

        self.trait_items.insert(
            ir::TraitItemId(id),
            ir::TraitItem {
                id,
                span: decl.span,
                name: decl.name,
                kind,
            },
        );

        self.current_item = old_id.0;
        self.id_counter = old_id.1;

        ir::TraitItemRef {
            id: ir::TraitItemId(id),
            span: decl.span,
            name: decl.name,
            kind: ir::AssocItemKind::Func,
        }
    }

    fn convert_impl_decl(
        &mut self,
        imp: ir::Symbol,
        kind: group::ImplDeclGroupKind,
        decls: &[ast::ImplDecl],
    ) -> ir::ImplItemRef {
        let first = &decls[0];
        let old_id = (self.current_item, self.id_counter);
        let defindex = ir::DefIndex::from_path(
            self.module_name.symbol,
            &[
                ir::DefPath::Type(imp),
                match kind {
                    group::ImplDeclGroupKind::Func(_) => ir::DefPath::Value(first.name.symbol),
                },
            ],
        );

        self.current_item = ir::DefId::new(self.lib, defindex);
        self.id_counter = 0;

        let id = self.next_id();
        let span = first.span.merge(decls.last().unwrap().span);
        let kind = match kind {
            group::ImplDeclGroupKind::Func(_) => {
                let mut ty = None;
                let mut params = None::<Vec<ir::Param>>;
                let mut arms = Vec::new();
                let body_id = ir::BodyId(self.next_id());

                for decl in decls {
                    match &decl.kind {
                        ast::ImplDeclKind::FuncTy { ty: fty } => {
                            ty = Some(self.convert_type(fty));
                        }
                        ast::ImplDeclKind::Func { pats, val } => {
                            if let None = params {
                                params = Some(
                                    (0..pats.len())
                                        .map(|_| ir::Param { id: self.next_id() })
                                        .collect(),
                                );
                            }

                            self.resolver.push_rib(Ns::Values);

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

                            self.resolver.pop_rib(Ns::Values);
                        }
                    }
                }

                let ty = ty.unwrap_or_else(|| ir::Type {
                    id: self.next_id(),
                    span: first.name.span,
                    kind: ir::TypeKind::Infer,
                });

                let params = if let Some(p) = params {
                    p
                } else {
                    self.db
                        .to_diag_db()
                        .error(format!("function '{}' has no body", first.name))
                        .with_label(diagnostics::Label::primary(self.file, first.name.span))
                        .finish();

                    Vec::new()
                };

                let value = ir::Expr {
                    id: self.next_id(),
                    span,
                    kind: ir::ExprKind::Case {
                        arms,
                        pred: params
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
                        params,
                        value,
                    },
                );

                ir::ImplItemKind::Func { ty, body: body_id }
            }
        };

        self.impl_items.insert(
            ir::ImplItemId(id),
            ir::ImplItem {
                id,
                span,
                name: first.name,
                kind,
            },
        );

        self.current_item = old_id.0;
        self.id_counter = old_id.1;

        ir::ImplItemRef {
            id: ir::ImplItemId(id),
            span,
            name: first.name,
            kind: ir::AssocItemKind::Func,
        }
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
                    Some(_) => {
                        self.db
                            .to_diag_db()
                            .error(format!("'{}' is not a constructor", name))
                            .with_label(diagnostics::Label::primary(self.file, name.span))
                            .finish();

                        ir::PatKind::Error
                    }
                    None => {
                        self.db
                            .to_diag_db()
                            .error(format!("unknown constructor '{}'", name))
                            .with_label(diagnostics::Label::primary(self.file, name.span))
                            .finish();

                        ir::PatKind::Error
                    }
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
                Some(
                    res
                    @
                    (ir::Res::Local(_)
                    | ir::Res::Def(
                        ir::DefKind::Func
                        | ir::DefKind::Const
                        | ir::DefKind::Static
                        | ir::DefKind::Ctor,
                        _,
                    )),
                ) => ir::ExprKind::Ident { res },
                Some(_) => {
                    self.db
                        .to_diag_db()
                        .error(format!("'{}' is not a value", name))
                        .with_label(diagnostics::Label::primary(self.file, name.span))
                        .finish();

                    ir::ExprKind::Error
                }
                None => {
                    self.db
                        .to_diag_db()
                        .error(format!("unknown value '{}'", name))
                        .with_label(diagnostics::Label::primary(self.file, name.span))
                        .finish();

                    ir::ExprKind::Error
                }
            },
            ast::ExprKind::Int { val } => ir::ExprKind::Int { val },
            ast::ExprKind::Float { bits } => ir::ExprKind::Float { bits },
            ast::ExprKind::Char { val } => ir::ExprKind::Char { val },
            ast::ExprKind::Str { ref val } => ir::ExprKind::Str { val: val.clone() },
            ast::ExprKind::App { ref base, ref args } => ir::ExprKind::App {
                base: Box::new(self.convert_expr(base)),
                args: args.iter().map(|e| self.convert_expr(e)).collect(),
            },
            ast::ExprKind::Array { ref exprs } => ir::ExprKind::Array {
                exprs: exprs.iter().map(|e| self.convert_expr(e)).collect(),
            },
            ast::ExprKind::Tuple { ref exprs } => ir::ExprKind::Tuple {
                exprs: exprs.iter().map(|e| self.convert_expr(e)).collect(),
            },
            ast::ExprKind::Record { ref fields } => ir::ExprKind::Record {
                fields: fields.iter().map(|f| self.convert_field_expr(f)).collect(),
            },
            ast::ExprKind::Field { ref base, field } => ir::ExprKind::Field {
                base: Box::new(self.convert_expr(base)),
                field,
            },
            ast::ExprKind::Index {
                ref base,
                ref index,
            } => ir::ExprKind::Index {
                base: Box::new(self.convert_expr(base)),
                index: Box::new(self.convert_expr(index)),
            },
            ast::ExprKind::Assign { ref lhs, ref rhs } => ir::ExprKind::Assign {
                lhs: Box::new(self.convert_expr(lhs)),
                rhs: Box::new(self.convert_expr(rhs)),
            },
            ast::ExprKind::Infix {
                op,
                ref lhs,
                ref rhs,
            } => ir::ExprKind::Infix {
                op,
                lhs: Box::new(self.convert_expr(lhs)),
                rhs: Box::new(self.convert_expr(rhs)),
            },
            ast::ExprKind::Prefix { op, ref rhs } => ir::ExprKind::Prefix {
                op,
                rhs: Box::new(self.convert_expr(rhs)),
            },
            ast::ExprKind::Postfix { op, ref lhs } => ir::ExprKind::Postfix {
                op,
                lhs: Box::new(self.convert_expr(lhs)),
            },
            ast::ExprKind::Let {
                ref bindings,
                ref body,
            } => {
                self.resolver.push_rib(Ns::Values);

                let bindings = group::LetBindingGroups::new(bindings)
                    .filter_map(|b| self.convert_let_binding(b))
                    .collect();

                let body = self.convert_expr(body);

                self.resolver.pop_rib(Ns::Values);

                ir::ExprKind::Let {
                    bindings,
                    body: Box::new(body),
                }
            }
            ast::ExprKind::If {
                ref cond,
                ref then,
                ref else_,
            } => ir::ExprKind::If {
                cond: Box::new(self.convert_expr(cond)),
                then: Box::new(self.convert_expr(then)),
                else_: Box::new(self.convert_expr(else_)),
            },
            ast::ExprKind::Case { ref pred, ref arms } => ir::ExprKind::Case {
                pred: pred.iter().map(|p| self.convert_expr(p)).collect(),
                arms: arms.iter().map(|a| self.convert_case_arm(a)).collect(),
            },
            ast::ExprKind::Loop { ref body } => ir::ExprKind::Loop {
                body: self.convert_block(body),
            },
            ast::ExprKind::While { ref cond, ref body } => ir::ExprKind::While {
                cond: Box::new(self.convert_expr(cond)),
                body: self.convert_block(body),
            },
            ast::ExprKind::Break {} => ir::ExprKind::Break {},
            ast::ExprKind::Next {} => ir::ExprKind::Next {},
            ast::ExprKind::Do { ref block } => ir::ExprKind::Do {
                block: self.convert_block(block),
            },
            ast::ExprKind::Return { ref val } => ir::ExprKind::Return {
                val: Box::new(self.convert_expr(val)),
            },
            ast::ExprKind::Typed { ref expr, ref ty } => ir::ExprKind::Typed {
                expr: Box::new(self.convert_expr(expr)),
                ty: self.convert_type(ty),
            },
        };

        ir::Expr {
            id,
            span: expr.span,
            kind,
        }
    }

    fn convert_field_expr(
        &mut self,
        field: &ast::RecordField<ast::Expr>,
    ) -> ir::RecordField<ir::Expr> {
        match *field {
            ast::RecordField::Field { name, ref val } => ir::RecordField {
                id: self.next_id(),
                span: name.span.merge(val.span),
                name,
                val: self.convert_expr(val),
            },
            ast::RecordField::Pun { name } => ir::RecordField {
                id: self.next_id(),
                span: name.span,
                name,
                val: ir::Expr {
                    id: self.next_id(),
                    span: name.span,
                    kind: match self.resolver.get(Ns::Values, name.symbol) {
                        Some(
                            res
                            @
                            (ir::Res::Local(_)
                            | ir::Res::Def(
                                ir::DefKind::Func
                                | ir::DefKind::Const
                                | ir::DefKind::Static
                                | ir::DefKind::Ctor,
                                _,
                            )),
                        ) => ir::ExprKind::Ident { res },
                        Some(_) => {
                            self.db
                                .to_diag_db()
                                .error(format!("'{}' is not a value", name))
                                .with_label(diagnostics::Label::primary(self.file, name.span))
                                .finish();

                            ir::ExprKind::Error
                        }
                        None => {
                            self.db
                                .to_diag_db()
                                .error(format!("unknown value '{}'", name))
                                .with_label(diagnostics::Label::primary(self.file, name.span))
                                .finish();

                            ir::ExprKind::Error
                        }
                    },
                },
            },
        }
    }

    fn convert_block(&mut self, block: &ast::Block) -> ir::Block {
        let id = self.next_id();

        self.resolver.push_rib(Ns::Values);

        let stmts = block.stmts.iter().map(|s| self.convert_stmt(s)).collect();

        self.resolver.pop_rib(Ns::Values);

        ir::Block {
            id,
            span: block.span,
            stmts,
        }
    }

    fn convert_stmt(&mut self, stmt: &ast::Stmt) -> ir::Stmt {
        let id = self.next_id();
        let kind = match stmt.kind {
            ast::StmtKind::Discard { ref expr } => ir::StmtKind::Discard {
                expr: self.convert_expr(expr),
            },
            ast::StmtKind::Bind { ref pat, ref val } => ir::StmtKind::Bind {
                binding: ir::Binding {
                    id: self.next_id(),
                    span: stmt.span,
                    pat: self.convert_pat(pat),
                    val: self.convert_expr(val),
                    ty: ir::Type {
                        id: self.next_id(),
                        span: stmt.span,
                        kind: ir::TypeKind::Infer,
                    },
                },
            },
        };

        ir::Stmt {
            id,
            span: stmt.span,
            kind,
        }
    }

    fn convert_let_binding(&mut self, bindings: &[ast::LetBinding]) -> Option<ir::Binding> {
        let first = &bindings[0];
        let id = self.next_id();
        let mut ty = None;
        let mut body = None;

        for binding in bindings {
            match &binding.kind {
                ast::LetBindingKind::Type { ty: ty2, .. } => {
                    ty = Some(self.convert_type(ty2));
                }
                ast::LetBindingKind::Value { pat, val } => {
                    body = Some((self.convert_pat(pat), self.convert_expr(val)));
                }
            }
        }

        let (pat, val) = if let Some(b) = body {
            b
        } else {
            return None;
        };

        let ty = ty.unwrap_or_else(|| ir::Type {
            id: self.next_id(),
            span: first.span,
            kind: ir::TypeKind::Infer,
        });

        Some(ir::Binding {
            id,
            span: first.span.merge(bindings.last().unwrap().span),
            pat,
            val,
            ty,
        })
    }

    fn convert_case_arm(&mut self, arm: &ast::CaseArm) -> ir::CaseArm {
        ir::CaseArm {
            id: self.next_id(),
            span: arm.span,
            pats: arm.pats.iter().map(|p| self.convert_pat(p)).collect(),
            val: self.convert_guarded(&arm.val),
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
                Some(
                    res
                    @
                    (ir::Res::Local(_)
                    | ir::Res::Def(
                        ir::DefKind::Alias | ir::DefKind::Data | ir::DefKind::Trait,
                        _,
                    )),
                ) => ir::TypeKind::Ident { res },
                Some(_) => {
                    self.db
                        .to_diag_db()
                        .error(format!("'{}' is not a type", name))
                        .with_label(diagnostics::Label::primary(self.file, name.span))
                        .finish();

                    ir::TypeKind::Error
                }
                None => {
                    self.db
                        .to_diag_db()
                        .error(format!("unknown type '{}'", name))
                        .with_label(diagnostics::Label::primary(self.file, name.span))
                        .finish();

                    ir::TypeKind::Error
                }
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
            ast::TypeKind::Forall { ref vars, ref ret } => {
                self.resolver.push_rib(Ns::Types);

                let vars = vars.iter().map(|v| self.convert_type_var(v)).collect();
                let ty = self.convert_type(ret);

                self.resolver.pop_rib(Ns::Types);

                ir::TypeKind::Forall {
                    vars,
                    ty: Box::new(ty),
                }
            }
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
                trait_: match self.resolver.get(Ns::Types, iface.symbol) {
                    Some(ir::Res::Def(ir::DefKind::Trait, iface)) => iface,
                    Some(_) => {
                        self.db
                            .to_diag_db()
                            .error(format!("'{}' is not a trait", iface))
                            .with_label(diagnostics::Label::primary(self.file, iface.span))
                            .finish();

                        return None;
                    }
                    None => {
                        self.db
                            .to_diag_db()
                            .error(format!("unknown trait '{}'", iface))
                            .with_label(diagnostics::Label::primary(self.file, iface.span))
                            .finish();

                        return None;
                    }
                },
                tys: tys.iter().map(|t| self.convert_type(t)).collect(),
            }),
            ast::Constraint::Parens { inner } => self.convert_constraint(inner),
        }
    }
}

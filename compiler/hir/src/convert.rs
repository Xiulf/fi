use crate::*;
use diagnostics::{Diagnostic, Reporter, Severity};
use resolve::{Export, ModuleId, Ns, Res, Resolver, RibKind};
use std::collections::{BTreeMap, HashMap};
use syntax::ast;

pub fn convert<'a>(
    reporter: &Reporter,
    ast: &ast::Package,
    package_name: &str,
    sdeps: impl Iterator<Item = &'a std::path::Path>,
    ideps: impl Iterator<Item = &'a std::path::Path>,
) -> (Package, Vec<resolve::ModuleMeta>) {
    let modules = sort_modules(reporter, &ast.modules);
    let mut converter = Converter::new(reporter, sdeps, ideps);

    for module in modules {
        let id = converter.register_module(module);

        converter.trans_module(module, id);
    }

    converter.finish(Symbol::new(package_name))
}

fn sort_modules<'a>(reporter: &Reporter, modules: &'a [ast::Module]) -> Vec<&'a ast::Module> {
    let mut g = petgraph::Graph::new();
    let ids = modules
        .iter()
        .map(|m| (m.name.symbol, g.add_node(m)))
        .collect::<HashMap<_, _>>();

    for module in modules {
        for import in &module.imports {
            match (ids.get(&module.name.symbol), ids.get(&import.module.symbol)) {
                (Some(a), Some(b)) => {
                    g.add_edge(*a, *b, ());
                }
                _ => {}
            }
        }
    }

    match petgraph::algo::toposort(&g, None) {
        Ok(order) => order
            .into_iter()
            .map(|i| g.node_weight(i).unwrap())
            .copied()
            .rev()
            .collect(),
        Err(e) => {
            let module = g.node_weight(e.node_id()).unwrap();

            reporter.add(
                Diagnostic::new(
                    Severity::Error,
                    0019,
                    format!("cyclic module '{}'", module.name),
                )
                .label(Severity::Error, module.name.span, None::<String>),
            );

            reporter.report(true);
            unreachable!();
        }
    }
}

pub struct Converter<'a> {
    reporter: &'a Reporter,
    resolver: Resolver<'a>,
    items: BTreeMap<Id, Item>,
    exprs: BTreeMap<Id, Expr>,
    types: BTreeMap<Id, Type>,
    imports: Imports,
    current_item: ItemId,
    local_id: u64,
    vmods: usize,
}

impl<'a> Converter<'a> {
    pub fn new<'b>(
        reporter: &'a Reporter,
        sdeps: impl Iterator<Item = &'b std::path::Path>,
        ideps: impl Iterator<Item = &'b std::path::Path>,
    ) -> Self {
        let mut imports = Imports(BTreeMap::new());

        for dep in ideps {
            let import = Imports::load(dep);

            imports.0.extend(import.0);
        }

        Converter {
            reporter,
            resolver: Resolver::new(reporter, sdeps),
            items: BTreeMap::new(),
            exprs: BTreeMap::new(),
            types: BTreeMap::new(),
            imports,
            current_item: ItemId(0),
            local_id: 0,
            vmods: 0,
        }
    }

    pub fn finish(self, package: Symbol) -> (Package, Vec<resolve::ModuleMeta>) {
        (
            Package {
                name: package,
                modules: self
                    .resolver
                    .modules
                    .iter()
                    .filter_map(|(id, m)| {
                        if let crate::resolve::Module::Normal(m) = m {
                            if let ModuleId::Normal(id) = id {
                                Some((*id, m.module.clone()))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect(),
                items: self.items,
                exprs: self.exprs,
                types: self.types,
                imports: self.imports,
            },
            self.resolver.all_module_meta(),
        )
    }

    fn next_id(&mut self) -> Id {
        self.local_id += 1;

        Id(self.current_item, self.local_id)
    }

    pub fn register_module(&mut self, module: &ast::Module) -> ModuleId {
        let id = ModuleId::Normal(module.name.symbol);

        self.resolver.add_module(module.name.symbol, false);
        self.resolver.set_module(id);

        for item in &module.items {
            self.register_item(item, true);
        }

        for import in &module.imports {
            self.register_import(import, &id);
        }

        self.register_exports(&id, &module.exports);

        id
    }

    pub fn register_import(&mut self, import: &ast::Import, module: &ModuleId) {
        let imodule = ModuleId::Normal(import.module.symbol);

        if let Some(alias) = import.alias {
            self.import_virtual(
                module,
                &imodule,
                &import.imports,
                import.hiding,
                alias,
                import.span,
            );
        } else {
            self.import_normal(
                module,
                &imodule,
                &import.imports,
                import.hiding,
                import.span,
            );
        }
    }

    pub fn import_virtual(
        &mut self,
        module: &ModuleId,
        imodule: &ModuleId,
        names: &Option<Vec<ast::ImportItem>>,
        hiding: bool,
        alias: Ident,
        span: Span,
    ) {
        let exports = self.collect_imports(imodule, names, hiding, span);

        if let Some(&vmod_id) = self.resolver.modules[module]
            .info()
            .imports
            .get(&alias.symbol)
        {
            for (name, export) in exports {
                // TODO: check for clashing import
                self.resolver
                    .modules
                    .get_mut(&vmod_id)
                    .unwrap()
                    .virt_mut()
                    .exports
                    .insert(name.symbol, export);
            }
        } else {
            self.vmods += 1;
            let vmod_id = ModuleId::Virtual(self.vmods);

            self.resolver
                .modules
                .get_mut(module)
                .unwrap()
                .info_mut()
                .imports
                .insert(alias.symbol, vmod_id);

            self.resolver
                .define(Ns::Modules, alias.symbol, alias.span, Res::Module(vmod_id));

            self.resolver.modules.insert(
                vmod_id,
                crate::resolve::Module::Virtual(crate::resolve::VirtualModule {
                    name: alias.symbol,
                    exports: exports.into_iter().map(|(n, e)| (n.symbol, e)).collect(),
                }),
            );
        }
    }

    pub fn import_normal(
        &mut self,
        module: &ModuleId,
        imodule: &ModuleId,
        names: &Option<Vec<ast::ImportItem>>,
        hiding: bool,
        span: Span,
    ) {
        let imports = self.collect_imports(imodule, names, hiding, span);

        for (name, export) in imports {
            self.import_single(module, name, export);
        }
    }

    pub fn import_single(&mut self, module: &ModuleId, name: Ident, export: Export) {
        match export {
            Export::Value(m, id) => {
                // TODO: check for duplicate import
                self.resolver
                    .modules
                    .get_mut(module)
                    .unwrap()
                    .info_mut()
                    .scopes[Ns::Values]
                    .last_mut()
                    .unwrap()
                    .insert(name.symbol, Res::Item(id));
            }
            Export::Type(m, id) => {
                // TODO: check for duplicate import
                self.resolver
                    .modules
                    .get_mut(module)
                    .unwrap()
                    .info_mut()
                    .scopes[Ns::Types]
                    .last_mut()
                    .unwrap()
                    .insert(name.symbol, Res::Item(id));
            }
        }
    }

    pub fn collect_imports(
        &self,
        imodule: &ModuleId,
        imports: &Option<Vec<ast::ImportItem>>,
        hiding: bool,
        span: Span,
    ) -> Vec<(Ident, Export)> {
        let mut exports = self.collect_exports(imodule);

        if let Some(imports) = imports {
            if hiding {
                for import in imports {
                    self.find_export(imodule, &mut exports, &import.name);
                }

                exports
                    .into_iter()
                    .map(|(mut n, e)| {
                        n.span = span;
                        (n, e)
                    })
                    .collect()
            } else {
                imports
                    .into_iter()
                    .flat_map(|import| {
                        self.find_export(imodule, &mut exports, &import.name)
                            .into_iter()
                            .map(|(mut n, e)| {
                                n.span = span;
                                (n, e)
                            })
                    })
                    .collect()
            }
        } else {
            exports
                .into_iter()
                .map(|(mut n, e)| {
                    n.span = span;
                    (n, e)
                })
                .collect()
        }
    }

    pub fn register_exports(&mut self, module: &ModuleId, exports: &ast::Exports) {
        match exports {
            ast::Exports::All => {
                self.export_all(module);
            }
            ast::Exports::Some(exports) => {
                for export in exports {
                    let mut found = false;

                    match self
                        .resolver
                        .get(Ns::Modules, Some(*module), &export.name.symbol)
                    {
                        (Some(Res::Module(id)), _) => {
                            let imodule =
                                &self.resolver.modules[module].info().imports[&export.name.symbol];

                            if imodule == module {
                                self.export_all(module);
                            } else {
                                let exports = match &self.resolver.modules[imodule] {
                                    crate::resolve::Module::Normal(m) => m.exports.clone(),
                                    crate::resolve::Module::Virtual(m) => {
                                        m.exports.iter().map(|(_, e)| e.clone()).collect()
                                    }
                                };

                                self.resolver
                                    .modules
                                    .get_mut(module)
                                    .unwrap()
                                    .info_mut()
                                    .exports
                                    .extend(exports);
                            }

                            found = true;
                        }
                        _ => {}
                    }

                    if let (Some(Res::Item(id)), _) =
                        self.resolver
                            .get(Ns::Values, Some(*module), &export.name.symbol)
                    {
                        self.resolver
                            .modules
                            .get_mut(module)
                            .unwrap()
                            .info_mut()
                            .exports
                            .push(Export::Value(*module, id));

                        found = true;
                    }

                    if let (Some(Res::Item(id)), _) =
                        self.resolver
                            .get(Ns::Types, Some(*module), &export.name.symbol)
                    {
                        self.resolver
                            .modules
                            .get_mut(module)
                            .unwrap()
                            .info_mut()
                            .exports
                            .push(Export::Type(*module, id));

                        found = true;
                    }

                    if !found {
                        self.reporter.add(
                            Diagnostic::new(
                                Severity::Error,
                                0021,
                                format!(
                                    "Module '{}' does not contain item '{}'",
                                    module, export.name
                                ),
                            )
                            .label(
                                Severity::Error,
                                export.name.span,
                                None::<String>,
                            ),
                        );
                    }
                }
            }
        }
    }

    pub fn export_all(&mut self, module: &ModuleId) {
        let module = self.resolver.modules.get_mut(module).unwrap().info_mut();

        for (_, res) in module.scopes[Ns::Values][0].iter() {
            if let Res::Item(id) = res {
                module
                    .exports
                    .push(Export::Value(ModuleId::Normal(module.module.name), *id));
            }
        }

        for (_, res) in module.scopes[Ns::Types][0].iter() {
            if let Res::Item(id) = res {
                module
                    .exports
                    .push(Export::Type(ModuleId::Normal(module.module.name), *id));
            }
        }
    }

    pub fn find_export(
        &self,
        module: &ModuleId,
        exports: &mut Vec<(Ident, Export)>,
        name: &Ident,
    ) -> Vec<(Ident, Export)> {
        let exp = exports
            .drain_filter(|(n, _)| n.symbol == name.symbol)
            .collect::<Vec<_>>();

        if exp.is_empty() {
            self.reporter.add(
                Diagnostic::new(
                    Severity::Error,
                    0020,
                    format!("Module '{}' does not export name '{}'", module, name),
                )
                .label(Severity::Error, name.span, None::<String>),
            );
        }

        exp
    }

    pub fn collect_exports(&self, module: &ModuleId) -> Vec<(Ident, Export)> {
        match &self.resolver.modules[module] {
            crate::resolve::Module::Normal(m) => m
                .exports
                .iter()
                .map(|export| {
                    (
                        if let Some(item) = self.items.get(export.id()) {
                            item.name
                        } else {
                            self.imports.0[export.id()].name
                        },
                        *export,
                    )
                })
                .collect(),
            crate::resolve::Module::Virtual(m) => m
                .exports
                .iter()
                .map(|(_, export)| {
                    (
                        if let Some(item) = self.items.get(export.id()) {
                            item.name
                        } else {
                            self.imports.0[export.id()].name
                        },
                        *export,
                    )
                })
                .collect(),
        }
    }

    pub fn register_item(&mut self, item: &ast::Item, top_level: bool) -> ItemId {
        let id = ItemId::new(item);

        if top_level {
            self.resolver
                .modules
                .get_mut(&self.resolver.current_module)
                .unwrap()
                .info_mut()
                .module
                .items
                .push(Id::item(id));
        }

        match &item.kind {
            ast::ItemKind::Extern { .. } => {
                self.resolver.define(
                    Ns::Values,
                    item.name.symbol,
                    item.name.span,
                    Res::Item(Id::item(id)),
                );
            }
            ast::ItemKind::Func { .. } => {
                self.resolver.define(
                    Ns::Values,
                    item.name.symbol,
                    item.name.span,
                    Res::Item(Id::item(id)),
                );
            }
            ast::ItemKind::Var { .. } => {
                if top_level {
                    self.resolver.define(
                        Ns::Values,
                        item.name.symbol,
                        item.name.span,
                        Res::Item(Id::item(id)),
                    );
                }
            }
            ast::ItemKind::Const { .. } => {
                self.resolver.define(
                    Ns::Values,
                    item.name.symbol,
                    item.name.span,
                    Res::Item(Id::item(id)),
                );
            }
            ast::ItemKind::Struct { .. } => {
                self.resolver.define(
                    Ns::Types,
                    item.name.symbol,
                    item.name.span,
                    Res::Item(Id::item(id)),
                );

                self.resolver.define(
                    Ns::Values,
                    item.name.symbol,
                    item.name.span,
                    Res::Item(Id(id, 1)),
                );
            }
            ast::ItemKind::Enum { variants, .. } => {
                self.resolver.define(
                    Ns::Types,
                    item.name.symbol,
                    item.name.span,
                    Res::Item(Id::item(id)),
                );

                for (i, variant) in variants.iter().enumerate() {
                    self.resolver.define(
                        Ns::Values,
                        variant.name.symbol,
                        variant.name.span,
                        Res::Item(Id(id, i as u64 + 1)),
                    );
                }
            }
        }

        id
    }

    pub fn trans_module(&mut self, module: &ast::Module, id: ModuleId) {
        self.resolver.set_module(id);

        for item in &module.items {
            self.trans_item(item, true);
        }
    }

    pub fn trans_item(&mut self, item: &ast::Item, top_level: bool) -> Id {
        let id = Id::item(ItemId::new(item));
        let attrs = item.attrs.clone();

        self.current_item = id.item_id();
        self.local_id = 0;

        match &item.kind {
            ast::ItemKind::Extern { abi, ty } => {
                let ty = self.trans_ty(ty);

                self.items.insert(
                    id,
                    Item {
                        span: item.span,
                        id,
                        attrs,
                        name: item.name,
                        kind: ItemKind::Extern { abi: *abi, ty },
                    },
                );
            }
            ast::ItemKind::Func {
                generics,
                params,
                ret,
                body,
            } => {
                self.resolver.push_rib(Ns::Values, RibKind::Block);
                self.resolver.push_rib(Ns::Types, RibKind::Block);
                self.resolver.push_rib(Ns::Labels, RibKind::Block);

                let generics = self.trans_generics(generics);
                let params = params
                    .iter()
                    .map(|param| {
                        let id = self.next_id();
                        let ty = self.trans_ty(&param.ty);

                        self.resolver.define(
                            Ns::Values,
                            param.name.symbol,
                            param.name.span,
                            Res::Local(id),
                        );

                        self.items.insert(
                            id,
                            Item {
                                span: param.span,
                                id,
                                attrs: Vec::new(),
                                name: param.name,
                                kind: ItemKind::Param { ty },
                            },
                        );

                        id
                    })
                    .collect();

                let ret = self.trans_ty(ret);
                let body = self.trans_block(body);

                self.resolver.pop_rib(Ns::Values);
                self.resolver.pop_rib(Ns::Types);
                self.resolver.pop_rib(Ns::Labels);

                self.items.insert(
                    id,
                    Item {
                        span: item.span,
                        id,
                        attrs,
                        name: item.name,
                        kind: ItemKind::Func {
                            generics,
                            params,
                            ret,
                            body,
                        },
                    },
                );
            }
            ast::ItemKind::Var { ty, val } => {
                let ty = self.trans_ty(ty);
                let val = val.as_ref().map(|expr| self.trans_expr(expr));

                self.items.insert(
                    id,
                    Item {
                        span: item.span,
                        id,
                        attrs,
                        name: item.name,
                        kind: ItemKind::Var {
                            ty,
                            val,
                            global: top_level,
                        },
                    },
                );
            }
            ast::ItemKind::Const { ty, val } => {
                let ty = self.trans_ty(ty);
                let val = self.trans_expr(val);

                self.items.insert(
                    id,
                    Item {
                        span: item.span,
                        id,
                        attrs,
                        name: item.name,
                        kind: ItemKind::Const { ty, val },
                    },
                );
            }
            ast::ItemKind::Struct { generics, fields } => {
                let cons_id = self.next_id();

                self.resolver.push_rib(Ns::Types, RibKind::Block);

                let generics = self.trans_generics(generics);
                let fields = fields
                    .iter()
                    .map(|f| StructField {
                        span: f.span,
                        name: f.name,
                        ty: self.trans_ty(&f.ty),
                    })
                    .collect::<Vec<_>>();

                self.resolver.pop_rib(Ns::Types);

                self.items.insert(
                    id,
                    Item {
                        span: item.span,
                        id,
                        attrs,
                        name: item.name,
                        kind: ItemKind::Struct {
                            generics,
                            fields: fields.clone(),
                        },
                    },
                );

                self.items.insert(
                    cons_id,
                    Item {
                        span: item.span,
                        id: cons_id,
                        attrs: Vec::new(),
                        name: item.name,
                        kind: ItemKind::Ctor {
                            item: id,
                            variant: 0,
                            params: Some(fields),
                        },
                    },
                );
            }
            ast::ItemKind::Enum { generics, variants } => {
                let variant_ids = variants.iter().map(|_| self.next_id()).collect::<Vec<_>>();

                self.resolver.push_rib(Ns::Types, RibKind::Block);

                let generics = self.trans_generics(generics);
                let variants = variants
                    .iter()
                    .zip(variant_ids)
                    .enumerate()
                    .map(|(i, (v, cons_id))| {
                        let fields = v.fields.as_ref().map(|fields| {
                            fields
                                .iter()
                                .map(|f| StructField {
                                    span: f.span,
                                    name: f.name,
                                    ty: self.trans_ty(&f.ty),
                                })
                                .collect()
                        });

                        self.items.insert(
                            cons_id,
                            Item {
                                span: v.span,
                                id: cons_id,
                                attrs: Vec::new(),
                                name: v.name,
                                kind: ItemKind::Ctor {
                                    item: id,
                                    variant: i,
                                    params: fields.clone(),
                                },
                            },
                        );

                        EnumVariant {
                            span: v.span,
                            name: v.name,
                            ctor: cons_id,
                            fields,
                        }
                    })
                    .collect();

                self.resolver.pop_rib(Ns::Types);

                self.items.insert(
                    id,
                    Item {
                        span: item.span,
                        id,
                        attrs,
                        name: item.name,
                        kind: ItemKind::Enum { generics, variants },
                    },
                );
            }
        }

        id
    }

    pub fn trans_generics(&mut self, generics: &ast::Generics) -> Generics {
        Generics {
            span: generics.span,
            params: generics
                .params
                .iter()
                .map(|generic| {
                    let id = self.next_id();

                    self.resolver.define(
                        Ns::Types,
                        generic.name.symbol,
                        generic.name.span,
                        Res::Local(id),
                    );

                    Generic {
                        span: generic.span,
                        id,
                        name: generic.name,
                    }
                })
                .collect(),
        }
    }

    pub fn trans_block(&mut self, block: &ast::Block) -> Block {
        self.resolver.push_rib(Ns::Modules, RibKind::Local);
        self.resolver.push_rib(Ns::Values, RibKind::Local);
        self.resolver.push_rib(Ns::Types, RibKind::Local);

        let mut stmts = Vec::new();
        let mut scopes = 0;

        for stmt in &block.stmts {
            if let ast::StmtKind::Item(item) = &stmt.kind {
                self.register_item(item, false);
            }
        }

        for stmt in &block.stmts {
            let kind = match &stmt.kind {
                ast::StmtKind::Item(item) => {
                    if let ast::ItemKind::Var { ty, val } = &item.kind {
                        let id = self.next_id();
                        let ty = self.trans_ty(ty);
                        let val = val.as_ref().map(|expr| self.trans_expr(expr));

                        self.resolver.push_rib(Ns::Values, RibKind::Local);
                        self.resolver.define(
                            Ns::Values,
                            item.name.symbol,
                            item.name.span,
                            Res::Local(id),
                        );

                        self.items.insert(
                            id,
                            Item {
                                span: item.span,
                                id,
                                attrs: item.attrs.clone(),
                                name: item.name,
                                kind: ItemKind::Var {
                                    ty,
                                    val,
                                    global: false,
                                },
                            },
                        );

                        scopes += 1;

                        StmtKind::Item(id)
                    } else {
                        StmtKind::Item(self.trans_item(item, false))
                    }
                }
                ast::StmtKind::Semi(expr) => StmtKind::Semi(self.trans_expr(expr)),
                ast::StmtKind::Expr(expr) => StmtKind::Expr(self.trans_expr(expr)),
            };

            stmts.push(Stmt {
                span: stmt.span,
                kind,
            });
        }

        for _ in 0..scopes {
            self.resolver.pop_rib(Ns::Values);
        }

        self.resolver.pop_rib(Ns::Modules);
        self.resolver.pop_rib(Ns::Values);
        self.resolver.pop_rib(Ns::Types);

        Block {
            span: block.span,
            stmts,
        }
    }

    pub fn trans_expr(&mut self, expr: &ast::Expr) -> Id {
        let id = self.next_id();
        let kind = match &expr.kind {
            ast::ExprKind::Parens { inner } => return self.trans_expr(inner),
            ast::ExprKind::Ident { name } => {
                if let (Some(res), _) = self.resolver.get(Ns::Values, None, &name.symbol) {
                    ExprKind::Path { res }
                } else {
                    self.reporter.add(
                        Diagnostic::new(Severity::Error, 0004, format!("Unknown value '{}'", name))
                            .label(Severity::Error, name.span, None::<String>),
                    );

                    ExprKind::Err
                }
            }
            ast::ExprKind::Apply { expr, args } => ExprKind::Apply {
                expr: self.trans_expr(expr),
                args: args.iter().map(|a| self.trans_ty(a)).collect(),
            },
            ast::ExprKind::Int { val } => ExprKind::Int { val: *val },
            ast::ExprKind::Float { bits } => ExprKind::Float { bits: *bits },
            ast::ExprKind::Char { val } => ExprKind::Char { val: *val },
            ast::ExprKind::String { val } => ExprKind::String { val: val.clone() },
            ast::ExprKind::Type { ty } => ExprKind::Type {
                ty: self.trans_ty(ty),
            },
            ast::ExprKind::Array { exprs } => ExprKind::Array {
                exprs: exprs.iter().map(|e| self.trans_expr(e)).collect(),
            },
            ast::ExprKind::Tuple { exprs } => ExprKind::Tuple {
                exprs: exprs.iter().map(|e| self.trans_expr(e)).collect(),
            },
            ast::ExprKind::Range { lo, hi } => ExprKind::Range {
                lo: self.trans_expr(lo),
                hi: self.trans_expr(hi),
            },
            ast::ExprKind::Block { block } => ExprKind::Block {
                block: self.trans_block(block),
            },
            ast::ExprKind::Call { func, args } => {
                let func = self.trans_expr(func);
                let args = args
                    .iter()
                    .map(|arg| Arg {
                        span: arg.span,
                        name: arg.name,
                        value: self.trans_expr(&arg.value),
                    })
                    .collect();

                ExprKind::Call { func, args }
            }
            ast::ExprKind::Field { obj, field } => {
                if let ast::ExprKind::Ident { name } = &obj.kind {
                    if let (Some(Res::Module(m)), _) =
                        self.resolver.get(Ns::Modules, None, &name.symbol)
                    {
                        if let Some(res) = self.resolver.get_virt(Ns::Values, m, &field.symbol) {
                            ExprKind::Path { res }
                        } else {
                            self.reporter.add(
                                Diagnostic::new(
                                    Severity::Error,
                                    0004,
                                    format!("Unknown value '{}.{}'", obj, field),
                                )
                                .label(
                                    Severity::Error,
                                    expr.span,
                                    None::<String>,
                                ),
                            );

                            ExprKind::Err
                        }
                    } else {
                        ExprKind::Field {
                            obj: self.trans_expr(obj),
                            field: *field,
                        }
                    }
                } else {
                    ExprKind::Field {
                        obj: self.trans_expr(obj),
                        field: *field,
                    }
                }
            }
            ast::ExprKind::Index { list, index } => ExprKind::Index {
                list: self.trans_expr(list),
                index: self.trans_expr(index),
            },
            ast::ExprKind::Slice { list, low, high } => ExprKind::Slice {
                list: self.trans_expr(list),
                low: low.as_ref().map(|l| self.trans_expr(l)),
                high: high.as_ref().map(|h| self.trans_expr(h)),
            },
            ast::ExprKind::Ref { expr } => ExprKind::Ref {
                expr: self.trans_expr(expr),
            },
            ast::ExprKind::Deref { expr } => ExprKind::Deref {
                expr: self.trans_expr(expr),
            },
            ast::ExprKind::TypeOf { expr } => ExprKind::TypeOf {
                expr: self.trans_expr(expr),
            },
            ast::ExprKind::Cast { expr, ty } => ExprKind::Cast {
                expr: self.trans_expr(expr),
                ty: self.trans_ty(ty),
            },
            ast::ExprKind::Box { expr } => ExprKind::Box {
                expr: self.trans_expr(expr),
            },
            ast::ExprKind::Unbox { expr } => ExprKind::Unbox {
                expr: self.trans_expr(expr),
            },
            ast::ExprKind::Assign { lhs, rhs } => ExprKind::Assign {
                lhs: self.trans_expr(lhs),
                rhs: self.trans_expr(rhs),
            },
            ast::ExprKind::AssignOp { op, lhs, rhs } => {
                let lhs_ = self.trans_expr(lhs);
                let rhs = self.trans_expr(&ast::Expr {
                    span: expr.span,
                    kind: ast::ExprKind::BinOp {
                        op: *op,
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    },
                });

                ExprKind::Assign { lhs: lhs_, rhs }
            }
            ast::ExprKind::BinOp { op, lhs, rhs } => ExprKind::BinOp {
                op: *op,
                lhs: self.trans_expr(lhs),
                rhs: self.trans_expr(rhs),
            },
            ast::ExprKind::UnOp { op, rhs } => ExprKind::UnOp {
                op: *op,
                rhs: self.trans_expr(rhs),
            },
            ast::ExprKind::IfElse { cond, then, else_ } => ExprKind::IfElse {
                cond: self.trans_expr(cond),
                then: self.trans_block(then),
                else_: else_.as_ref().map(|e| self.trans_block(e)),
            },
            ast::ExprKind::While { label, cond, body } => {
                let label = if let Some(label) = label {
                    let id = self.next_id();

                    self.resolver
                        .define(Ns::Labels, label.symbol, label.span, Res::Label(id));

                    Some(id)
                } else {
                    None
                };

                let cond = self.trans_expr(cond);
                let body = self.trans_block(body);

                ExprKind::While { label, cond, body }
            }
            ast::ExprKind::Defer { expr } => ExprKind::Defer {
                expr: self.trans_expr(expr),
            },
            _ => unimplemented!("{}", expr),
        };

        self.exprs.insert(
            id,
            Expr {
                span: expr.span,
                id,
                kind,
            },
        );

        id
    }

    pub fn trans_ty(&mut self, ty: &ast::Type) -> Id {
        let id = self.next_id();
        let kind = match &ty.kind {
            ast::TypeKind::Parens { inner } => return self.trans_ty(inner),
            ast::TypeKind::Infer => TypeKind::Infer,
            ast::TypeKind::Path { module, name } => {
                if let (Some(Res::Module(m)), _) =
                    self.resolver.get(Ns::Modules, None, &module.symbol)
                {
                    if let Some(res) = self.resolver.get_virt(Ns::Types, m, &name.symbol) {
                        TypeKind::Path { res }
                    } else {
                        self.reporter.add(
                            Diagnostic::new(
                                Severity::Error,
                                0005,
                                format!("Unknown type '{}.{}'", module, name),
                            )
                            .label(
                                Severity::Error,
                                ty.span,
                                None::<String>,
                            ),
                        );

                        TypeKind::Err
                    }
                } else {
                    self.reporter.add(
                        Diagnostic::new(
                            Severity::Error,
                            0022,
                            format!("Unknown module '{}'", module),
                        )
                        .label(
                            Severity::Error,
                            module.span,
                            None::<String>,
                        ),
                    );

                    TypeKind::Err
                }
            }
            ast::TypeKind::Ident { name } => {
                if let (Some(res), _) = self.resolver.get(Ns::Types, None, &name.symbol) {
                    TypeKind::Path { res }
                } else {
                    self.reporter.add(
                        Diagnostic::new(Severity::Error, 0005, format!("Unknown type '{}'", name))
                            .label(Severity::Error, name.span, None::<String>),
                    );

                    TypeKind::Err
                }
            }
            ast::TypeKind::Ref { ty, mut_ } => TypeKind::Ref {
                mut_: *mut_,
                to: self.trans_ty(ty),
            },
            ast::TypeKind::Array { of, len } => TypeKind::Array {
                of: self.trans_ty(of),
                len: *len,
            },
            ast::TypeKind::Slice { of } => TypeKind::Slice {
                of: self.trans_ty(of),
            },
            ast::TypeKind::Func { params, ret } => {
                let params = params
                    .iter()
                    .map(|param| self.trans_ty_param(param))
                    .collect();
                let ret = self.trans_ty(ret);

                TypeKind::Func { params, ret }
            }
            ast::TypeKind::Tuple { tys } => {
                let tys = tys.iter().map(|ty| self.trans_ty(ty)).collect();

                TypeKind::Tuple { tys }
            }
            ast::TypeKind::Subst { ty, args } => {
                let ty = self.trans_ty(ty);
                let args = args.iter().map(|a| self.trans_ty(a)).collect();

                TypeKind::Subst { ty, args }
            }
            ast::TypeKind::Forall { gen, ty } => {
                self.resolver.push_rib(Ns::Types, RibKind::Local);

                let gen = self.trans_generics(gen);
                let ty = self.trans_ty(ty);

                self.resolver.pop_rib(Ns::Types);

                TypeKind::Forall { gen, ty }
            }
        };

        self.types.insert(
            id,
            Type {
                span: ty.span,
                id,
                kind,
            },
        );

        id
    }

    pub fn trans_ty_param(&mut self, param: &ast::TypeParam) -> TypeParam {
        TypeParam {
            span: param.span,
            name: param.name,
            ty: self.trans_ty(&param.ty),
        }
    }
}

use crate::*;
use diagnostics::{Diagnostic, Reporter, Severity};
use resolve::{Ns, Res, Resolver, RibKind};
use std::collections::BTreeMap;
use syntax::ast;

pub fn convert(reporter: &Reporter, ast: &ast::Package) -> Package {
    let mut converter = Converter::new(reporter);
    let root = ItemId::new(&ast.module);
    let package_name = Ident {
        symbol: Symbol::new(ast.span.file.name.file_stem().unwrap().to_str().unwrap()),
        span: Span::empty(ast.span.file),
    };

    converter.register_module(&ast.module, &package_name, root);
    converter.trans_module(&ast.module, root);
    converter.finish()
}

pub struct Converter<'a> {
    reporter: &'a Reporter,
    resolver: Resolver<'a>,
    items: BTreeMap<ItemId, Item>,
    exprs: BTreeMap<Id, Expr>,
    types: BTreeMap<Id, Type>,
    current_item: ItemId,
    local_id: u64,
}

impl<'a> Converter<'a> {
    pub fn new(reporter: &'a Reporter) -> Self {
        Converter {
            reporter,
            resolver: Resolver::new(reporter),
            items: BTreeMap::new(),
            exprs: BTreeMap::new(),
            types: BTreeMap::new(),
            current_item: ItemId(0),
            local_id: 0,
        }
    }

    pub fn finish(self) -> Package {
        Package {
            items: self.items,
            exprs: self.exprs,
            types: self.types,
        }
    }

    fn next_id(&mut self) -> Id {
        self.local_id += 1;

        Id(self.current_item, self.local_id)
    }

    pub fn register_module(&mut self, module: &ast::Module, name: &Ident, id: ItemId) {
        let parent = self.resolver.current_module;

        self.resolver
            .define(Ns::Modules, name.symbol, name.span, Res::Module(id));

        self.resolver.add_module(id);
        self.resolver.set_module(id);
        self.resolver
            .define(Ns::Modules, Symbol::new("self"), name.span, Res::Module(id));

        if !parent.is_null() {
            self.resolver.define(
                Ns::Modules,
                Symbol::new("super"),
                name.span,
                Res::Module(parent),
            );
        }

        for item in &module.items {
            self.register_item(item, true);
        }

        self.resolver.set_module(parent);
    }

    pub fn register_item(&mut self, item: &ast::Item, top_level: bool) -> ItemId {
        let id = ItemId::new(item);

        match &item.kind {
            ast::ItemKind::Module { module } => {
                self.register_module(module, &item.name, id);
            }
            ast::ItemKind::Extern { .. } => {
                self.resolver
                    .define(Ns::Values, item.name.symbol, item.name.span, Res::Item(id));
            }
            ast::ItemKind::Func { .. } => {
                self.resolver
                    .define(Ns::Values, item.name.symbol, item.name.span, Res::Item(id));
            }
            ast::ItemKind::Var { .. } => {
                self.resolver.define(
                    Ns::Values,
                    item.name.symbol,
                    item.name.span,
                    if top_level {
                        Res::Item(id)
                    } else {
                        Res::Local(id)
                    },
                );
            }
        }

        id
    }

    pub fn trans_module(&mut self, module: &ast::Module, id: ItemId) {
        let parent = self.resolver.current_module;

        self.resolver.set_module(id);

        for item in &module.items {
            self.trans_item(item, true);
        }

        self.resolver.set_module(parent);
    }

    pub fn trans_item(&mut self, item: &ast::Item, top_level: bool) -> ItemId {
        let id = ItemId::new(item);

        self.current_item = id;
        self.local_id = 0;

        match &item.kind {
            ast::ItemKind::Module { module } => {
                self.trans_module(module, id);
            }
            ast::ItemKind::Extern { abi, ty } => {
                let ty = self.trans_ty(ty);

                self.items.insert(
                    id,
                    Item {
                        span: item.span,
                        id,
                        name: item.name,
                        kind: ItemKind::Extern { abi: *abi, ty },
                    },
                );
            }
            ast::ItemKind::Func { params, ret, body } => {
                self.resolver.push_rib(Ns::Values, RibKind::Block);
                self.resolver.push_rib(Ns::Types, RibKind::Block);
                self.resolver.push_rib(Ns::Labels, RibKind::Block);

                let params = params
                    .iter()
                    .map(|param| {
                        let id = ItemId::new(param);
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
                        name: item.name,
                        kind: ItemKind::Func { params, ret, body },
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
                        name: item.name,
                        kind: ItemKind::Var {
                            ty,
                            val,
                            global: top_level,
                        },
                    },
                );
            }
        }

        id
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
                    if let ast::ItemKind::Var { .. } = &item.kind {
                        self.resolver.push_rib(Ns::Values, RibKind::Local);
                        scopes += 1;
                    }

                    StmtKind::Item(self.trans_item(item, false))
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
            ast::ExprKind::Path { path } => {
                if let Some(res) = self.resolver.get_path(Ns::Values, path) {
                    ExprKind::Path { res }
                } else {
                    self.reporter.add(
                        Diagnostic::new(Severity::Error, 0004, format!("Unknown value '{}'", path))
                            .label(Severity::Error, path.span, None::<String>),
                    );
                    ExprKind::Err
                }
            }
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
            ast::ExprKind::Field { obj, field } => ExprKind::Field {
                obj: self.trans_expr(obj),
                field: *field,
            },
            ast::ExprKind::Index { list, index } => ExprKind::Index {
                list: self.trans_expr(list),
                index: self.trans_expr(index),
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
            ast::TypeKind::Infer => TypeKind::Infer,
            ast::TypeKind::Path { path } => {
                if let Some(res) = self.resolver.get_path(Ns::Types, path) {
                    TypeKind::Path { res }
                } else {
                    self.reporter.add(
                        Diagnostic::new(Severity::Error, 0005, format!("Unknown type '{}'", path))
                            .label(Severity::Error, path.span, None::<String>),
                    );

                    TypeKind::Err
                }
            }
            ast::TypeKind::Ref { ty, mut_ } => TypeKind::Ref {
                mut_: *mut_,
                to: self.trans_ty(ty),
            },
            ast::TypeKind::Func { params, ret } => {
                let params = params
                    .iter()
                    .map(|param| self.trans_ty_param(param))
                    .collect();
                let ret = self.trans_ty(ret);

                TypeKind::Func { params, ret }
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

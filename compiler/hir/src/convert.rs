use crate::*;
use diagnostics::{Diagnostic, Reporter, Severity};
use resolve::{Ns, Res, Resolver, RibKind};
use std::collections::BTreeMap;
use syntax::ast;

pub fn convert(reporter: &Reporter, ast: &ast::Package) -> (Package, resolve::ModuleStructure) {
    let root = ItemId::new(&ast.module);
    let mut converter = Converter::new(reporter, root);
    let package_name = Ident {
        symbol: Symbol::new(ast.span.file.name.file_stem().unwrap().to_str().unwrap()),
        span: Span::empty(ast.span.file),
    };

    converter.register_module(&ast.module, &package_name, root);
    converter.trans_module(&ast.module, root);
    converter.finish(package_name, root)
}

pub struct Converter<'a> {
    reporter: &'a Reporter,
    resolver: Resolver<'a>,
    items: BTreeMap<Id, Item>,
    exprs: BTreeMap<Id, Expr>,
    types: BTreeMap<Id, Type>,
    package_id: ItemId,
    current_item: ItemId,
    local_id: u64,
}

impl<'a> Converter<'a> {
    pub fn new(reporter: &'a Reporter, package_id: ItemId) -> Self {
        Converter {
            reporter,
            resolver: Resolver::new(reporter),
            items: BTreeMap::new(),
            exprs: BTreeMap::new(),
            types: BTreeMap::new(),
            package_id,
            current_item: ItemId(0),
            local_id: 0,
        }
    }

    pub fn finish(self, package: Ident, root: ItemId) -> (Package, resolve::ModuleStructure) {
        (
            Package {
                name: package.symbol,
                items: self.items,
                exprs: self.exprs,
                types: self.types,
            },
            self.resolver.module_structure(package.symbol, &root),
        )
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

        self.resolver.define(
            Ns::Modules,
            Symbol::new("@package"),
            name.span,
            Res::Module(self.package_id),
        );

        self.resolver.define(
            Ns::Modules,
            Symbol::new("@self"),
            name.span,
            Res::Module(id),
        );

        if !parent.is_null() {
            self.resolver.define(
                Ns::Modules,
                Symbol::new("@super"),
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

    pub fn trans_module(&mut self, module: &ast::Module, id: ItemId) {
        let parent = self.resolver.current_module;

        self.resolver.set_module(id);

        for item in &module.items {
            self.trans_item(item, true);
        }

        self.resolver.set_module(parent);
    }

    pub fn trans_item(&mut self, item: &ast::Item, top_level: bool) -> Id {
        let id = Id::item(ItemId::new(item));
        let attrs = item.attrs.clone();

        self.current_item = id.item_id();
        self.local_id = 0;

        match &item.kind {
            ast::ItemKind::Module { module } => {
                self.trans_module(module, id.item_id());
            }
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

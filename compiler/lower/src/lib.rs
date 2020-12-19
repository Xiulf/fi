#![feature(label_break_value)]

use hir::ir as hir;
use lowlang::ir;
use std::collections::HashMap;
use std::sync::Arc;

#[salsa::query_group(LowerDatabaseStorage)]
pub trait LowerDatabase: check::TypeDatabase {
    fn lower(&self, lib: hir::LibId, module: hir::ModuleId) -> Arc<ir::Module>;
}

pub fn lower(db: &dyn LowerDatabase, lib: hir::LibId, module: hir::ModuleId) -> Arc<ir::Module> {
    let file = db.module_tree(lib).file(module);
    let hir = db.module_hir(file);
    let mut converter = Converter::new(db);

    converter.convert(&hir);

    let mut low = converter.finish();

    println!("{}", low);

    required_optimizations(db, lib, &mut low);

    Arc::new(low)
}

fn required_optimizations(db: &dyn LowerDatabase, lib: hir::LibId, low: &mut ir::Module) {
    use lowlang::analysis::{analyze, copy, generic, thunk, type_info, witness};

    analyze(witness::WitnessAnalyzer, low);
    analyze(thunk::ThunkAnalyzer::new(), low);
    analyze(generic::GenericAnalyzer, low);
    analyze(copy::CopyAnalyzer, low);
    analyze(type_info::TypeInfoAnalyzer::new(&db.target(lib)), low);
}

pub struct Converter<'db> {
    db: &'db dyn LowerDatabase,
    decls: ir::Decls,
    impls: ir::Impls,
    bodies: ir::Bodies,
}

pub struct BodyConverter<'db, 'c> {
    db: &'db dyn LowerDatabase,
    hir: &'db hir::Module,
    types: Arc<check::TypeCheckResult>,
    builder: ir::Builder<'c>,
    locals: HashMap<hir::HirId, ir::Local>,
}

impl<'db> Converter<'db> {
    pub fn new(db: &'db dyn LowerDatabase) -> Self {
        Converter {
            db,
            decls: ir::Decls::new(),
            impls: ir::Impls::new(),
            bodies: ir::Bodies::new(),
        }
    }

    pub fn finish(self) -> ir::Module {
        ir::Module {
            decls: self.decls,
            impls: self.impls,
            bodies: self.bodies,
        }
    }

    pub fn convert(&mut self, hir: &hir::Module) {
        for &id in &hir.imports {
            let ty = self.db.typecheck(id);
            let declid = self.decls.next_idx();

            self.decls.insert(
                declid,
                ir::Decl {
                    id: declid,
                    name: self.link_name(id),
                    ty: lower_type(self.db, &ty.ty),
                    linkage: ir::Linkage::Import,
                },
            );
        }

        for (_, item) in &hir.items {
            match &item.kind {
                hir::ItemKind::Func { body, .. } => {
                    let body = &hir.bodies[body];
                    let ty = self.db.typecheck(item.id.owner);
                    let declid = self.decls.next_idx();

                    self.decls.insert(
                        declid,
                        ir::Decl {
                            id: declid,
                            name: self.link_name(item.id.owner),
                            ty: lower_type(self.db, &ty.ty),
                            linkage: ir::Linkage::Export,
                        },
                    );

                    let bodyid = self.bodies.next_idx();
                    let mut b = ir::Body::new(bodyid, declid);
                    let builder = ir::Builder::new(&mut b);
                    let conv = BodyConverter::new(self.db, hir, ty, builder);

                    conv.convert(body);
                    self.bodies.insert(bodyid, b);
                }
                hir::ItemKind::Const { .. } => unimplemented!(),
                hir::ItemKind::Foreign { .. } => {
                    if !item.is_intrinsic() {
                        let ty = self.db.typecheck(item.id.owner);
                        let declid = self.decls.next_idx();

                        self.decls.insert(
                            declid,
                            ir::Decl {
                                id: declid,
                                name: item.name.to_string(),
                                ty: lower_type(self.db, &ty.ty),
                                linkage: ir::Linkage::Import,
                            },
                        );
                    }
                }
                _ => {}
            }
        }
    }

    fn link_name(&self, id: hir::DefId) -> String {
        let file = self.db.module_tree(id.lib).file(id.module);
        let hir = self.db.module_hir(file);
        let def = hir.def(id);

        let name = match def {
            hir::Def::Item(item) => {
                if item.is_main() {
                    return String::from("main");
                } else if item.is_no_mangle() {
                    return item.name.to_string();
                } else {
                    format!("{}.{}", hir.name, item.name)
                }
            }
            hir::Def::TraitItem(item) => format!("{}.{}", hir.name, item.name),
            hir::Def::ImplItem(item) => format!("{}.{}", hir.name, item.name),
        };

        mangling::mangle(name.bytes())
    }
}

impl<'db, 'c> BodyConverter<'db, 'c> {
    pub fn new(
        db: &'db dyn LowerDatabase,
        hir: &'db hir::Module,
        types: Arc<check::TypeCheckResult>,
        builder: ir::Builder<'c>,
    ) -> Self {
        BodyConverter {
            db,
            hir,
            types,
            builder,
            locals: HashMap::new(),
        }
    }

    pub fn convert(mut self, body: &hir::Body) {
        self.create_header(&body.params);

        let entry = self.builder.create_block();

        self.builder.set_block(entry);
        self.builder.return_();
    }

    fn create_header(&mut self, params: &[hir::Param]) {
        use check::ty::Type;
        let mut ty = &self.types.ty;

        if let Type::ForAll(_, ty2) = &**ty {
            ty = ty2;
        }

        if let Type::Func(param_tys, ret) = &**ty {
            self.builder.create_ret(lower_type(self.db, ret));

            for (param, ty) in params.iter().zip(param_tys) {
                let local = self.builder.create_arg(lower_type(self.db, &ty));

                self.locals.insert(param.id, local);
            }
        } else {
            self.builder.create_ret(lower_type(self.db, ty));
        }
    }
}

fn lower_type(db: &dyn LowerDatabase, ty: &check::ty::Ty) -> ir::Type {
    use check::ty::Type;

    match &**ty {
        Type::Error => unreachable!(),
        Type::Int(_) => unreachable!(),
        Type::Infer(_) => unreachable!(),
        Type::Var(var) => ir::Type::Opaque(var.to_string()),
        Type::TypeOf(id) => lower_type(db, &db.typecheck(*id).ty),
        Type::ForAll(_, ty) => lower_type(db, ty),
        Type::Func(args, ret) => ir::Type::Func(ir::Signature {
            params: args.iter().map(|a| lower_type(db, a)).collect(),
            rets: vec![lower_type(db, ret)],
        }),
        Type::Tuple(tys) => ir::Type::Tuple(tys.iter().map(|t| lower_type(db, t)).collect()),
        Type::Record(fields, None) => {
            ir::Type::Tuple(fields.iter().map(|f| lower_type(db, &f.ty)).collect())
        }
        Type::Record(_fields, Some(_tail)) => unimplemented!(),
        Type::App(base, _, args) => match &**base {
            Type::Data(def) => {
                if *def == db.lang_items().ptr_ty().owner {
                    assert_eq!(args.len(), 1);

                    ir::Type::Ptr(Box::new(lower_type(db, &args[0])))
                } else if *def == db.lang_items().array_ty().owner {
                    unimplemented!();
                } else if *def == db.lang_items().slice_ty().owner {
                    unimplemented!();
                } else if *def == db.lang_items().type_info().owner {
                    ir::Type::Type(String::new())
                } else if *def == db.lang_items().vwt().owner {
                    ir::Type::Vwt(String::new())
                } else {
                    lower_type(db, base)
                }
            }
            _ => lower_type(db, base),
        },
        Type::Data(id) => {
            let file = db.module_tree(id.lib).file(id.module);
            let hir = db.module_hir(file);
            let def = hir.def(*id);

            if let hir::Def::Item(item) = def {
                if let Some(repr) = item.repr() {
                    return match repr {
                        "u8" => ir::Type::U8,
                        "u16" => ir::Type::U16,
                        "u32" => ir::Type::U32,
                        "u64" => ir::Type::U64,
                        "u128" => ir::Type::U128,
                        "i8" => ir::Type::I8,
                        "i16" => ir::Type::I16,
                        "i32" => ir::Type::I32,
                        "i64" => ir::Type::I64,
                        "i128" => ir::Type::I128,
                        "f32" => ir::Type::F32,
                        "f64" => ir::Type::F64,
                        _ => unreachable!("unknown repr {}", repr),
                    };
                }
            }

            let variants = db.variants(*id);

            if variants.len() == 1 {
                ir::Type::Tuple(variants[0].tys.iter().map(|t| lower_type(db, t)).collect())
            } else {
                unimplemented!();
            }
        }
    }
}

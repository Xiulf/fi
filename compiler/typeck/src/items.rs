use crate::ctx::*;
use crate::error::*;
use crate::ty::*;
use hir::ir;
use std::collections::HashMap;

impl<'db> Ctx<'db> {
    pub fn typeck_def(&mut self, id: ir::DefId) -> Ty {
        if let Some(res) = self.module_types.items.get(&id) {
            res.ty.clone()
        } else if id.lib != self.db.lib() {
            let external = self.db.external_types(id.lib, id.module);

            external.types[&id].clone()
        } else if id.module != self.module {
            self.db.typeck_module(id.lib, id.module).items[&id].ty.clone()
        } else {
            let file = self.db.module_tree(id.lib).file(id.module);
            let hir = self.db.module_hir(file);
            let def = hir.def(id);
            let prev_tys = std::mem::replace(&mut self.tys, HashMap::new());
            let prev_bounds = std::mem::replace(&mut self.bounds, HashMap::new());
            let prev_subst = std::mem::replace(&mut self.subst, Substitution::empty());
            let res: Result<Ty> = match def {
                | ir::Def::Item(item) => match &item.kind {
                    | ir::ItemKind::Foreign { ty, .. } => {
                        try {
                            let ty = self.hir_ty(ty);
                            let ty_kind = self.ty_kind(ty.span(), self.file);
                            let elab_ty = self.check_kind(ty, ty_kind)?;

                            self.subst_type(elab_ty)
                        }
                    },
                    | ir::ItemKind::Func { ty, body } => {
                        try {
                            let ty = self.hir_ty(ty);

                            self.tys.insert(item.id, ty.clone());

                            let (ty, should_generalize) = if let Type::Unknown(_) = *ty {
                                let infer = self.infer_body(item.span, &hir.bodies[body])?;
                                let _ = self.unify_types(ty.clone(), infer.clone())?;

                                (infer, true)
                            } else {
                                self.check_body(item.span, &hir.bodies[body], ty.clone())?;
                                (ty, false)
                            };

                            let unsolved = self.solve_ctnts(should_generalize)?;
                            let ty = self.subst_type(ty);
                            let ty = self.constrain(unsolved, ty);

                            self.generalize(ty, item.id.owner)
                        }
                    },
                    | ir::ItemKind::Static { ty, body } => {
                        try {
                            let ty = self.hir_ty(ty);

                            self.tys.insert(item.id, ty.clone());

                            let (ty, should_generalize) = if let Type::Unknown(_) = *ty {
                                let infer = self.infer_body(item.span, &hir.bodies[body])?;
                                let _ = self.unify_types(ty, infer.clone())?;

                                (infer, true)
                            } else {
                                self.check_body(item.span, &hir.bodies[body], ty.clone())?;
                                (ty, false)
                            };

                            let unsolved = self.solve_ctnts(should_generalize)?;
                            let ty = self.subst_type(ty);
                            let ty = self.constrain(unsolved, ty);

                            self.generalize(ty, item.id.owner)
                        }
                    },
                    | ir::ItemKind::Const { ty, body } => {
                        try {
                            let ty = self.hir_ty(ty);

                            self.tys.insert(item.id, ty.clone());

                            let (ty, should_generalize) = if let Type::Unknown(_) = *ty {
                                let infer = self.infer_body(item.span, &hir.bodies[body])?;
                                let _ = self.unify_types(ty, infer.clone())?;

                                (infer, true)
                            } else {
                                self.check_body(item.span, &hir.bodies[body], ty.clone())?;
                                (ty, false)
                            };

                            let unsolved = self.solve_ctnts(should_generalize)?;
                            let ty = self.subst_type(ty);
                            let ty = self.constrain(unsolved, ty);

                            self.generalize(ty, item.id.owner)
                        }
                    },
                    | ir::ItemKind::Alias { head, value } => {
                        try {
                            let mut ty = self.hir_ty(value);

                            if !head.vars.is_empty() {
                                let vars = head
                                    .vars
                                    .iter()
                                    .map(|v| (TypeVar(v.id), Some(self.ty_kind(v.span, self.file))))
                                    .collect::<Vec<_>>();

                                for (v, k) in vars.into_iter().rev() {
                                    ty = Ty::forall(item.span, self.file, v, k, ty, None);
                                }
                            }

                            let kind = self.hir_ty(&head.kind);

                            self.check_kind(ty, kind)?
                        }
                    },
                    | ir::ItemKind::Data { head, body } => {
                        for v in &head.vars {
                            let kind = if let ir::TypeKind::Infer = &v.kind.kind {
                                self.fresh_kind(v.span, self.file)
                            } else {
                                self.hir_ty(&v.kind)
                            };

                            self.tys.insert(v.id, kind);
                        }

                        let expected = self.hir_ty(&head.kind);

                        self.tys.insert(item.id, expected.clone());

                        let variants = body
                            .iter()
                            .map(|ctor_id| {
                                let ctor_item = &hir.items[ctor_id];
                                let (_, tys) = ctor_item.ctor();
                                let tys = tys.iter().map(|t| self.hir_ty(t)).collect::<List<_>>();
                                let res: Result<Ty> = try {
                                    let mut ty = Ty::ctor(ctor_item.span, self.file, item.id.owner);

                                    if !head.vars.is_empty() {
                                        for var in &head.vars {
                                            ty = Ty::app(ctor_item.span, self.file, ty, Ty::var(var.span, self.file, TypeVar(var.id)));
                                        }
                                    }

                                    if !tys.is_empty() {
                                        let func_ty = self.func_ty(item.span, self.file);

                                        for arg in (&tys).into_iter().rev() {
                                            ty = Ty::app(item.span, self.file, Ty::app(item.span, self.file, func_ty.clone(), arg), ty);
                                        }
                                    }

                                    if !head.vars.is_empty() {
                                        let vars = head
                                            .vars
                                            .iter()
                                            .map(|v| (TypeVar(v.id), Some(self.subst_type(self.tys[&v.id].clone()))))
                                            .collect::<Vec<_>>();

                                        for (v, k) in vars.into_iter().rev() {
                                            ty = Ty::forall(ctor_item.span, self.file, v, k, ty, None);
                                        }
                                    }

                                    let ty_kind = self.ty_kind(item.span, self.file);
                                    let ty = self.check_kind(ty, ty_kind)?;

                                    ty
                                };

                                match res {
                                    | Ok(ty) => {
                                        self.module_types.items.insert(ctor_id.owner, crate::TypecheckResult {
                                            ty,
                                            tys: HashMap::new(),
                                            bounds: HashMap::new(),
                                        });
                                    },
                                    | Err(e) => e.report(self.db),
                                }

                                Variant { id: ctor_id.owner, tys }
                            })
                            .collect();

                        self.module_types.variants.insert(item.id.owner, Variants {
                            vars: head.vars.iter().map(|v| TypeVar(v.id)).collect(),
                            variants,
                        });

                        let mut kind = self.ty_kind(item.span, self.file);

                        if !head.vars.is_empty() {
                            let func_ty = self.func_ty(item.span, self.file);

                            for v in head.vars.iter().rev() {
                                let param = self.tys[&v.id].clone();
                                let param = self.subst_type(param);

                                kind = Ty::app(item.span, self.file, Ty::app(item.span, self.file, func_ty.clone(), param), kind);
                            }
                        }

                        try {
                            let expected = self.subst_type(expected);
                            let _ = self.subsumes_kind(kind, expected.clone())?;

                            self.subst_type(expected)
                        }
                    },
                    | ir::ItemKind::DataCtor { data, .. } => {
                        self.typeck_def(data.owner);

                        if let Some(ty) = self.module_types.items.get(&id) {
                            return ty.ty.clone();
                        } else {
                            return Ty::error(item.span, self.file);
                        }
                    },
                    | ir::ItemKind::Class { head, body } => {
                        for v in &head.vars {
                            let kind = if let ir::TypeKind::Infer = &v.kind.kind {
                                self.fresh_kind(v.span, self.file)
                            } else {
                                self.hir_ty(&v.kind)
                            };

                            self.tys.insert(v.id, kind);
                        }

                        for item_ref in &body.items {
                            let item = &hir.class_items[&item_ref.id];

                            match &item.kind {
                                | ir::ClassItemKind::Func { ty } => {
                                    let item_ty: Result<Ty> = try {
                                        let ty = self.hir_ty(ty);
                                        let mut ty = Ty::ctnt(
                                            ty.span(),
                                            self.file,
                                            Ctnt {
                                                span: ty.span(),
                                                file: ty.file(),
                                                class: item.owner.owner,
                                                tys: head.vars.iter().map(|v| Ty::var(v.span, self.file, TypeVar(v.id))).collect(),
                                            },
                                            ty,
                                        );

                                        for v in head.vars.iter().rev() {
                                            let kind = self.tys[&v.id].clone();
                                            let kind = self.subst_type(kind);

                                            ty = Ty::forall(item.span, self.file, TypeVar(v.id), kind, ty, None);
                                        }

                                        let ty_kind = self.ty_kind(item.span, self.file);
                                        let elab_ty = self.check_kind(ty, ty_kind)?;

                                        self.subst_type(elab_ty)
                                    };

                                    match item_ty {
                                        | Ok(ty) => {
                                            self.module_types.items.insert(item.id.owner, crate::TypecheckResult {
                                                ty,
                                                tys: HashMap::new(),
                                                bounds: HashMap::new(),
                                            });
                                        },
                                        | Err(e) => e.report(self.db),
                                    }
                                },
                            }
                        }

                        for var in &head.vars {
                            let kind = self.subst_type(self.tys[&var.id].clone());

                            if let Type::Unknown(_) = &*kind {
                                TypeError::CantInferKind(self.file, var.span).report(self.db);
                            }
                        }

                        self.module_types.classes.insert(id, crate::ClassTypes {
                            var_kinds: head.vars.iter().map(|v| (TypeVar(v.id), self.subst_type(self.tys[&v.id].clone()))).collect(),
                        });

                        Ok(Ty::error(item.span, self.file))
                    },
                    | ir::ItemKind::Instance { head, body, .. } => {
                        try {
                            let _ = self.typeck_def(head.class);

                            if !self.db.has_errors() {
                                let class = if head.class.lib == self.db.lib() && head.class.module == self.module {
                                    self.module_types.classes[&head.class].clone()
                                } else {
                                    self.db.typeck_module(head.class.lib, head.class.module).classes[&head.class].clone()
                                };

                                assert_eq!(class.var_kinds.len(), head.tys.len());

                                let subst = head
                                    .tys
                                    .iter()
                                    .zip(class.var_kinds)
                                    .map(|(ty, (var, kind))| {
                                        let ty = self.hir_ty(ty);
                                        let elab_ty = self.check_kind(ty, kind)?;

                                        Ok((var, elab_ty))
                                    })
                                    .collect::<Result<HashMap<TypeVar, Ty>>>()?;

                                for item_ref in &body.items {
                                    let item = &hir.instance_items[&item_ref.id];
                                    let prev_tys = std::mem::replace(&mut self.tys, HashMap::new());
                                    let prev_bounds = std::mem::replace(&mut self.bounds, HashMap::new());
                                    let prev_subst = std::mem::replace(&mut self.subst, Substitution::empty());
                                    let res: Result<Ty> = match &item.kind {
                                        | ir::InstanceItemKind::Func { ty, body } => {
                                            try {
                                                let ty = self.hir_ty(ty);
                                                let class_item = if head.class.lib == self.db.lib() {
                                                    let file = self.db.module_tree(head.class.lib).file(head.class.module);
                                                    let hir = self.db.module_hir(file);
                                                    let class = hir.items[&head.class.into()].class_body();

                                                    class
                                                        .items
                                                        .iter()
                                                        .find(|it| it.name.symbol == item.name.symbol)
                                                        .map(|it| it.id.0.owner)
                                                        .unwrap()
                                                } else {
                                                    let external = self.db.external_item_data(head.class.lib, head.class.module);

                                                    external.classes[&head.class]
                                                        .iter()
                                                        .find(|it| it.0.symbol == item.name.symbol)
                                                        .map(|it| it.1.owner)
                                                        .unwrap()
                                                };

                                                self.tys.insert(item.id, ty.clone());

                                                let (ty, should_generalize) = if let Type::Unknown(_) = *ty {
                                                    let infer = self.infer_body(item.span, &hir.bodies[body])?;
                                                    let _ = self.unify_types(ty.clone(), infer.clone())?;

                                                    (infer, true)
                                                } else {
                                                    self.check_body(item.span, &hir.bodies[body], ty.clone())?;
                                                    (ty, false)
                                                };

                                                let expected = self.typeck_def(class_item);
                                                let expected = expected.replace_vars(subst.clone());
                                                let elaborate = self.subsumes(expected, ty.clone())?;
                                                let _ = elaborate(self, item.id, item.span);
                                                let unsolved = self.solve_ctnts(should_generalize)?;
                                                let ty = self.subst_type(ty);
                                                let ty = self.constrain(unsolved, ty);

                                                self.generalize(ty, item.id.owner)
                                            }
                                        },
                                    };

                                    self.finalize();
                                    self.subst = prev_subst;

                                    let tys = std::mem::replace(&mut self.tys, prev_tys);
                                    let bounds = std::mem::replace(&mut self.bounds, prev_bounds);

                                    match res {
                                        | Ok(ty) => {
                                            self.module_types.items.insert(item.id.owner, crate::TypecheckResult { ty, tys, bounds });
                                        },
                                        | Err(e) => e.report(self.db),
                                    }
                                }
                            }

                            Ty::error(item.span, self.file)
                        }
                    },
                    | ir::ItemKind::Fixity { .. } => Ok(Ty::error(item.span, self.file)),
                },
                | ir::Def::ClassItem(item) => {
                    self.typeck_def(item.owner.owner);

                    if let Some(ty) = self.module_types.items.get(&id) {
                        return ty.ty.clone();
                    } else {
                        return Ty::error(item.span, self.file);
                    }
                },
                | ir::Def::InstanceItem(item) => {
                    self.typeck_def(item.owner.owner);

                    if let Some(ty) = self.module_types.items.get(&id) {
                        return ty.ty.clone();
                    } else {
                        return Ty::error(item.span, self.file);
                    }
                },
            };

            self.subst = prev_subst;

            let tys = std::mem::replace(&mut self.tys, prev_tys);
            let bounds = std::mem::replace(&mut self.bounds, prev_bounds);

            match res {
                | Ok(ty) => {
                    self.module_types.items.insert(id, crate::TypecheckResult { ty: ty.clone(), tys, bounds });

                    ty
                },
                | Err(e) => {
                    e.report(self.db);
                    Ty::error(def.span(), self.file)
                },
            }
        }
    }
}

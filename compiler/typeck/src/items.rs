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
                    | ir::ItemKind::Alias { vars, value, kind: _ } => {
                        try {
                            let mut ty = self.hir_ty(value);

                            if !vars.is_empty() {
                                let vars = vars.iter().map(|v| (TypeVar(v.id), Some(self.ty_kind(v.span, self.file)))).collect::<Vec<_>>();

                                for (v, k) in vars.into_iter().rev() {
                                    ty = Ty::forall(item.span, self.file, v, k, ty, None);
                                }
                            }

                            ty
                        }
                    },
                    | ir::ItemKind::Data { head, .. } => {
                        if let ir::TypeKind::Infer = head.kind.kind {
                            let mut kind = self.ty_kind(item.span, self.file);

                            if !head.vars.is_empty() {
                                // let params = (0..head.vars.len()).map(|_| {
                                //     ctx.fresh_type_with_kind(item.span, file, ctx.ty_kind(item.span, file))
                                // });

                                // for now data types can only be paramaterized by types
                                let params = head.vars.iter().map(|v| self.ty_kind(v.span, self.file)).collect::<Vec<_>>();
                                let func_ty = self.func_ty(item.span, self.file);

                                for param in params.into_iter().rev() {
                                    kind = Ty::app(item.span, self.file, Ty::app(item.span, self.file, func_ty.clone(), param), kind);
                                }
                            }

                            Ok(kind)
                        } else {
                            let kind = self.hir_ty(&head.kind);

                            Ok(kind)
                        }
                    },
                    | ir::ItemKind::DataCtor { data, tys } => {
                        try {
                            let data = hir.items[data].data();
                            let mut ty = Ty::ctor(item.span, self.file, data.id.owner);

                            if !data.vars.is_empty() {
                                for var in &data.vars {
                                    ty = Ty::app(item.span, self.file, ty, Ty::var(var.span, self.file, TypeVar(var.id)));
                                }
                            }

                            if !tys.is_empty() {
                                let args = tys.iter().map(|t| self.hir_ty(t)).collect::<Vec<_>>();
                                let func_ty = self.func_ty(item.span, self.file);

                                for arg in args.into_iter().rev() {
                                    ty = Ty::app(item.span, self.file, Ty::app(item.span, self.file, func_ty.clone(), arg), ty);
                                }
                            }

                            if !data.vars.is_empty() {
                                let vars = data
                                    .vars
                                    .iter()
                                    .map(|v| (TypeVar(v.id), Some(self.ty_kind(v.span, self.file))))
                                    .collect::<Vec<_>>();

                                for (v, k) in vars.into_iter().rev() {
                                    ty = Ty::forall(item.span, self.file, v, k, ty, None);
                                }
                            }

                            let ty_kind = self.ty_kind(item.span, self.file);
                            let ty = self.check_kind(ty, ty_kind)?;

                            ty
                        }
                    },
                    | ir::ItemKind::Class { head, body } => {
                        for v in &head.vars {
                            let kind = self.fresh_kind(v.span, self.file);

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

                        self.module_types.classes.insert(id, crate::ClassTypes {
                            var_kinds: head.vars.iter().map(|v| (TypeVar(v.id), self.tys[&v.id].clone())).collect(),
                        });

                        Ok(Ty::error(item.span, self.file))
                    },
                    | ir::ItemKind::Instance { head, body, .. } => {
                        try {
                            let _ = self.typeck_def(head.class);
                            let class = if head.class.lib == self.db.lib() && head.class.module == self.module {
                                self.module_types.classes[&head.class].clone()
                            } else {
                                self.db.typeck_module(head.class.lib, head.class.module).classes[&head.class].clone()
                            };

                            for item_ref in &body.items {
                                let item = &hir.instance_items[&item_ref.id];
                                let prev_tys = std::mem::replace(&mut self.tys, HashMap::new());
                                let prev_bounds = std::mem::replace(&mut self.bounds, HashMap::new());

                                let res: Result<Ty> = match &item.kind {
                                    | ir::InstanceItemKind::Func { ty, body } => {
                                        try {
                                            let ty = self.hir_ty(ty);
                                            let imp = hir.items[&item.owner].instance();
                                            // @TODO
                                            let trait_file = self.db.module_tree(imp.class.lib).file(imp.class.module);
                                            let trait_hir = self.db.module_hir(trait_file);
                                            let trait_ = trait_hir.items[&imp.class.into()].class_body();
                                            let trait_item = trait_.items.iter().find(|it| it.name.symbol == item.name.symbol).unwrap();

                                            self.tys.insert(item.id, ty.clone());

                                            let (ty, should_generalize) = if let Type::Unknown(_) = *ty {
                                                let infer = self.infer_body(item.span, &hir.bodies[body])?;
                                                let _ = self.unify_types(ty.clone(), infer.clone())?;

                                                (infer, true)
                                            } else {
                                                self.check_body(item.span, &hir.bodies[body], ty.clone())?;
                                                (ty, false)
                                            };

                                            // let _unsolved = self.solve_ctnts(should_generalize)?;
                                            let expected = self.typeck_def(trait_item.id.0.owner);
                                            let expected = self.instantiate(item.id, expected) ^ (item.span, self.file);
                                            let _ = self.subsumes(expected, ty.clone())?;

                                            self.subst_type(ty)
                                        }
                                    },
                                };

                                self.finalize();

                                let tys = std::mem::replace(&mut self.tys, prev_tys);
                                let bounds = std::mem::replace(&mut self.bounds, prev_bounds);

                                match res {
                                    | Ok(ty) => {
                                        self.module_types.items.insert(item.id.owner, crate::TypecheckResult { ty, tys, bounds });
                                    },
                                    | Err(e) => e.report(self.db),
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

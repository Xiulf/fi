use super::{BodyInferenceContext, InferenceDiagnostic};
use crate::lower::LowerCtx;
use crate::ty::*;
use hir_def::expr::Literal;
use hir_def::pat::{Pat, PatId};
use hir_def::resolver::ValueNs;
use std::sync::Arc;

impl BodyInferenceContext<'_> {
    pub fn infer_pat(&mut self, pat: PatId) -> Ty {
        self.db.check_canceled();

        let body = Arc::clone(&self.body);
        let ty = match &body[pat] {
            | Pat::Missing => self.error(),
            | Pat::Wildcard => self.fresh_type(),
            | Pat::Typed { pat, ty } => self.owner.with_type_map(self.db.upcast(), |type_map| {
                let mut lcx = LowerCtx::new(type_map, self);
                let ty = lcx.lower_ty(*ty);

                self.check_pat(*pat, ty);
                ty
            }),
            | Pat::App { base, args } => {
                let ret = self.fresh_type();
                let ty = args.iter().rev().fold(ret, |ty, &arg| {
                    let arg = self.infer_pat(arg);

                    self.fn_type(arg, ty)
                });

                self.check_pat(*base, ty);
                ret
            },
            | Pat::Path { path } => match self.resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some((value, vis)) => {
                    if !vis.is_visible_from(self.db.upcast(), self.resolver.module().unwrap()) {
                        self.report(InferenceDiagnostic::PrivateValue { id: pat.into() });
                    }

                    match value {
                        | ValueNs::Ctor(id) => {
                            let ty = self.db.value_ty(id.into());

                            self.instantiate(ty, pat.into())
                        },
                        | ValueNs::Const(id) => {
                            let ty = self.db.value_ty(id.into());

                            self.instantiate(ty, pat.into())
                        },
                        | _ => {
                            todo!();
                        },
                    }
                },
                | None => {
                    self.report(InferenceDiagnostic::UnresolvedValue { id: pat.into() });
                    self.error()
                },
            },
            | Pat::Bind { subpat: None, .. } => self.fresh_type(),
            | Pat::Bind {
                subpat: Some(subpat), ..
            } => self.infer_pat(*subpat),
            | Pat::Tuple { pats } => {
                let tys = pats.iter().map(|&p| self.infer_pat(p)).collect();

                TyKind::Tuple(tys).intern(self.db)
            },
            | Pat::Record { fields, has_rest } => {
                let tail = if *has_rest {
                    let row_kind = self.lang_type("row-kind");
                    let type_kind = self.lang_type("type-kind");
                    let kind = TyKind::App(row_kind, type_kind).intern(self.db);

                    Some(self.fresh_type_with_kind(kind))
                } else {
                    None
                };

                let record_type = self.lang_type("record-type");
                let fields = fields
                    .iter()
                    .map(|f| Field {
                        name: f.name.clone(),
                        ty: self.infer_pat(f.val),
                    })
                    .collect();

                let row = TyKind::Row(fields, tail).intern(self.db);

                TyKind::App(record_type, row).intern(self.db)
            },
            | Pat::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let integer = self.lang_class("integer-class");
                    let ty = self.fresh_type();

                    self.constrain(pat.into(), Constraint {
                        class: integer,
                        types: vec![ty].into(),
                    });

                    ty
                },
                | Literal::Float(_) => {
                    let decimal = self.lang_class("decimal-class");
                    let ty = self.fresh_type();

                    self.constrain(pat.into(), Constraint {
                        class: decimal,
                        types: vec![ty].into(),
                    });

                    ty
                },
                | Literal::Char(_) => self.lang_type("char-type"),
                | Literal::String(_) => self.lang_type("str-type"),
            },
        };

        self.result.type_of_pat.insert(pat, ty);
        ty
    }

    pub fn check_pat(&mut self, pat: PatId, expected: Ty) {
        let body = Arc::clone(&self.body);

        self.result.type_of_pat.insert(pat, expected);

        match (&body[pat], expected.lookup(self.db)) {
            | (Pat::Wildcard, _) => {},
            | (Pat::Bind { subpat: None, .. }, _) => {},
            | (
                Pat::Bind {
                    subpat: Some(subpat), ..
                },
                _,
            ) => self.check_pat(*subpat, expected),
            | (_, _) => {
                let infer = self.infer_pat(pat);

                if !self.unify_types(infer, expected) {
                    self.report(InferenceDiagnostic::MismatchedType {
                        id: pat.into(),
                        expected,
                        found: infer,
                    });
                }
            },
        }
    }
}

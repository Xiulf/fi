use super::{BodyInferenceContext, InferenceDiagnostic};
use crate::info::{CtntInfo, FieldInfo, ToInfo, TyId, TyInfo};
use crate::lower::LowerCtx;
use hir_def::expr::Literal;
use hir_def::pat::{Pat, PatId};
use hir_def::resolver::ValueNs;
use std::sync::Arc;

impl BodyInferenceContext<'_> {
    pub fn infer_pat(&mut self, pat: PatId) -> TyId {
        self.db.check_canceled();

        let body = Arc::clone(&self.body);
        let src = self.source(pat);
        let ty = match &body[pat] {
            | Pat::Missing => self.error(src),
            | Pat::Wildcard => self.fresh_type(src),
            | Pat::Typed { pat, ty } => self.owner.with_type_map(self.db.upcast(), |type_map| {
                let mut lcx = LowerCtx::new(type_map, self);
                let ty = lcx.lower_ty(*ty);

                self.check_pat(*pat, ty);
                ty
            }),
            | Pat::App { base, args } => {
                let ret = self.fresh_type(src);
                let args = args.iter().map(|&a| self.infer_pat(a)).collect();
                let base_src = self.source(*base);
                let ty = self.fn_type(args, ret, base_src);

                self.check_pat(*base, ty);
                ret
            },
            | Pat::Path { path } => match self.resolver.resolve_value_fully(self.db.upcast(), path) {
                | Some((value, vis)) => {
                    if path.segments().len() > 1
                        && !vis.is_visible_from(self.db.upcast(), self.resolver.module().unwrap())
                    {
                        self.report(InferenceDiagnostic::PrivateValue { id: pat.into() });
                    }

                    let ty = match value {
                        | ValueNs::Ctor(id) => self.db.value_ty(id.into()),
                        | ValueNs::Const(id) => self.db.value_ty(id.into()),
                        | _ => todo!(),
                    };

                    let ty = ty.to_info(self.icx.db, &mut self.icx.types, &mut self.icx.type_vars, src);

                    self.instantiate(ty, pat.into())
                },
                | None => {
                    self.report(InferenceDiagnostic::UnresolvedValue { id: pat.into() });
                    self.error(src)
                },
            },
            | Pat::Bind { subpat: None, .. } => self.fresh_type(src),
            | Pat::Bind {
                subpat: Some(subpat), ..
            } => self.infer_pat(*subpat),
            | Pat::Tuple { pats } => {
                let tys = pats.iter().map(|&p| self.infer_pat(p)).collect();

                self.types.insert(TyInfo::Tuple(tys), src)
            },
            | Pat::Record { fields, has_rest } => {
                let tail = if *has_rest {
                    let row_kind = self.lang_type("row-kind", src);
                    let type_kind = self.lang_type("type-kind", src);
                    let kind = self.types.insert(TyInfo::App(row_kind, [type_kind].into()), src);

                    Some(self.fresh_type_with_kind(kind, src))
                } else {
                    None
                };

                let record_type = self.lang_type("record-type", src);
                let fields = fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.clone(),
                        ty: self.infer_pat(f.val),
                    })
                    .collect();

                let row = self.types.insert(TyInfo::Row(fields, tail), src);

                self.types.insert(TyInfo::App(record_type, [row].into()), src)
            },
            | Pat::Lit { lit } => match lit {
                | Literal::Int(_) => {
                    let integer = self.lang_class("integer-class");
                    let ty = self.fresh_type(src);

                    self.constrain(pat.into(), CtntInfo {
                        class: integer,
                        types: vec![ty].into(),
                    });

                    ty
                },
                | Literal::Float(_) => {
                    let decimal = self.lang_class("decimal-class");
                    let ty = self.fresh_type(src);

                    self.constrain(pat.into(), CtntInfo {
                        class: decimal,
                        types: vec![ty].into(),
                    });

                    ty
                },
                | Literal::Char(_) => self.lang_type("char-type", src),
                | Literal::String(_) => self.lang_type("str-type", src),
            },
        };

        self.result.type_of_pat.insert(pat, ty);
        ty
    }

    pub fn check_pat(&mut self, pat: PatId, expected: TyId) {
        let body = Arc::clone(&self.body);

        self.result.type_of_pat.insert(pat, expected);

        match (&body[pat], self.types[expected].clone()) {
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
                    self.report_mismatch(expected, infer, pat);
                }
            },
        }
    }
}

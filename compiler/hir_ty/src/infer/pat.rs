use super::BodyInferenceContext;
use crate::ty::*;
use hir_def::pat::{Pat, PatId};
use std::sync::Arc;

impl BodyInferenceContext<'_> {
    pub fn infer_pat(&mut self, pat: PatId) -> Ty {
        let body = Arc::clone(&self.body);
        let ty = match &body[pat] {
            | p => unimplemented!("{:?}", p),
        };

        self.result.type_of_pat.insert(pat, ty);
        ty
    }

    pub fn check_pat(&mut self, pat: PatId, expected: Ty) {
    }
}

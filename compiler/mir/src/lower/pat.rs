use hir::Pat;

use super::*;

impl BodyLowerCtx<'_> {
    pub fn define_pat(&mut self, pat: hir::PatId, place: Place) {
        let body = self.body.clone();

        match body[pat] {
            | Pat::Missing => unreachable!(),
            | Pat::Wildcard => {},
            | Pat::Bind { subpat: None, .. } => {
                self.locals.insert(pat, place);
            },
            | ref p => todo!("{:?}", p),
        }
    }

    pub fn lower_pat(&mut self, pat: hir::PatId, place: Place) -> Option<Operand> {
        let body = self.body.clone();

        match body[pat] {
            | Pat::Missing => unreachable!(),
            | Pat::Wildcard => None,
            | Pat::Bind { subpat: None, .. } => {
                self.locals.insert(pat, place);
                None
            },
            | ref p => todo!("{:?}", p),
        }
    }
}

use super::*;

struct Candidate<'hir, 'tcx> {
    place: Place<'tcx>,
    pat: &'hir hir::Pat,
    otherwise: BlockId,
}

impl<'a, 'tcx> BodyConverter<'a, 'tcx> {
    pub(super) fn trans_match(
        &mut self,
        id: &hir::Id,
        pred: &hir::Id,
        arms: &'a [hir::MatchArm],
    ) -> Operand<'tcx> {
        let res = self.builder.create_tmp(self.tcx.type_of(id));
        let res = Place::local(res);
        let (pred, discr) = self.trans_pred(pred);
        let exit_block = self.builder.create_block();
        let candidates = self.build_candidates(pred, exit_block, arms);

        self.trans_candidates(candidates, discr, exit_block, res.clone());
        self.builder.use_block(exit_block);

        Operand::Move(res)
    }

    fn trans_pred(&mut self, pred: &hir::Id) -> (Place<'tcx>, Place<'tcx>) {
        let pred_ty = self.tcx.type_of(pred);
        let pred = self.trans_expr(pred, None);
        let pred = self.builder.placed(pred, pred_ty);

        if let Type::Enum(..) = pred_ty {
            let discr = self.builder.create_tmp(self.tcx.builtin.usize);
            let discr = Place::local(discr);

            self.builder.discriminant(discr.clone(), pred.clone());

            (pred, discr)
        } else {
            (pred.clone(), pred)
        }
    }

    fn build_candidates(
        &self,
        pred: Place<'tcx>,
        exit_block: BlockId,
        arms: &'a [hir::MatchArm],
    ) -> Vec<(Candidate<'a, 'tcx>, &'a hir::MatchArm)> {
        arms.iter()
            .map(|arm| {
                let candidate = Candidate::new(pred.clone(), exit_block, &self.hir.pats[&arm.pat]);

                (candidate, arm)
            })
            .collect()
    }

    fn trans_candidates(
        &mut self,
        candidates: Vec<(Candidate<'a, 'tcx>, &'a hir::MatchArm)>,
        pred: Place<'tcx>,
        exit_block: BlockId,
        res: Place<'tcx>,
    ) {
        let mut values = Vec::new();
        let mut blocks = Vec::new();
        let block = self.builder.current_block.unwrap();

        for (candidate, arm) in candidates {
            if self.trans_candidate(
                candidate,
                arm,
                exit_block,
                res.clone(),
                true,
                &mut values,
                &mut blocks,
            ) {
                break;
            }
        }

        blocks.push(exit_block);

        self.builder.use_block(block);
        self.builder.switch(Operand::Move(pred), values, blocks);
    }

    fn trans_candidate(
        &mut self,
        candidate: Candidate<'a, 'tcx>,
        arm: &'a hir::MatchArm,
        exit_block: BlockId,
        res: Place<'tcx>,
        eval_arm: bool,
        values: &mut Vec<u128>,
        blocks: &mut Vec<BlockId>,
    ) -> bool {
        match &candidate.pat.kind {
            hir::PatKind::Err => return true,
            hir::PatKind::Wildcard => return true,
            hir::PatKind::Bind { var, inner, by_ref } => {
                let local = self.builder.create_var(self.tcx.type_of(var));

                if *by_ref {
                    self.builder
                        .ref_(Place::local(local), candidate.place.clone());
                } else {
                    self.builder
                        .use_(Place::local(local), Operand::Copy(candidate.place.clone()));
                }

                self.locals.insert(*var, local);

                if let Some(inner) = inner {
                    let c =
                        Candidate::new(candidate.place, candidate.otherwise, &self.hir.pats[inner]);

                    return self.trans_candidate(c, arm, exit_block, res, eval_arm, values, blocks);
                }
            }
            hir::PatKind::Ctor { id, pats } => {
                if let hir::ItemKind::Ctor { variant, .. } = &self.hir.items[id].kind {
                    let next = self.builder.create_block();

                    values.push(*variant as u128);
                    blocks.push(next);
                    self.builder.use_block(next);

                    for (i, pat) in pats.iter().enumerate() {
                        let c = Candidate::new(
                            candidate.place.clone().as_variant(*variant).field(i),
                            candidate.otherwise,
                            &self.hir.pats[pat],
                        );

                        self.trans_candidate(
                            c,
                            arm,
                            exit_block,
                            res.clone(),
                            if i < pats.len() - 1 { false } else { eval_arm },
                            &mut Vec::new(),
                            &mut Vec::new(),
                        );
                    }
                }
            }
        }

        if eval_arm {
            let val = self.trans_expr(&arm.value, None);

            self.builder.use_(res, val);
            self.builder.jump(exit_block);
        }

        false
    }
}

impl<'hir, 'tcx> Candidate<'hir, 'tcx> {
    fn new(place: Place<'tcx>, otherwise: BlockId, pat: &'hir hir::Pat) -> Self {
        Candidate {
            place,
            otherwise,
            pat,
        }
    }
}

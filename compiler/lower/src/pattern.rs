use super::*;

#[derive(Debug)]
pub struct Case<'hir> {
    arms: Vec<Arm<'hir>>,
}

#[derive(Debug)]
pub struct Arm<'hir> {
    pat: Pattern,
    guard: &'hir hir::Guarded,
}

#[derive(Debug)]
pub enum Pattern {
    Seq(Vec<Pattern>),
    Bind(ir::Local, ir::Place),
    Switch(ir::Operand, u128),
    And(Vec<Pattern>),
    Or(Vec<Pattern>),
}

impl Arm<'_> {
    pub fn matches_all(&self) -> bool {
        !self.pat.checked()
    }
}

impl Pattern {
    pub fn checked(&self) -> bool {
        match self {
            Pattern::Switch(..) => true,
            Pattern::And(_) => true,
            Pattern::Or(_) => true,
            Pattern::Seq(ps) => ps.iter().any(Pattern::checked),
            Pattern::Bind(..) => false,
        }
    }
}

impl<'db, 'c> BodyConverter<'db, 'c> {
    pub(crate) fn compile_case(&mut self, case: Case, ty: ir::Type) -> ir::Operand {
        if !case.arms.is_empty() && case.arms[0].matches_all() {
            self.compile_guarded(case.arms[0].guard, None)
        } else {
            let res = self.builder.create_tmp(ty);
            let res = ir::Place::new(res);
            let exit_block = self.builder.create_block();
            let last = case.arms.len() - 1;

            for (i, arm) in case.arms.into_iter().enumerate() {
                if arm.matches_all() || i == last {
                    let block = self.compile_pattern(arm.pat, exit_block);

                    self.builder.set_block(block);
                    self.compile_guarded(arm.guard, Some(res.clone()));
                    self.builder.jump(exit_block);
                    break;
                } else {
                    let next = self.builder.create_block();
                    let block = self.compile_pattern(arm.pat, next);

                    self.builder.set_block(block);
                    self.compile_guarded(arm.guard, Some(res.clone()));
                    self.builder.jump(exit_block);
                    self.builder.set_block(next);
                }
            }

            self.builder.set_block(exit_block);

            ir::Operand::Place(res)
        }
    }

    fn compile_pattern(&mut self, pat: Pattern, next_block: ir::Block) -> ir::Block {
        match pat {
            Pattern::Bind(local, place) => {
                self.builder
                    .use_op(ir::Place::new(local), ir::Operand::Place(place));
                self.builder.get_block()
            }
            Pattern::Switch(op, val) => {
                let block = self.builder.create_block();
                let _ = self.builder.switch(op, vec![val], vec![block, next_block]);

                block
            }
            Pattern::Seq(mut pats) => {
                if !pats.is_empty() && pats[0].checked() {
                    let block = self.compile_pattern(pats.remove(0), next_block);

                    self.builder.set_block(block);

                    for pat in pats {
                        self.compile_pattern(pat, next_block);
                    }

                    block
                } else {
                    for pat in pats {
                        self.compile_pattern(pat, next_block);
                    }

                    self.builder.get_block()
                }
            }
            Pattern::And(pats) => {
                let mut block = self.builder.get_block();

                for pat in pats {
                    block = self.compile_pattern(pat, next_block);
                    self.builder.set_block(block);
                }

                block
            }
            _ => unimplemented!(),
        }
    }

    fn compile_guarded(&mut self, guard: &hir::Guarded, place: Option<ir::Place>) -> ir::Operand {
        match guard {
            hir::Guarded::Unconditional(expr) => {
                if let Some(place) = place {
                    let op = self.convert_expr(expr);

                    self.builder.use_op(place.clone(), op);
                    ir::Operand::Place(place)
                } else {
                    self.convert_expr(expr)
                }
            }
            hir::Guarded::Guarded(_) => unimplemented!(),
        }
    }

    pub(crate) fn convert_arms<'hir>(
        &mut self,
        preds: Vec<ir::Place>,
        arms: &'hir [hir::CaseArm],
    ) -> Case<'hir> {
        Case {
            arms: arms
                .iter()
                .map(|arm| Arm {
                    pat: self.convert_pats(&preds, &arm.pats),
                    guard: &arm.val,
                })
                .collect(),
        }
    }

    fn convert_pats(&mut self, preds: &[ir::Place], pats: &[hir::Pat]) -> Pattern {
        let mut pats = pats
            .iter()
            .zip(preds.to_owned())
            .filter_map(|(pat, pred)| self.convert_pat(pat, pred))
            .collect::<Vec<_>>();

        let _ = pats.sort_by_key(Pattern::checked);
        let i = pats.partition_point(Pattern::checked);
        let mut rest = pats.split_off(i);

        if rest.is_empty() && pats.len() == 1 {
            pats.pop().unwrap()
        } else if pats.is_empty() {
            Pattern::Seq(rest)
        } else if rest.is_empty() {
            Pattern::And(pats)
        } else {
            rest.insert(0, Pattern::And(pats));
            Pattern::Seq(rest)
        }
    }

    pub(crate) fn convert_pat(&mut self, pat: &hir::Pat, pred: ir::Place) -> Option<Pattern> {
        let ty = lower_type(self.db, &self.types.tys[&pat.id]);

        match &pat.kind {
            hir::PatKind::Wildcard => None,
            hir::PatKind::Bind { sub: None, .. } => {
                if pred.elems.is_empty() && self.builder.local_ty(pred.local) == ty {
                    self.locals.insert(pat.id, pred.local);

                    None
                } else {
                    let local = self.builder.create_var(ty);

                    self.locals.insert(pat.id, local);

                    Some(Pattern::Bind(local, pred))
                }
            }
            hir::PatKind::Int { val } => Some(Pattern::Switch(ir::Operand::Place(pred), *val)),
            _ => unimplemented!(),
        }
    }
}

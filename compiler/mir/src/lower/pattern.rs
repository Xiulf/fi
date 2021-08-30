use super::*;
use hir_def::resolver::HasResolver;

#[derive(Debug)]
pub struct Case {
    pub arms: Vec<Arm>,
}

#[derive(Debug)]
pub struct Arm {
    pub pat: Option<Pattern>,
    pub guard: Option<hir::ExprId>,
    pub expr: hir::ExprId,
}

#[derive(Debug)]
pub enum Pattern {
    Check(Operand, CheckVal),
    And(Vec<Pattern>),
    Or(Vec<Pattern>),
}

#[derive(Debug)]
pub enum CheckVal {
    Scalar(u128),
    String(String),
}

impl BodyLowerCtx<'_> {
    pub fn convert_arms(&mut self, preds: Vec<Place>, arms: &[hir::CaseArm]) -> Case {
        let mut discrs = FxHashMap::default();

        Case {
            arms: arms
                .iter()
                .map(|arm| Arm {
                    pat: self.convert_pats(&preds, &[arm.pat], &mut discrs),
                    guard: arm.guard,
                    expr: arm.expr,
                })
                .collect(),
        }
    }

    fn convert_pats(
        &mut self,
        preds: &[Place],
        pats: &[hir::PatId],
        discrs: &mut FxHashMap<Place, Place>,
    ) -> Option<Pattern> {
        let mut pats = pats
            .iter()
            .copied()
            .zip(preds.iter().cloned())
            .filter_map(|(pat, pred)| self.convert_pat(pat, pred, discrs))
            .collect::<Vec<_>>();

        match pats.len() {
            | 0 => None,
            | 1 => pats.pop(),
            | _ => Some(Pattern::And(pats)),
        }
    }

    pub fn convert_pat(
        &mut self,
        pat: hir::PatId,
        pred: Place,
        discrs: &mut FxHashMap<Place, Place>,
    ) -> Option<Pattern> {
        match &self.hir[pat] {
            | hir::Pat::Typed { pat, .. } => self.convert_pat(*pat, pred, discrs),
            | hir::Pat::Missing => None,
            | hir::Pat::Wildcard => None,
            | hir::Pat::Bind { subpat, .. } => {
                self.binders.insert(pat, pred.clone());
                subpat.and_then(|s| self.convert_pat(s, pred, discrs))
            },
            | hir::Pat::Tuple { pats } => {
                let preds = (0..pats.len()).map(|i| pred.clone().field(i)).collect::<Vec<_>>();

                self.convert_pats(&preds, pats, discrs)
            },
            | hir::Pat::App { base, args } => {
                if let hir::Pat::Path { path } = &self.hir[*base] {
                    let resolver = self.def.resolver(self.db.upcast());

                    match resolver.resolve_value_fully(self.db.upcast(), path) {
                        | Some((ValueNs::Ctor(id), _)) => self.convert_ctor_pat(id, args, pred, discrs),
                        | _ => unreachable!(),
                    }
                } else {
                    unreachable!();
                }
            },
            | hir::Pat::Path { path } => {
                let resolver = self.def.resolver(self.db.upcast());

                match resolver.resolve_value_fully(self.db.upcast(), path) {
                    | Some((ValueNs::Ctor(id), _)) => self.convert_ctor_pat(id, &[], pred, discrs),
                    | _ => unimplemented!(),
                }
            },
            | p => unimplemented!("{:?}", p),
        }
    }

    fn convert_ctor_pat(
        &mut self,
        id: hir::id::CtorId,
        args: &[hir::PatId],
        pred: Place,
        discrs: &mut FxHashMap<Place, Place>,
    ) -> Option<Pattern> {
        let data = self.db.type_ctor_data(id.parent);

        if data.ctors.len() == 1 {
            let preds = (0..args.len()).map(|i| pred.clone().field(i)).collect::<Vec<_>>();

            self.convert_pats(&preds, args, discrs)
        } else {
            let idx: u32 = id.local_id.into_raw().into();
            let discr = discrs
                .entry(pred.clone())
                .or_insert_with(|| {
                    let discr_ty = Type::discriminant(self.db, self.builder.place_type(&pred));
                    let discr = self.builder.create_var(discr_ty);
                    let discr = Place::new(discr);

                    self.builder.get_discr(discr.clone(), pred.clone());
                    discr
                })
                .clone();

            let discr = Pattern::Check(Operand::Place(discr), CheckVal::Scalar(idx as u128));
            let pred = pred.downcast(idx as usize);
            let preds = (0..args.len()).map(|i| pred.clone().field(i)).collect::<Vec<_>>();

            if let Some(mut pats) = self.convert_pats(&preds, args, discrs) {
                if let Pattern::And(pats) = &mut pats {
                    pats.insert(0, discr);
                }

                Some(pats)
            } else {
                Some(discr)
            }
        }
    }

    pub fn lower_case(&mut self, case: Case, ty: Arc<Type>, ret: Option<Place>) -> Operand {
        let ret = ret.unwrap_or_else(|| Place::new(self.builder.create_var(ty)));
        let exit_block = self.builder.create_block();
        let last = case.arms.len() - 1;

        for (i, arm) in case.arms.into_iter().enumerate() {
            if let Some(pat) = arm.pat {
                let succ = self.builder.create_block();
                let fail = if i == last {
                    exit_block
                } else {
                    self.builder.create_block()
                };

                self.lower_pattern(pat, arm.guard, succ, fail);
                self.builder.set_block(succ);
                self.lower_expr(arm.expr, Some(ret.clone()));
                self.builder.jump(exit_block);
                self.builder.set_block(fail);
            } else {
                self.lower_expr(arm.expr, Some(ret.clone()));
                self.builder.jump(exit_block);
                self.builder.set_block(exit_block);
                break;
            }
        }

        Operand::Place(ret)
    }

    fn lower_pattern(&mut self, pat: Pattern, guard: Option<hir::ExprId>, succ: BlockId, fail: BlockId) {
        match pat {
            | Pattern::Check(op, val) => match val {
                | CheckVal::Scalar(s) => {
                    self.builder.switch(op, vec![s], vec![succ, fail]);
                },
                | CheckVal::String(_) => unimplemented!(),
            },
            | _ => unimplemented!(),
        }
    }
}

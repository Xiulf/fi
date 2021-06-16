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
    Check(Operand, Const),
    And(Vec<Pattern>),
    Or(Vec<Pattern>),
}

impl BodyLowerCtx<'_> {
    pub fn convert_arms(&mut self, preds: Vec<Place>, arms: &[hir::CaseArm]) -> Case {
        Case {
            arms: arms
                .iter()
                .map(|arm| Arm {
                    pat: self.convert_pats(&preds, &[arm.pat]),
                    guard: arm.guard,
                    expr: arm.expr,
                })
                .collect(),
        }
    }

    fn convert_pats(&mut self, preds: &[Place], pats: &[hir::PatId]) -> Option<Pattern> {
        let mut pats = pats
            .iter()
            .copied()
            .zip(preds.iter().cloned())
            .filter_map(|(pat, pred)| self.convert_pat(pat, pred))
            .collect::<Vec<_>>();

        match pats.len() {
            | 0 => None,
            | 1 => pats.pop(),
            | _ => Some(Pattern::And(pats)),
        }
    }

    pub fn convert_pat(&mut self, pat: hir::PatId, pred: Place) -> Option<Pattern> {
        match &self.hir[pat] {
            | hir::Pat::Typed { pat, .. } => self.convert_pat(*pat, pred),
            | hir::Pat::Missing => None,
            | hir::Pat::Wildcard => None,
            | hir::Pat::Bind { subpat, .. } => {
                self.binders.insert(pat, pred.clone());
                subpat.and_then(|s| self.convert_pat(s, pred))
            },
            | hir::Pat::Tuple { pats } => {
                let preds = (0..pats.len()).map(|i| pred.clone().field(i)).collect::<Vec<_>>();

                self.convert_pats(&preds, pats)
            },
            | hir::Pat::App { base, args } => {
                if let hir::Pat::Path { path } = &self.hir[*base] {
                    let resolver = self.def.resolver(self.db.upcast());

                    match resolver.resolve_value_fully(self.db.upcast(), path) {
                        | Some(ValueNs::Ctor(id)) => self.convert_ctor_pat(id, args, pred),
                        | _ => unreachable!(),
                    }
                } else {
                    unreachable!();
                }
            },
            | hir::Pat::Path { path } => {
                let resolver = self.def.resolver(self.db.upcast());

                match resolver.resolve_value_fully(self.db.upcast(), path) {
                    | Some(ValueNs::Ctor(id)) => self.convert_ctor_pat(id, &[], pred),
                    | _ => unimplemented!(),
                }
            },
            | p => unimplemented!("{:?}", p),
        }
    }

    fn convert_ctor_pat(&mut self, id: hir::id::CtorId, args: &[hir::PatId], pred: Place) -> Option<Pattern> {
        let data = self.db.type_ctor_data(id.parent);
        let ctor = &data.ctors[id.local_id];

        if data.ctors.len() == 1 {
            let preds = (0..args.len()).map(|i| pred.clone().field(i)).collect::<Vec<_>>();

            self.convert_pats(&preds, args)
        } else {
            let idx = data.ctors.iter().position(|(i, _)| i == id.local_id).unwrap();
            let discr_ty = Type::discriminant(self.db, self.builder.place_type(&pred));
            let discr = self.builder.create_var(discr_ty);
            let discr = Place::new(discr);

            self.builder.get_discr(discr.clone(), pred.clone());

            let discr = Pattern::Check(Operand::Place(discr), Const::Scalar(idx as u128));
            let pred = pred.downcast(idx);
            let preds = (0..args.len()).map(|i| pred.clone().field(i)).collect::<Vec<_>>();

            if let Some(mut pats) = self.convert_pats(&preds, args) {
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

        for arm in case.arms {
            if let Some(pat) = arm.pat {
            } else {
                break;
            }
        }

        Operand::Place(ret)
    }
}

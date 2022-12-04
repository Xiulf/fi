use hir::{CaseValue, HasResolver, Literal, Pat, Resolver, ValueNs};

use super::*;

#[derive(Debug, Default)]
pub struct PatternMatch {
    pub arms: Vec<MatchArm>,
    pub fallback: Option<hir::ExprId>,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub value: MatchValue,
}

#[derive(Debug)]
pub enum MatchValue {
    Expr(hir::ExprId),
    Nested(PatternMatch),
}

#[derive(Debug)]
pub enum Pattern {
    Switch(Place, i128),
    Check(hir::ExprId),
    And(Box<Pattern>, Box<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
}

pub(super) enum Arg<'a> {
    PatId(hir::PatId),
    Fn(Box<dyn FnOnce(&mut BodyLowerCtx, Place) -> Option<Pattern> + 'a>),
}

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

    pub fn lower_case(&mut self, expr: hir::ExprId, discr: hir::ExprId, arms: &[hir::CaseArm]) -> Operand {
        let res = self.builder.add_local(LocalKind::Tmp, self.infer.type_of_expr[expr]);
        let discr_ty = self.infer.type_of_expr[discr];
        let discr = self.lower_expr(discr);
        let discr = self.place_op(discr, discr_ty);
        let pmatch = self.compile_case(arms, discr);
        let exit_block = self.builder.create_block();

        self.lower_pattern_match(&pmatch, exit_block);
        self.builder.add_block_param(exit_block, res);
        self.builder.switch_block(exit_block);

        Operand::Move(Place::new(res))
    }

    fn lower_pattern_match(&mut self, pmatch: &PatternMatch, exit_block: Block) {
        for arm in pmatch.arms.group_by(|a, b| match (&a.pattern, &b.pattern) {
            | (Pattern::Switch(a, _), Pattern::Switch(b, _)) => a == b,
            | _ => false,
        }) {
            if arm.len() > 1 {
                let mut values = arm
                    .iter()
                    .map(|a| match &a.pattern {
                        | Pattern::Switch(p, v) => (p, *v, &a.value),
                        | _ => unreachable!(),
                    })
                    .collect::<Vec<_>>();

                values.sort_by_key(|v| v.1);

                let discr = values[0].0.clone();
                let current_block = self.builder.current_block();
                let mut blocks = values
                    .iter()
                    .map(|(_, _, value)| {
                        let block = self.builder.create_block();
                        self.builder.switch_block(block);
                        self.lower_match_value(value, exit_block);
                        JumpTarget::from(block)
                    })
                    .collect::<Vec<_>>();

                let values = values.into_iter().map(|v| v.1).collect();
                let next_block = self.builder.create_block();
                blocks.push(next_block.into());
                self.builder.switch_block(current_block);
                self.builder.switch(Operand::Copy(discr), values, blocks);
                self.builder.switch_block(next_block);
            } else {
                let then_block = self.builder.create_block();
                let next_block = self.builder.create_block();

                match arm[0].pattern {
                    | Pattern::Switch(ref discr, value) => {
                        self.builder.switch(Operand::Copy(discr.clone()), vec![value], [
                            then_block.into(),
                            next_block.into(),
                        ]);
                    },
                    | Pattern::Check(expr) => {
                        let op = self.lower_expr(expr);
                        self.builder.switch(op, vec![0], [next_block.into(), then_block.into()]);
                    },
                    | Pattern::And(ref _a, ref _b) => todo!(),
                    | Pattern::Or(ref _a, ref _b) => todo!(),
                }

                self.builder.switch_block(then_block);
                self.lower_match_value(&arm[0].value, exit_block);
                self.builder.switch_block(next_block);
            }
        }

        if let Some(expr) = pmatch.fallback {
            let op = self.lower_expr(expr);
            self.builder.jump((exit_block, [op]));
        }
    }

    fn lower_match_value(&mut self, value: &MatchValue, exit_block: Block) {
        match *value {
            | MatchValue::Expr(expr) => {
                let op = self.lower_expr(expr);
                self.builder.jump((exit_block, [op]));
            },
            | MatchValue::Nested(ref inner) => self.lower_pattern_match(inner, exit_block),
        }
    }

    pub fn compile_case(&mut self, arms: &[hir::CaseArm], place: Place) -> PatternMatch {
        let mut pmatch = PatternMatch::default();

        for arm in arms {
            let pattern = self.compile_pat(arm.pat, place.clone());

            match (&arm.value, pattern) {
                | (&CaseValue::Normal(expr), None) => {
                    pmatch.fallback = Some(expr);
                    break;
                },
                | (&CaseValue::Normal(expr), Some(pattern)) => {
                    pmatch.arms.push(MatchArm {
                        pattern,
                        value: MatchValue::Expr(expr),
                    });
                },
                | (CaseValue::Guarded(guards, exprs), None) => {
                    let mut exprs = exprs.iter().copied();

                    for (&guard, expr) in guards.iter().zip(exprs.by_ref()) {
                        pmatch.arms.push(MatchArm {
                            pattern: Pattern::Check(guard),
                            value: MatchValue::Expr(expr),
                        });
                    }

                    pmatch.fallback = exprs.next();
                    break;
                },
                | (CaseValue::Guarded(guards, exprs), Some(pattern)) => {
                    let mut exprs = exprs.iter().copied();
                    let mut inner = PatternMatch::default();

                    for (&guard, expr) in guards.iter().zip(exprs.by_ref()) {
                        inner.arms.push(MatchArm {
                            pattern: Pattern::Check(guard),
                            value: MatchValue::Expr(expr),
                        });
                    }

                    inner.fallback = exprs.next();
                    pmatch.arms.push(MatchArm {
                        pattern,
                        value: MatchValue::Nested(inner),
                    });
                },
            }
        }

        pmatch
    }

    pub fn compile_pat(&mut self, pat: hir::PatId, place: Place) -> Option<Pattern> {
        let body = self.body.clone();

        match body[pat] {
            | Pat::Missing => unreachable!(),
            | Pat::Wildcard => None,
            | Pat::Bind { subpat, .. } => {
                self.locals.insert(pat, place.clone());
                subpat.and_then(|s| self.compile_pat(s, place))
            },
            | Pat::Lit { ref lit } => match *lit {
                | Literal::Int(v) => Some(Pattern::Switch(place.clone(), v)),
                | _ => todo!(),
            },
            | Pat::Path { ref path } => {
                let resolver = self.builder.origin().def.resolver(self.db.upcast());
                let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();

                match resolved {
                    | ValueNs::Ctor(id) => {
                        let ctor = hir::Ctor::from(id);
                        let ctors = ctor.type_ctor().ctors(self.db.upcast());
                        let index = ctors.iter().position(|&c| c == ctor).unwrap();

                        Some(Pattern::Switch(place.clone(), index as i128))
                    },
                    | _ => unreachable!(),
                }
            },
            | Pat::App { base, ref args } => match body[base] {
                | Pat::Path { ref path } => {
                    let resolver = self.builder.origin().def.resolver(self.db.upcast());
                    let args = args.iter().copied().map(Arg::PatId).collect();

                    self.compile_pat_app(&resolver, path, args, place)
                },
                | _ => unreachable!(),
            },
            | ref p => todo!("{:?}", p),
        }
    }

    fn compile_pat_app(
        &mut self,
        resolver: &Resolver,
        path: &hir::Path,
        args: Vec<Arg>,
        place: Place,
    ) -> Option<Pattern> {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();

        match resolved {
            | ValueNs::Ctor(id) => {
                let ctor = hir::Ctor::from(id);
                let ctors = ctor.type_ctor().ctors(self.db.upcast());
                let index = ctors.iter().position(|&c| c == ctor).unwrap();
                let first = Pattern::Switch(place.clone(), index as i128);
                let place = place.downcast(ctor);
                let args = args.into_iter().enumerate().filter_map(|(i, arg)| {
                    let place = place.clone().field(i);
                    self.compile_arg(arg, place)
                });

                Some(args.fold(first, |lhs, rhs| Pattern::And(Box::new(lhs), Box::new(rhs))))
            },
            | _ => unreachable!(),
        }
    }

    fn compile_arg(&mut self, arg: Arg, place: Place) -> Option<Pattern> {
        match arg {
            | Arg::PatId(pat) => self.compile_pat(pat, place),
            | Arg::Fn(f) => f(self, place),
        }
    }
}

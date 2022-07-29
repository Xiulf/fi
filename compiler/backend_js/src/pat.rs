use hir::{HasResolver, Pat, Resolver, ValueNs};

use crate::expr::JsExpr;
use crate::BodyCtx;

pub(crate) enum Arg<'a> {
    Pat(hir::PatId),
    Fn(Box<dyn FnOnce(&mut BodyCtx, JsExpr, &mut Vec<JsExpr>) -> Option<JsExpr> + 'a>),
}

impl BodyCtx<'_, '_> {
    pub fn pat_is_ignored(&self, pat: hir::PatId) -> bool {
        match self.body[pat] {
            | Pat::Missing | Pat::Wildcard => true,
            | Pat::Typed { pat, .. } => self.pat_is_ignored(pat),
            | Pat::App { base, ref args } => self.pat_is_ignored(base) && args.iter().all(|&p| self.pat_is_ignored(p)),
            | Pat::Infix { ref pats, .. } => pats.iter().all(|&p| self.pat_is_ignored(p)),
            | Pat::Record { ref fields, .. } => fields.iter().all(|f| self.pat_is_ignored(f.val)),
            | _ => false,
        }
    }

    pub fn lower_pat(&mut self, pat: hir::PatId, place: JsExpr, exprs: &mut Vec<JsExpr>) -> Option<JsExpr> {
        let body = self.body.clone();

        match body[pat] {
            | Pat::Missing => None,
            | Pat::Wildcard => None,
            | Pat::Unit => None,
            | Pat::Typed { pat, .. } => self.lower_pat(pat, place, exprs),
            | Pat::Bind { subpat, .. } => {
                self.locals.insert(pat, place.clone());

                if let Some(subpat) = subpat {
                    return self.lower_pat(subpat, place, exprs);
                }

                None
            },
            | Pat::Path { ref path } => {
                let resolver = self.owner.resolver(self.db.upcast());
                let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();

                match resolved {
                    | ValueNs::Ctor(id) => {
                        let (places, check) = self.deconstruct(id.into(), place);
                        assert!(places.is_empty());

                        check
                    },
                    | _ => unreachable!(),
                }
            },
            | Pat::App { base, ref args } => match body[base] {
                | Pat::Path { ref path } => {
                    let resolver = self.owner.resolver(self.db.upcast());
                    let args = args.iter().copied().map(Arg::Pat).collect();

                    self.lower_pat_app(&resolver, path, args, place, exprs)
                },
                | _ => unreachable!(),
            },
            | Pat::Infix { ref pats, ref ops } => self.lower_pat_infix(place, pats, ops, exprs),
            | ref p => {
                log::warn!(target: "lower_pat", "not yet implemented: {:?}", p);
                None
            },
        }
    }

    fn lower_pat_arg(&mut self, arg: Arg, place: JsExpr, exprs: &mut Vec<JsExpr>) -> Option<JsExpr> {
        match arg {
            | Arg::Pat(id) => self.lower_pat(id, place, exprs),
            | Arg::Fn(f) => f(self, place, exprs),
        }
    }

    pub fn lower_pat_app(
        &mut self,
        resolver: &Resolver,
        path: &hir::Path,
        args: Vec<Arg>,
        place: JsExpr,
        exprs: &mut Vec<JsExpr>,
    ) -> Option<JsExpr> {
        let (resolved, _) = resolver.resolve_value_fully(self.db.upcast(), path).unwrap();

        match resolved {
            | ValueNs::Ctor(id) => {
                let (places, check) = self.deconstruct(id.into(), place);
                let mut args = places
                    .into_iter()
                    .zip(args.into_iter())
                    .filter_map(|(place, a)| self.lower_pat_arg(a, place, exprs));

                let first = check.or_else(|| args.next())?;

                Some(args.fold(first, |lhs, rhs| JsExpr::BinOp {
                    op: "&&",
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }))
            },
            | _ => unreachable!(),
        }
    }

    pub fn lower_pat_infix(
        &mut self,
        place: JsExpr,
        pats: &[hir::PatId],
        ops: &[hir::Path],
        block: &mut Vec<JsExpr>,
    ) -> Option<JsExpr> {
        use std::iter::{once, Peekable};
        let resolver = self.owner.resolver(self.db.upcast());
        let db = self.db;
        let fixities = ops.iter().map(|op| {
            let (resolved, _, _) = resolver.resolve_value(db.upcast(), op).unwrap();

            match resolved {
                | ValueNs::Fixity(id) => match db.fixity_data(id).kind {
                    | hir::FixityKind::Infix { assoc, prec } => ((id, assoc, prec)),
                    | _ => unreachable!(),
                },
                | _ => unreachable!(),
            }
        });

        return go(
            self,
            fixities.peekable(),
            pats.iter().copied().map(Arg::Pat),
            place,
            block,
        );

        fn go<'a>(
            ctx: &mut BodyCtx,
            mut fixities: Peekable<impl Iterator<Item = (hir::id::FixityId, hir::Assoc, hir::Prec)> + 'a>,
            mut pats: impl Iterator<Item = Arg<'a>> + 'a,
            place: JsExpr,
            block: &mut Vec<JsExpr>,
        ) -> Option<JsExpr> {
            if let Some((fix, assoc, prec)) = fixities.next() {
                let data = ctx.db.fixity_data(fix);
                let resolver = fix.resolver(ctx.db.upcast());
                let left = if let Some((fix2, _, prec2)) = fixities.peek() {
                    if fix == *fix2 {
                        match assoc {
                            | hir::Assoc::Left => true,
                            | hir::Assoc::Right => false,
                            | hir::Assoc::None => true,
                        }
                    } else {
                        prec >= *prec2
                    }
                } else {
                    let lhs = pats.next().unwrap();
                    let rhs = pats.next().unwrap();

                    return ctx.lower_pat_app(&resolver, &data.func, vec![lhs, rhs], place, block);
                };

                if left {
                    let lhs = pats.next().unwrap();
                    let rhs = pats.next().unwrap();
                    let pat = Arg::Fn(Box::new(move |ctx, place, exprs| {
                        ctx.lower_pat_app(&resolver, &data.func, vec![lhs, rhs], place, exprs)
                    }));

                    let pats = once(pat).chain(pats).collect::<Vec<_>>();

                    go(ctx, fixities, pats.into_iter(), place, block)
                } else {
                    let lhs = pats.next().unwrap();
                    let rhs = Arg::Fn(Box::new(move |ctx, place, exprs| go(ctx, fixities, pats, place, exprs)));

                    ctx.lower_pat_app(&resolver, &data.func, vec![lhs, rhs], place, block)
                }
            } else {
                ctx.lower_pat_arg(pats.next().unwrap(), place, block)
            }
        }
    }
}

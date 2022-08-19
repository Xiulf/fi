use std::iter::{Enumerate, Peekable};

use syntax::{Assoc, Prec};

use crate::data::FixityKind;
use crate::db::DefDatabase;
use crate::id::FixityId;
use crate::path::Path;
use crate::resolver::{HasResolver, Resolver, ValueNs};

pub trait ProcessInfix {
    type It;
    type Src: Copy;

    fn db(&self) -> &dyn DefDatabase;
    fn resolver(&self) -> &Resolver;
    fn error(&mut self, src: Self::Src) -> Self::It;

    fn on_unresolved(&mut self, src: Self::Src, idx: usize, path: &Path);
    fn on_private(&mut self, src: Self::Src, idx: usize, path: &Path, id: FixityId);

    fn process_infix(
        &mut self,
        items: impl Iterator<Item = Self::It>,
        ops: &[Path],
        src: Self::Src,
        f: impl Fn(&mut Self, usize, Self::It, Self::It, Self::It) -> Self::It,
        resolve: impl FnMut(&mut Self, usize, &Path, Resolver) -> Self::It,
    ) -> Self::It {
        let fixities = ops
            .iter()
            .enumerate()
            .map(|(idx, op)| {
                let (resolved, vis, _) = match self.resolver().resolve_value(self.db(), op) {
                    | Some(r) => r,
                    | None => {
                        self.on_unresolved(src, idx, op);
                        return None;
                    },
                };

                let id = match resolved {
                    | ValueNs::Fixity(id) => id,
                    | _ => return None,
                };

                if !vis.is_visible_from(self.db(), self.resolver().module().unwrap()) {
                    self.on_private(src, idx, op, id);
                    return None;
                }

                match self.db().fixity_data(id).kind {
                    | FixityKind::Infix { assoc, prec } => Some((id, assoc, prec)),
                    | _ => None,
                }
            })
            .collect::<Vec<_>>();

        return go(
            self,
            fixities.into_iter().enumerate().peekable(),
            items,
            src,
            &f,
            resolve,
        );

        fn go<Ctx: ProcessInfix + ?Sized>(
            ctx: &mut Ctx,
            mut fixities: Peekable<Enumerate<impl Iterator<Item = Option<(FixityId, Assoc, Prec)>>>>,
            mut items: impl Iterator<Item = Ctx::It>,
            src: Ctx::Src,
            f: &impl Fn(&mut Ctx, usize, Ctx::It, Ctx::It, Ctx::It) -> Ctx::It,
            mut resolve: impl FnMut(&mut Ctx, usize, &Path, Resolver) -> Ctx::It,
        ) -> Ctx::It {
            if let Some((i, fix)) = fixities.next() {
                let (left, op) = if let Some((id, assoc, prec)) = fix {
                    let data = ctx.db().fixity_data(id);
                    let op = resolve(ctx, i, &data.func, id.resolver(ctx.db()));

                    if let Some((_, next)) = fixities.peek() {
                        let left = if let Some((id2, _, prec2)) = next {
                            if id == *id2 {
                                match assoc {
                                    | Assoc::Left => true,
                                    | Assoc::Right => false,
                                    | Assoc::None => true, // @TODO: report error
                                }
                            } else {
                                prec >= *prec2
                            }
                        } else {
                            false
                        };

                        (left, op)
                    } else {
                        let lhs = items.next().unwrap_or_else(|| ctx.error(src));
                        let rhs = items.next().unwrap_or_else(|| ctx.error(src));

                        return f(ctx, i, op, lhs, rhs);
                    }
                } else {
                    (false, ctx.error(src))
                };

                if left {
                    let lhs = items.next().unwrap_or_else(|| ctx.error(src));
                    let rhs = items.next().unwrap_or_else(|| ctx.error(src));
                    let t = f(ctx, i, op, lhs, rhs);
                    let items = std::iter::once(t).chain(items).collect::<Vec<_>>();

                    go(ctx, fixities, items.into_iter(), src, f, resolve)
                } else {
                    let lhs = items.next().unwrap_or_else(|| ctx.error(src));
                    let rhs = go(ctx, fixities, items, src, f, resolve);

                    f(ctx, i, op, lhs, rhs)
                }
            } else {
                items.next().unwrap_or_else(|| ctx.error(src))
            }
        }
    }
}

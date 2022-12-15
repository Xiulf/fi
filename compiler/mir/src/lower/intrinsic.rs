use expr::Arg;

use super::*;
use crate::repr::Repr;

impl BodyLowerCtx<'_> {
    pub fn lower_intrinsic(
        &mut self,
        expr: hir::ExprId,
        name: &str,
        args: Vec<Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let mut args = args.into_iter();

        match name {
            | "partial" => self.lower_arg(args.next().unwrap(), store_in),
            | "unsafe" => self.lower_arg(args.next().unwrap(), store_in),
            | "apply" => {
                let base = args.next().unwrap();

                self.lower_app(expr, base, args.collect(), store_in)
            },
            | "crash" => {
                let _msg = args.next().unwrap();
                self.builder.abort();
                Operand::Const(Const::Unit, Repr::unit())
            },
            | "addr_of" => {
                let place = self.lower_arg(args.next().unwrap(), &mut None);
                let place = self.place_op(place);
                let repr = self.db.repr_of(self.infer.type_of_expr[expr]);
                let res = self.builder.add_local(LocalKind::Tmp, repr);

                self.builder.init(res);
                self.builder.ref_(Place::new(res), place);
                Operand::Move(Place::new(res))
            },
            | "ptr_read" => {
                let arg = self.lower_arg(args.next().unwrap(), &mut None);
                let place = self.place_op(arg);

                Operand::Copy(place.deref())
            },
            | "ptr_write" => {
                let place = self.lower_arg(args.next().unwrap(), &mut None);
                let place = self.place_op(place);
                let op = self.lower_arg(args.next().unwrap(), &mut None);

                self.builder.assign(place, op);
                Operand::Const(Const::Unit, Repr::unit())
            },
            | "transmute" => {
                let arg = self.lower_arg(args.next().unwrap(), &mut None);
                let ty = self.infer.type_of_expr[expr];
                let res = self.store_in(store_in, ty);

                self.builder.cast(res.clone(), arg);
                Operand::Move(res)
            },
            | _ => todo!("intrinsic '{name}'"),
        }
    }
}

use expr::Arg;

use super::*;
use crate::repr::Repr;

impl BodyLowerCtx<'_> {
    pub fn lower_intrinsic(&mut self, expr: hir::ExprId, name: &str, args: Vec<Arg>) -> Operand {
        let mut args = args.into_iter();

        match name {
            | "partial" => self.lower_arg(args.next().unwrap()),
            | "unsafe" => self.lower_arg(args.next().unwrap()),
            | "apply" => {
                let base = args.next().unwrap();

                self.lower_app(expr, base, args.collect())
            },
            | "crash" => {
                let _msg = args.next().unwrap();
                self.builder.abort();
                Operand::Const(Const::Unit, Repr::unit())
            },
            | "addr_of" => {
                let place = self.lower_arg(args.next().unwrap());
                let place = self.place_op(place);
                let repr = self.db.repr_of(self.infer.type_of_expr[expr]);
                let res = self.builder.add_local(LocalKind::Tmp, repr);

                self.builder.init(res);
                self.builder.ref_(Place::new(res), place);
                Operand::Move(Place::new(res))
            },
            | "ptr_read" => {
                let arg = self.lower_arg(args.next().unwrap());
                let place = self.place_op(arg);

                Operand::Copy(place.deref())
            },
            | "ptr_write" => {
                let place = self.lower_arg(args.next().unwrap());
                let place = self.place_op(place);
                let op = self.lower_arg(args.next().unwrap());

                self.builder.assign(place, op);
                Operand::Const(Const::Unit, Repr::unit())
            },
            | "transmute" => {
                let arg = self.lower_arg(args.next().unwrap());
                let repr = self.db.repr_of(self.infer.type_of_expr[expr]);
                let res = self.builder.add_local(LocalKind::Tmp, repr);

                self.builder.init(res);
                self.builder.cast(Place::new(res), arg);
                Operand::Move(Place::new(res))
            },
            | _ => todo!("intrinsic '{name}'"),
        }
    }
}

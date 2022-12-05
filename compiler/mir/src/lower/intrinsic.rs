use expr::Arg;

use super::*;
use crate::repr::Repr;

impl BodyLowerCtx<'_> {
    pub fn lower_intrinsic(&mut self, name: &str, args: Vec<Arg>) -> Operand {
        let mut args = args.into_iter();

        match name {
            | "addr_of" => {
                let place = self.lower_arg(args.next().unwrap());
                let place = self.place_op(place);
                let repr = Repr::Ptr(Box::new(self.builder.body().locals[place.local.0].repr.clone()));
                let res = self.builder.add_local(LocalKind::Tmp, repr);

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
            | _ => todo!("intrinsic '{name}'"),
        }
    }
}

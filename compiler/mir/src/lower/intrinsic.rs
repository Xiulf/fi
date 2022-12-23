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
            | "ptr_offset" => {
                let ptr = self.lower_arg(args.next().unwrap(), &mut None);
                let offset = self.lower_arg(args.next().unwrap(), &mut None);
                let ty = self.infer.type_of_expr[expr];
                let res = self.store_in(store_in, ty);

                self.builder.binop(res.clone(), BinOp::Offset, ptr, offset);
                Operand::Move(res)
            },
            | "array_index" => {
                let arr = self.lower_arg(args.next().unwrap(), &mut None);
                let arr = self.place_op(arr);
                let idx = self.lower_arg(args.next().unwrap(), &mut None);

                Operand::Copy(arr.index(idx))
            },
            | "iadd" => {
                let lhs = self.lower_arg(args.next().unwrap(), &mut None);
                let rhs = self.lower_arg(args.next().unwrap(), &mut None);
                let ty = self.infer.type_of_expr[expr];
                let res = self.store_in(store_in, ty);

                self.builder.binop(res.clone(), BinOp::Add, lhs, rhs);
                Operand::Move(res)
            },
            | "iconvert" => {
                let val = self.lower_arg(args.next().unwrap(), &mut None);
                let ty = self.infer.type_of_expr[expr];
                let res = self.store_in(store_in, ty);

                self.builder.cast(res.clone(), CastKind::IntToInt, val);
                Operand::Move(res)
            },
            | "transmute" => {
                let arg = self.lower_arg(args.next().unwrap(), &mut None);
                let ty = self.infer.type_of_expr[expr];
                let res = self.store_in(store_in, ty);

                self.builder.cast(res.clone(), CastKind::Bitcast, arg);
                Operand::Move(res)
            },
            | _ => todo!("intrinsic '{name}'"),
        }
    }
}

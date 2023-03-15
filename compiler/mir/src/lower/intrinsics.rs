use hir_def::expr::ExprId;
use hir_def::name::Name;

use super::expr::Arg;
use super::*;

impl Ctx<'_> {
    pub fn lower_intrinsic(
        &mut self,
        expr: ExprId,
        name: Name,
        args: Vec<Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let mut args = args.into_iter();

        match name.as_str(self.db) {
            | "isub" => self.lower_intrinsic_binop(expr, BinOp::Sub, args, store_in),
            | "addr_of" => {
                let place = self.lower_arg(args.next().unwrap());
                let place = self.place_op(place);
                let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                let res = self.builder.add_local(LocalKind::Tmp, repr);

                self.builder.init(res);
                self.builder.addrof(Place::new(res), place);
                Place::new(res).into()
            },
            | s => {
                let args = args.map(|a| self.lower_arg(a)).collect::<Vec<_>>();
                let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                let ret = self.store_in(store_in, ret_repr);

                self.builder.intrinsic(ret.clone(), s.to_string(), args);
                ret.into()
            },
        }
    }

    fn lower_intrinsic_binop(
        &mut self,
        expr: ExprId,
        op: BinOp,
        mut args: impl Iterator<Item = Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let lhs = self.lower_arg(args.next().unwrap());
        let rhs = self.lower_arg(args.next().unwrap());
        let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
        let res = self.store_in(store_in, repr);

        self.builder.binop(res.clone(), op, lhs, rhs);
        res.into()
    }
}

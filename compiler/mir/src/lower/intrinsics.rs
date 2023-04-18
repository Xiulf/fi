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
            | "iadd" => self.lower_intrinsic_binop(expr, BinOp::Add, args, store_in),
            | "isub" => self.lower_intrinsic_binop(expr, BinOp::Sub, args, store_in),
            | "imul" => self.lower_intrinsic_binop(expr, BinOp::Mul, args, store_in),
            | "idiv" => self.lower_intrinsic_binop(expr, BinOp::Div, args, store_in),
            | "irem" => self.lower_intrinsic_binop(expr, BinOp::Rem, args, store_in),
            | "addr_of" => {
                let place = self.lower_arg(args.next().unwrap());
                let place = self.place_op(place);
                let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                let res = self.builder.add_local(LocalKind::Tmp, repr);

                self.builder.init(res);
                self.builder.addrof(Place::new(res), place);
                Place::new(res).into()
            },
            | "iconvert" => self.lower_cast(expr, CastKind::IntToInt, args, store_in),
            | "fconvert" => self.lower_cast(expr, CastKind::FloatToFloat, args, store_in),
            | "ifconvert" => self.lower_cast(expr, CastKind::IntToFloat, args, store_in),
            | "ficonvert" => self.lower_cast(expr, CastKind::FloatToInt, args, store_in),
            | "transmute" => self.lower_cast(expr, CastKind::Bitcast, args, store_in),
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

    fn lower_cast(
        &mut self,
        expr: ExprId,
        kind: CastKind,
        mut args: impl Iterator<Item = Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let arg = self.lower_arg(args.next().unwrap());
        let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
        let res = self.store_in(store_in, repr);

        self.builder.cast(res.clone(), kind, arg);
        Operand::Move(res)
    }
}

use hir::id::{HasModule, LibId};
use hir_def::expr::ExprId;
use hir_def::lang_item;
use hir_def::name::Name;
use hir_ty::ty::Ty;

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
            | "iconvert" => self.lower_cast(expr, CastKind::IntToInt, args, store_in),
            | "fconvert" => self.lower_cast(expr, CastKind::FloatToFloat, args, store_in),
            | "ifconvert" => self.lower_cast(expr, CastKind::IntToFloat, args, store_in),
            | "ficonvert" => self.lower_cast(expr, CastKind::FloatToInt, args, store_in),
            | "transmute" => self.lower_cast(expr, CastKind::Bitcast, args, store_in),
            | "size_of" => self.lower_intrinsic_nullop(expr, NullOp::SizeOf, args, store_in),
            | "align_of" => self.lower_intrinsic_nullop(expr, NullOp::AlignOf, args, store_in),
            | "stride_of" => self.lower_intrinsic_nullop(expr, NullOp::StrideOf, args, store_in),
            | "undefined" => {
                let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                Operand::Const(Const::Undefined, repr)
            },
            | "zeroed" => {
                let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                Operand::Const(Const::Zeroed, repr)
            },
            | "addr_of" => {
                let place = self.lower_arg(args.next().unwrap());
                let place = self.place_op(place);
                let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                let res = self.builder.add_local(LocalKind::Tmp, repr);
                self.builder.addrof(Place::new(res), place);
                Place::new(res).into()
            },
            | "ptr_read" | "deref" => {
                let arg = self.lower_arg(args.next().unwrap());
                self.place_op(arg).deref().into()
            },
            | "ptr_write" => {
                let place = self.lower_arg(args.next().unwrap());
                let place = self.place_op(place);
                let op = self.lower_arg(args.next().unwrap());

                self.builder.assign(place.deref(), op);
                Operand::Const(Const::Unit, Repr::unit(self.db))
            },
            | "array_index" => {
                let arr = self.lower_arg(args.next().unwrap());
                let arr = self.place_op(arr);
                let idx = self.lower_arg(args.next().unwrap());
                arr.index(idx).into()
            },
            | "panic" => {
                let args = args.map(|a| self.lower_arg(a)).collect::<Vec<_>>();
                let ret_repr = repr_of(self.db, self.infer.type_of_expr[expr]);
                let ret = self.store_in(store_in, ret_repr);

                self.builder.intrinsic(ret, "panic".to_string(), args);
                self.builder.abort();
                Operand::Const(Const::Undefined, Repr::uninhabited(self.db))
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

    fn lower_intrinsic_nullop(
        &mut self,
        expr: ExprId,
        op: NullOp,
        mut args: impl Iterator<Item = Arg>,
        store_in: &mut Option<Place>,
    ) -> Operand {
        let proxy = match args.next().unwrap() {
            | Arg::ExprId(e) => self.infer.type_of_expr[e],
            | Arg::Op(_) => unreachable!(),
        };

        let proxy = self.get_proxy_type(proxy);
        let proxy_repr = repr_of(self.db, proxy);
        let repr = repr_of(self.db, self.infer.type_of_expr[expr]);
        let res = self.store_in(store_in, repr);

        self.builder.nullop(res.clone(), op, proxy_repr);
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
        res.into()
    }

    fn lib(&self) -> LibId {
        match self.id {
            | MirValueId::ValueId(id) | MirValueId::Lambda(id, _) => id.container(self.db).module(self.db).lib(self.db),
            | MirValueId::CtorId(id) => id.type_ctor(self.db).module(self.db).lib(self.db),
            | MirValueId::FieldId(id) => id.ctor(self.db).type_ctor(self.db).module(self.db).lib(self.db),
        }
    }

    fn get_proxy_type(&self, proxy: Ty) -> Ty {
        let lib = self.lib();
        let proxy_id = match lang_item::query(self.db, lib, lang_item::PROXY_TYPE).and_then(|it| it.as_type_ctor()) {
            | Some(id) => id,
            | None => {
                tracing::error!("unkown language item '{}'", lang_item::PROXY_TYPE);
                return Ty::new(self.db, hir_ty::ty::TyKind::Error);
            },
        };

        proxy.match_ctor(self.db, proxy_id).unwrap()[0]
    }
}

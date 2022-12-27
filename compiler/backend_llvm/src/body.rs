use std::sync::Arc;

use arena::Idx;
use hir::HirDisplay;
use inkwell::values::{self, BasicValue, BasicValueEnum, CallableValue};
use mir::instance::InstanceDef;
use mir::layout::ReprAndLayout;
use mir::repr::Repr;
use mir::syntax::{
    BinOp, Block, BlockData, CastKind, Const, JumpTarget, Local, LocalKind, Operand, Place, Projection, Rvalue, Stmt,
    Term,
};

use crate::abi::{ArgAbi, EmptySinglePair, PassMode};
use crate::ctx::BodyCtx;
use crate::local::LocalRef;
use crate::operand::{OperandRef, OperandValue};
use crate::place::PlaceRef;

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn codegen(&mut self) {
        let body = self.body.clone();
        let by_ref_locals = crate::ssa::analyze(self);
        tracing::debug!("{}", self.func.get_type());
        tracing::debug!("{}", body.display(self.db.upcast()));

        let entry = self.context.append_basic_block(self.func, "entry");
        let first_block = Idx::from_raw(0u32.into());
        self.builder.position_at_end(entry);

        for (arg, local_ref) in body.blocks[first_block]
            .params
            .iter()
            .zip(self.arg_local_refs(&by_ref_locals))
        {
            self.locals.insert(arg.0, local_ref);
        }

        if self.fn_abi.ret.is_indirect() {
            let ptr = self.func.get_first_param().unwrap().into_pointer_value();
            let val = PlaceRef::new(self.fn_abi.ret.layout.clone(), ptr, None);

            self.ret_ptr = Some(val);
        }

        for (local, data) in body.locals.iter() {
            if !body.blocks[first_block].params.contains(&Local(local)) {
                let repr = self.instance.subst_repr(self.db, &data.repr);
                let layout = self.db.layout_of(repr);
                let value = if by_ref_locals.contains(&Local(local)) {
                    if layout.abi.is_unsized() {
                        todo!();
                    } else {
                        LocalRef::Place(PlaceRef::new_alloca(self.cx, layout))
                    }
                } else if data.kind == LocalKind::Arg {
                    let ty = self.basic_type_for_ral(&layout);
                    let phi = self.builder.build_phi(ty, "");
                    let phi = OperandRef::new_phi(layout, phi);

                    LocalRef::Operand(Some(phi))
                } else {
                    LocalRef::new_operand(self.cx, layout)
                };

                self.locals.insert(local, value);
            }
        }

        for (block, _) in body.blocks.iter() {
            let bb = self.context.append_basic_block(self.func, "");

            self.blocks.insert(block, bb);
        }

        let first_block = self.blocks[first_block];

        self.builder.build_unconditional_branch(first_block);

        for (block, data) in body.blocks.iter() {
            self.codegen_block(Block(block), data);
        }

        if self.func.verify(true) {
            self.fpm.run_on(&self.func);
        } else {
            eprintln!();
            self.func.print_to_stderr();
            eprintln!();

            unsafe {
                self.func.delete();
            }

            panic!("generated invalid function");
        }
    }

    pub fn codegen_block(&mut self, block: Block, data: &BlockData) {
        let bb = self.blocks[block.0];

        self.builder.position_at_end(bb);

        for stmt in &data.stmts {
            self.codegen_stmt(stmt);
        }

        self.codegen_term(&data.term);
    }

    pub fn codegen_term(&mut self, term: &Term) {
        match term {
            | Term::None => unreachable!(),
            | Term::Unreachable => {
                self.builder.build_unreachable();
            },
            | Term::Abort => {
                todo!();
            },
            | Term::Return(op) => match self.fn_abi.ret.mode {
                | PassMode::NoPass => {
                    self.builder.build_return(None);
                },
                | PassMode::ByVal(_) => {
                    let op = self.codegen_operand(op);
                    let value = op.load(self.cx);

                    self.builder.build_return(Some(&value));
                },
                | PassMode::ByValPair(_, _) => {
                    let op = self.codegen_operand(op);
                    let (a, b) = op.pair();

                    self.builder.build_aggregate_return(&[a, b]);
                },
                | PassMode::ByRef { size: Some(_) } => {
                    let ret_ptr = self.ret_ptr.clone().unwrap();
                    let op = self.codegen_operand(op);

                    op.store(self.cx, &ret_ptr);
                    self.builder.build_return(None);
                },
                | PassMode::ByRef { size: None } => todo!(),
            },
            | Term::Jump(target) => {
                self.codegen_jump_target(target);
                let block = self.blocks[target.block.0];
                self.builder.build_unconditional_branch(block);
            },
            | Term::Switch { discr, values, targets } => {
                let mut targets = targets.iter();
                let discr = self.codegen_operand(discr);
                let discr_val = discr.load(self.cx).into_int_value();
                let discr_ty = discr_val.get_type();

                if values.len() == 1 {
                    let val = discr_ty.const_int(values[0] as u64, false);
                    let then = targets.next().unwrap();
                    self.codegen_jump_target(then);
                    let then = self.blocks[then.block.0];
                    let else_ = targets.next().unwrap();
                    self.codegen_jump_target(else_);
                    let else_ = self.blocks[else_.block.0];

                    if discr.layout.is_bool() {
                        let i1_type = self.context.bool_type();
                        let discr_val = self.builder.build_int_truncate(discr_val, i1_type, "");

                        match values[0] {
                            | 0 => self.builder.build_conditional_branch(discr_val, else_, then),
                            | 1 => self.builder.build_conditional_branch(discr_val, then, else_),
                            | _ => unreachable!(),
                        }
                    } else {
                        let cmp = self
                            .builder
                            .build_int_compare(inkwell::IntPredicate::EQ, discr_val, val, "");
                        self.builder.build_conditional_branch(cmp, then, else_)
                    };

                    return;
                }

                let cases = values
                    .iter()
                    .zip(&mut targets)
                    .map(|(&val, target)| {
                        let val = discr_ty.const_int(val as u64, false);
                        self.codegen_jump_target(target);

                        (val, self.blocks[target.block.0])
                    })
                    .collect::<Vec<_>>();

                let else_ = targets.next().unwrap();
                self.codegen_jump_target(else_);
                let else_ = self.blocks[else_.block.0];

                self.builder.build_switch(discr_val, else_, &cases);
            },
        }
    }

    pub fn codegen_jump_target(&mut self, target: &JumpTarget) {
        let body = self.body.clone();
        let data = &body.blocks[target.block.0];
        let block = self.builder.get_insert_block().unwrap();

        for (param, arg) in data.params.iter().zip(target.args.iter()) {
            let op = self.codegen_operand(arg);

            match self.locals[param.0].clone() {
                | LocalRef::Place(ref place) => {
                    op.store(self.cx, place);
                },
                | LocalRef::Operand(Some(phi)) => {
                    let phi = phi.phi();
                    let val = op.load(self.cx);

                    phi.add_incoming(&[(&val, block)]);
                },
                | LocalRef::Operand(None) => unreachable!(),
            }
        }
    }

    pub fn codegen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            | Stmt::Init(_local) => {
                // let repr = &self.body.locals[local.0].repr;
                // let ty = self.basic_type_for_repr(repr);
                // let ptr = self.builder.build_alloca(ty, "");

                // self.locals[local.0].ptr = ptr;
            },
            | Stmt::Drop(_local) => {
                // let ptr = self.locals[local.0].ptr;

                // self.builder.build_free(ptr);
            },
            | Stmt::Assign(place, rvalue) => {
                if place.projection.is_empty() {
                    match &self.locals[place.local.0] {
                        | LocalRef::Place(place) => self.codegen_rvalue(place.clone(), rvalue),
                        | LocalRef::Operand(None) => {
                            let repr = self.instance.subst_repr(self.db, &self.body.locals[place.local.0].repr);
                            let layout = self.db.layout_of(repr);
                            let op = self.codegen_rvalue_operand(layout, rvalue);
                            self.locals[place.local.0] = LocalRef::Operand(Some(op));
                        },
                        | LocalRef::Operand(Some(op)) => {
                            assert!(!op.layout.is_zst());
                            let repr = self.instance.subst_repr(self.db, &self.body.locals[place.local.0].repr);
                            let layout = self.db.layout_of(repr);
                            self.codegen_rvalue_operand(layout, rvalue);
                        },
                    }
                } else {
                    let place = self.codegen_place(place);
                    self.codegen_rvalue(place, rvalue)
                }
            },
            | Stmt::SetDiscriminant(place, ctor) => {
                let place = self.codegen_place(place);
                let ctors = ctor.type_ctor().ctors(self.db.upcast());
                let index = ctors.iter().position(|c| c == ctor).unwrap();

                place.set_discr(self.cx, index);
            },
            | Stmt::Call { place, func, args } => {
                let func = self.codegen_operand(func);
                let func_sig = match &func.layout.repr {
                    | Repr::Func(sig, _) => sig,
                    | _ => unreachable!(),
                };

                let func_abi = self.compute_fn_abi(&func_sig);
                let func = match func.val {
                    | OperandValue::Ref(ptr, None) => self.builder.build_load(ptr, ""),
                    | _ => func.immediate(),
                };

                let func = match func {
                    | BasicValueEnum::PointerValue(pv) => CallableValue::try_from(pv).unwrap(),
                    | v => unreachable!("{}", v.to_string()),
                };

                let ret_ptr = if func_abi.ret.is_indirect() {
                    Some(self.codegen_place(place).ptr.as_basic_value_enum().into())
                } else {
                    None
                };

                let args = ret_ptr
                    .into_iter()
                    .chain(
                        args.iter()
                            .zip(func_abi.args.iter())
                            .flat_map(|(arg, abi)| self.pass_arg(arg, abi)),
                    )
                    .collect::<Vec<_>>();

                let call = self.builder.build_call(func, &args, "").try_as_basic_value();

                match func_abi.ret.mode {
                    | PassMode::ByVal(_) => {
                        let res = OperandRef::new_imm(func_abi.ret.layout, call.left().unwrap());
                        self.store_return(place, res);
                    },
                    | PassMode::ByValPair(_, _) => {
                        let val = call.left().unwrap().into_struct_value();
                        let a = self.builder.build_extract_value(val, 0, "").unwrap();
                        let b = self.builder.build_extract_value(val, 1, "").unwrap();
                        let res = OperandRef::new_pair(func_abi.ret.layout, a, b);
                        self.store_return(place, res);
                    },
                    | _ => {},
                }
            },
        }
    }

    pub fn pass_arg(&mut self, arg: &Operand, abi: &ArgAbi) -> EmptySinglePair<values::BasicMetadataValueEnum<'ctx>> {
        match abi.mode {
            | PassMode::NoPass => EmptySinglePair::Empty,
            | PassMode::ByVal(_) => EmptySinglePair::Single(self.codegen_operand(arg).load(self.cx).into()),
            | PassMode::ByValPair(_, _) => {
                let op = self.codegen_operand(arg);
                let (a, b) = op.pair();
                EmptySinglePair::Pair(a.into(), b.into())
            },
            | PassMode::ByRef { size: Some(_) } => {
                let op = self.codegen_operand(arg);
                let ptr = self.make_ref(op).ptr.as_basic_value_enum();
                EmptySinglePair::Single(ptr.into())
            },
            | PassMode::ByRef { size: None } => todo!(),
        }
    }

    pub fn store_return(&mut self, place: &Place, op: OperandRef<'ctx>) {
        if !place.projection.is_empty() {
            let place = self.codegen_place(place);
            op.store(self.cx, &place);
            return;
        }

        match self.locals[place.local.0].clone() {
            | LocalRef::Place(place) => op.store(self.cx, &place),
            | LocalRef::Operand(None) => self.locals[place.local.0] = LocalRef::Operand(Some(op)),
            | LocalRef::Operand(Some(_)) => unreachable!(),
        }
    }

    pub fn codegen_rvalue(&mut self, place: PlaceRef<'ctx>, rvalue: &Rvalue) {
        match rvalue {
            | Rvalue::Use(op) => {
                let value = self.codegen_operand(op);
                value.store(self.cx, &place);
            },
            | _ => {
                let op = self.codegen_rvalue_operand(place.layout.clone(), rvalue);
                op.store(self.cx, &place);
            },
        }
    }

    pub fn codegen_rvalue_operand(&mut self, layout: Arc<ReprAndLayout>, rvalue: &Rvalue) -> OperandRef<'ctx> {
        match rvalue {
            | Rvalue::Ref(pl) => {
                let ptr = self.codegen_place(pl).ptr.as_basic_value_enum();

                OperandRef::new_imm(layout, ptr)
            },
            | Rvalue::Cast(CastKind::Bitcast, op) => {
                let value = self.codegen_operand(op);
                assert_eq!(value.layout.size, layout.size);
                value.bitcast(self.cx, layout)
            },
            | Rvalue::Cast(CastKind::IntToInt, op) => {
                let ty = self.basic_type_for_ral(&layout).into_int_type();
                let value = self.codegen_operand(op);
                let is_signed = value.layout.is_signed();
                let value = value.load(self.cx).into_int_value();
                let value = self.builder.build_int_cast_sign_flag(value, ty, is_signed, "");

                OperandRef::new_imm(layout, value.as_basic_value_enum())
            },
            | Rvalue::Discriminant(place) => {
                let place = self.codegen_place(place);
                let repr = place.layout.repr.discr();
                let layout = self.db.layout_of(repr);
                let discr = place.get_discr(self.cx, &layout);

                OperandRef::new_imm(layout, discr.as_basic_value_enum())
            },
            | Rvalue::InstanceRef(instance) => {
                let value = match instance.def {
                    | InstanceDef::Def(hir::DefWithBody::Func(_)) => {
                        let func = self.cx.declare_or_codegen_func(instance.clone()).0;
                        func.as_global_value().as_basic_value_enum()
                    },
                    | _ => todo!(),
                };

                OperandRef::new_imm(layout, value)
            },
            | Rvalue::BinOp(op, lhs, rhs) => self.codegen_binop(layout, op, lhs, rhs),
            | _ => todo!(),
        }
    }

    pub fn codegen_binop(
        &mut self,
        layout: Arc<ReprAndLayout>,
        op: &BinOp,
        lhs: &Operand,
        rhs: &Operand,
    ) -> OperandRef<'ctx> {
        let is_float = layout.is_float();
        let is_signed = layout.is_signed();
        let lhs = self.codegen_operand(lhs).load(self.cx);
        let rhs = self.codegen_operand(rhs).load(self.cx);

        if let BinOp::Offset = op {
            let lhs = lhs.into_pointer_value();
            let rhs = rhs.into_int_value();
            let value = unsafe { self.builder.build_gep(lhs, &[rhs], "") };

            OperandRef::new_imm(layout, value.as_basic_value_enum())
        } else if is_float {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();
            let val = match op {
                | BinOp::Add => self.builder.build_float_add(lhs, rhs, ""),
                | BinOp::Sub => self.builder.build_float_sub(lhs, rhs, ""),
                | BinOp::Mul => self.builder.build_float_mul(lhs, rhs, ""),
                | BinOp::Div => self.builder.build_float_div(lhs, rhs, ""),
                | BinOp::Rem => self.builder.build_float_rem(lhs, rhs, ""),
                // | BinOp::Eq => self.builder.build_float_compare(FloatPredicate::OEQ, lhs, rhs, ""),
                // | BinOp::Ne => self.builder.build_float_compare(FloatPredicate::UNE, lhs, rhs, ""),
                // | BinOp::Lt => self.builder.build_float_compare(FloatPredicate::OLT, lhs, rhs, ""),
                // | BinOp::Le => self.builder.build_float_compare(FloatPredicate::OLE, lhs, rhs, ""),
                // | BinOp::Gt => self.builder.build_float_compare(FloatPredicate::OGT, lhs, rhs, ""),
                // | BinOp::Ge => self.builder.build_float_compare(FloatPredicate::OGE, lhs, rhs, ""),
                | _ => unreachable!(),
            };

            OperandRef::new_imm(layout, val.as_basic_value_enum())
        } else {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();
            let val = match op {
                | BinOp::Add => self.builder.build_int_add(lhs, rhs, ""),
                | BinOp::Sub => self.builder.build_int_sub(lhs, rhs, ""),
                | BinOp::Mul => self.builder.build_int_mul(lhs, rhs, ""),
                | BinOp::Div if is_signed => self.builder.build_int_signed_div(lhs, rhs, ""),
                | BinOp::Div => self.builder.build_int_unsigned_div(lhs, rhs, ""),
                | BinOp::Rem if is_signed => self.builder.build_int_signed_rem(lhs, rhs, ""),
                | BinOp::Rem => self.builder.build_int_unsigned_rem(lhs, rhs, ""),
                | BinOp::Lsh => self.builder.build_left_shift(lhs, rhs, ""),
                | BinOp::Rsh => self.builder.build_right_shift(lhs, rhs, is_signed, ""),
                | BinOp::And => self.builder.build_and(lhs, rhs, ""),
                | BinOp::Or => self.builder.build_or(lhs, rhs, ""),
                | BinOp::Xor => self.builder.build_xor(lhs, rhs, ""),
                | _ => unreachable!(),
            };

            OperandRef::new_imm(layout, val.as_basic_value_enum())
        }
    }

    pub fn codegen_operand(&mut self, op: &Operand) -> OperandRef<'ctx> {
        match op {
            | Operand::Move(p) | Operand::Copy(p) => self.codegen_consume(p),
            | Operand::Const(c, r) => self.codegen_const(c, r),
        }
    }

    pub fn codegen_place(&mut self, place: &Place) -> PlaceRef<'ctx> {
        let mut base = 0;
        let mut res = match &self.locals[place.local.0] {
            | LocalRef::Place(place) => place.clone(),
            | LocalRef::Operand(Some(_)) if place.has_deref() => {
                base = 1;
                self.codegen_consume(&Place::new(place.local)).deref(self.cx)
            },
            | LocalRef::Operand(_) => {
                self.func.print_to_stderr();
                unreachable!();
            },
        };

        for proj in place.projection[base..].iter() {
            res = match proj {
                | Projection::Deref => res.deref(self.cx),
                | Projection::Field(i) => res.field(self.cx, *i),
                | Projection::Downcast(ctor) => res.downcast(self.cx, *ctor),
                | Projection::Index(i) => {
                    let i = self.codegen_operand(i).load(self.cx);
                    res.index(self.cx, i)
                },
                | Projection::Slice(lo, hi) => {
                    let lo = self.codegen_operand(lo).load(self.cx);
                    let hi = self.codegen_operand(hi).load(self.cx);
                    res.slice(self.cx, lo, hi)
                },
            };
        }

        res
    }

    pub fn codegen_consume(&mut self, place: &Place) -> OperandRef<'ctx> {
        if let Some(o) = self.maybe_codegen_consume(place) {
            return o;
        }

        let place = self.codegen_place(place);
        place.load_operand(self.cx)
    }

    pub fn maybe_codegen_consume(&mut self, place: &Place) -> Option<OperandRef<'ctx>> {
        match self.locals[place.local.0].clone() {
            | LocalRef::Operand(Some(mut o)) => {
                for proj in place.projection.iter() {
                    match *proj {
                        | Projection::Field(index) => {
                            o = o.field(self.cx, index);
                        },
                        | _ => return None,
                    }
                }

                Some(o)
            },
            | LocalRef::Operand(None) => unreachable!(),
            | LocalRef::Place(_) => None,
        }
    }

    pub fn codegen_const(&mut self, const_: &Const, repr: &Repr) -> OperandRef<'ctx> {
        let repr = self.instance.subst_repr(self.db, repr);
        let layout = self.db.layout_of(repr);
        let ty = self.basic_type_for_ral(&layout);
        let value = match *const_ {
            | Const::Undefined => unreachable!(),
            | Const::Unit => todo!(),
            | Const::Int(i) => ty.into_int_type().const_int(i as u64, true).as_basic_value_enum(),
            | Const::Float(f) => ty
                .into_float_type()
                .const_float(f64::from_bits(f))
                .as_basic_value_enum(),
            | Const::Char(c) => ty.into_int_type().const_int(c as u64, false).as_basic_value_enum(),
            | Const::String(ref s) => {
                let name = self.alloc_const_name("str");
                let ptr = self.builder.build_global_string_ptr(s, &name).as_basic_value_enum();
                let len = self
                    .context
                    .ptr_sized_int_type(&self.target_data, None)
                    .const_int(s.len() as u64, false)
                    .as_basic_value_enum();

                return OperandRef::new_pair(layout, ptr, len);
            },
            | Const::Ctor(ctor) => {
                let type_ctor = ctor.type_ctor();
                let ctors = type_ctor.ctors(self.db.upcast());
                let idx = ctors.iter().position(|&c| c == ctor).unwrap();

                ty.into_int_type().const_int(idx as u64, false).as_basic_value_enum()
            },
        };

        OperandRef::new_imm(layout, value)
    }

    pub fn make_ref(&mut self, op: OperandRef<'ctx>) -> PlaceRef<'ctx> {
        match op.val {
            | OperandValue::Ref(ptr, extra) => PlaceRef::new(op.layout, ptr, extra),
            | _ => {
                let place = PlaceRef::new_alloca(self.cx, op.layout.clone());
                op.store(self.cx, &place);
                place
            },
        }
    }

    pub fn alloc_const_name(&self, prefix: &str) -> String {
        use std::fmt::Write;
        let id = self.consts.get();
        let mut name = String::with_capacity(prefix.len() + 2);
        name.push_str(prefix);
        name.push('.');
        write!(name, "{id}").unwrap();
        self.consts.set(id + 1);
        name
    }
}

use arena::Idx;
use inkwell::values::{self, BasicValue, BasicValueEnum, CallableValue};
use mir::instance::Instance;
use mir::ir::{self, LocalKind};
use mir::repr::Repr;
use triomphe::Arc;

use crate::abi::{ArgAbi, EmptySinglePair, PassMode};
use crate::ctx::BodyCtx;
use crate::layout::{repr_and_layout, ReprAndLayout};
use crate::local::LocalRef;
use crate::operand::{OperandRef, OperandValue};
use crate::place::PlaceRef;

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn codegen(&mut self) {
        let by_ref_locals = crate::ssa::analyze(self);
        let entry = self.context.append_basic_block(self.func, "entry");
        let first_block = Idx::from_raw(0u32.into());
        self.builder.position_at_end(entry);

        for (arg, local_ref) in self.body.blocks[first_block]
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

        for (local, data) in self.body.locals.iter() {
            if !self.body.blocks[first_block].params.contains(&ir::Local(local)) {
                let repr = self.instance.subst_repr(self.db, &data.repr);
                let layout = repr_and_layout(self.db, repr);
                let value = if by_ref_locals.contains(&ir::Local(local)) {
                    if layout.abi.is_unsized() {
                        todo!();
                    } else {
                        LocalRef::Place(PlaceRef::new_alloca(self.cx, layout))
                    }
                } else if data.kind == LocalKind::Arg {
                    continue;
                } else {
                    LocalRef::new_operand(self.cx, layout)
                };

                self.locals.insert(local, value);
            }
        }

        for (block, data) in self.body.blocks.iter() {
            let bb = self.context.append_basic_block(self.func, "");
            self.builder.position_at_end(bb);
            self.blocks.insert(block, bb);

            if block != first_block {
                for local in data.params.iter() {
                    if !by_ref_locals.contains(local) {
                        let repr = self.instance.subst_repr(self.db, &self.body.locals[local.0].repr);
                        let layout = repr_and_layout(self.db, repr);

                        if layout.is_zst() {
                            let zst = OperandRef::new_zst(self.cx, layout);
                            self.locals.insert(local.0, LocalRef::Operand(Some(zst)));
                        } else {
                            let ty = self.basic_type_for_ral(&layout);
                            let phi = self.builder.build_phi(ty, "");
                            let phi = OperandRef::new_phi(layout, phi);
                            self.locals.insert(local.0, LocalRef::Operand(Some(phi)));
                        }
                    }
                }
            }
        }

        let first_block = self.blocks[first_block];
        self.builder.position_at_end(entry);
        self.builder.build_unconditional_branch(first_block);

        for (block, data) in self.body.blocks.iter() {
            self.codegen_block(ir::Block(block), data);
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

    pub fn codegen_block(&mut self, block: ir::Block, data: &ir::BlockData) {
        let bb = self.blocks[block.0];
        self.builder.position_at_end(bb);

        for stmt in &data.statements {
            self.codegen_statement(stmt);
        }

        self.codegen_terminator(&data.terminator);
    }

    pub fn codegen_terminator(&mut self, terminator: &ir::Terminator) {
        match terminator {
            | ir::Terminator::None => unreachable!(),
            | ir::Terminator::Unreachable => {
                self.builder.build_unreachable();
            },
            | ir::Terminator::Abort => {
                todo!();
            },
            | ir::Terminator::Return(op) => self.codegen_return(op),
            | ir::Terminator::Jump(target) => {
                self.codegen_jump_target(target);
                let block = self.blocks[target.block.0];
                self.builder.build_unconditional_branch(block);
            },
            | ir::Terminator::Switch { discr, values, targets } => self.codegen_switch(discr, values, targets),
        }
    }

    pub fn codegen_return(&mut self, op: &ir::Operand) {
        match self.fn_abi.ret.mode {
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
        }
    }

    pub fn codegen_switch(&mut self, discr: &ir::Operand, values: &[i128], targets: &[ir::JumpTarget]) {
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
                };
            } else {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::EQ, discr_val, val, "");
                self.builder.build_conditional_branch(cmp, then, else_);
            }

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
    }

    pub fn codegen_jump_target(&mut self, target: &ir::JumpTarget) {
        let data = &self.body.blocks[target.block.0];
        let block = self.builder.get_insert_block().unwrap();

        for (param, arg) in data.params.iter().zip(target.args.iter()) {
            let op = self.codegen_operand(arg);

            match self.locals[param.0].clone() {
                | LocalRef::Place(place) => {
                    op.store(self.cx, &place);
                },
                | LocalRef::Operand(Some(phi)) => {
                    if !phi.layout.is_zst() {
                        let phi = phi.phi();
                        let val = op.load(self.cx);
                        phi.add_incoming(&[(&val, block)]);
                    }
                },
                | LocalRef::Operand(None) => unreachable!(),
            }
        }
    }

    pub fn codegen_statement(&mut self, stmt: &ir::Statement) {
        match stmt {
            | ir::Statement::Init(_) => todo!(),
            | ir::Statement::Drop(_) => todo!(),
            | ir::Statement::Assign(place, rvalue) => self.codegen_assign(place, rvalue),
            | ir::Statement::Call { place, func, args } => self.codegen_call(place, func, args),
            | _ => todo!("{stmt:?}"),
        }
    }

    pub fn codegen_assign(&mut self, place: &ir::Place, rvalue: &ir::RValue) {
        if place.projection.is_empty() {
            match &self.locals[place.local.0] {
                | LocalRef::Place(place) => self.codegen_rvalue(place.clone(), rvalue),
                | LocalRef::Operand(None) => {
                    let repr = self.instance.subst_repr(self.db, &self.body.locals[place.local.0].repr);
                    let layout = repr_and_layout(self.db, repr);
                    let op = self.codegen_rvalue_operand(layout, rvalue);
                    self.locals[place.local.0] = LocalRef::Operand(Some(op));
                },
                | LocalRef::Operand(Some(op)) => {
                    assert!(op.layout.is_zst());
                    let repr = self.instance.subst_repr(self.db, &self.body.locals[place.local.0].repr);
                    let layout = repr_and_layout(self.db, repr);
                    self.codegen_rvalue_operand(layout, rvalue);
                },
            }
        } else {
            let place = self.codegen_place(place);
            self.codegen_rvalue(place, rvalue);
        }
    }

    pub fn codegen_call(&mut self, place: &ir::Place, func: &ir::Operand, args: &[ir::Operand]) {
        let func = self.codegen_operand(func);
        let func_sig = match &*func.layout.repr {
            | Repr::Func(sig, _) => sig,
            | _ => unreachable!(),
        };

        let func_abi = self.compute_fn_abi(func_sig);
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

        let call = self.builder.build_call(func, &args, "");
        let call = call.try_as_basic_value();

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
    }

    pub fn pass_arg(
        &mut self,
        arg: &ir::Operand,
        abi: &ArgAbi<'ctx>,
    ) -> EmptySinglePair<values::BasicMetadataValueEnum<'ctx>> {
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

    pub fn store_return(&mut self, place: &ir::Place, op: OperandRef<'ctx>) {
        if !place.projection.is_empty() {
            let place = self.codegen_place(place);
            op.store(self.cx, &place);
        }

        match self.locals[place.local.0].clone() {
            | LocalRef::Place(place) => op.store(self.cx, &place),
            | LocalRef::Operand(None) => self.locals[place.local.0] = LocalRef::Operand(Some(op)),
            | LocalRef::Operand(Some(_)) => unreachable!(),
        }
    }

    pub fn codegen_rvalue(&mut self, place: PlaceRef<'ctx>, rvalue: &ir::RValue) {
        match rvalue {
            | ir::RValue::Use(op) => {
                let value = self.codegen_operand(op);
                value.store(self.cx, &place);
            },
            | _ => {
                let op = self.codegen_rvalue_operand(place.layout.clone(), rvalue);
                op.store(self.cx, &place);
            },
        }
    }

    pub fn codegen_rvalue_operand(&mut self, layout: Arc<ReprAndLayout>, rvalue: &ir::RValue) -> OperandRef<'ctx> {
        match rvalue {
            | ir::RValue::Use(op) => self.codegen_operand(op),
            | ir::RValue::AddrOf(place) => {
                let ptr = self.codegen_place(place).ptr.as_basic_value_enum();
                OperandRef::new_imm(layout, ptr)
            },
            | _ => todo!("{rvalue:?}"),
        }
    }

    pub fn codegen_operand(&mut self, op: &ir::Operand) -> OperandRef<'ctx> {
        match op {
            | ir::Operand::Move(p) | ir::Operand::Copy(p) => self.codegen_consume(p),
            | ir::Operand::Const(c, r) => self.codegen_const(c, r),
        }
    }

    pub fn codegen_consume(&mut self, place: &ir::Place) -> OperandRef<'ctx> {
        if let Some(o) = self.maybe_codegen_consume(place) {
            return o;
        }

        let place = self.codegen_place(place);
        place.load_operand(self.cx)
    }

    pub fn maybe_codegen_consume(&mut self, place: &ir::Place) -> Option<OperandRef<'ctx>> {
        match self.locals[place.local.0].clone() {
            | LocalRef::Operand(Some(mut o)) => {
                for proj in place.projection.iter() {
                    match *proj {
                        | ir::Projection::Field(index) => {
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

    pub fn codegen_place(&mut self, place: &ir::Place) -> PlaceRef<'ctx> {
        let mut base = 0;
        let mut res = match &self.locals[place.local.0] {
            | LocalRef::Place(place) => place.clone(),
            | LocalRef::Operand(Some(_)) if matches!(place.projection.get(0), Some(ir::Projection::Deref)) => {
                base = 1;
                self.codegen_consume(&ir::Place::new(place.local)).deref(self.cx)
            },
            | LocalRef::Operand(_) => {
                self.func.print_to_stderr();
                unreachable!();
            },
        };

        for proj in place.projection[base..].iter() {
            res = match proj {
                | ir::Projection::Deref => res.deref(self.cx),
                | ir::Projection::Field(i) => res.field(self.cx, *i),
                | ir::Projection::Downcast(ctor) => res.downcast(self.cx, *ctor),
                | ir::Projection::Index(i) => {
                    let i = self.codegen_operand(i).load(self.cx);
                    res.index(self.cx, i)
                },
                | ir::Projection::Slice(lo, hi) => {
                    let lo = self.codegen_operand(lo).load(self.cx);
                    let hi = self.codegen_operand(hi).load(self.cx);
                    res.slice(self.cx, lo, hi)
                },
            };
        }

        res
    }

    pub fn codegen_const(&mut self, const_: &ir::Const, repr: &Arc<Repr>) -> OperandRef<'ctx> {
        let repr = self.instance.subst_repr(self.db, repr);
        let layout = repr_and_layout(self.db, repr);
        let ty = self.basic_type_for_ral(&layout);
        let value = match *const_ {
            | ir::Const::Undefined => unreachable!(),
            | ir::Const::Zeroed => ty.const_zero(),
            | ir::Const::Unit => return OperandRef::new_zst(self.cx, layout),
            | ir::Const::Int(i) => ty.into_int_type().const_int(i as u64, true).as_basic_value_enum(),
            | ir::Const::Instance(i) => self.codegen_instance(i),
            | _ => todo!("{const_:?}"),
        };

        OperandRef::new_imm(layout, value)
    }

    pub fn codegen_instance(&mut self, inst: Instance) -> values::BasicValueEnum<'ctx> {
        let inst = self.instance.subst_instance(self.db, inst);

        if inst.is_func(self.db) {
            let (value, _) = self.declare_func(inst);
            value.as_global_value().as_basic_value_enum()
        } else {
            todo!("statics");
        }
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
}

use arena::Idx;
use hir::HirDisplay;
use inkwell::values::{BasicValue, BasicValueEnum, CallableValue};
use mir::repr::Repr;
use mir::syntax::{Block, BlockData, Const, LocalKind, Operand, Projection, Rvalue, Stmt, Term};

use crate::ctx::BodyCtx;
use crate::place::PlaceRef;
use crate::value::Value;

impl<'ctx> BodyCtx<'_, '_, 'ctx> {
    pub fn codegen(&mut self) {
        let body = self.body.clone();
        eprintln!("{}", body.display(self.db.upcast()));

        for (local, data) in body.locals.iter() {
            let repr = data.repr.clone();
            let layout = crate::layout::layout_of(self.db, &repr);
            let place = PlaceRef::new_uninit(self.cx, layout);

            self.locals.insert(local, place);
        }

        let entry = self.context.append_basic_block(self.func, "entry");
        let first_block = Idx::from_raw(0u32.into());
        self.builder.position_at_end(entry);

        for (local, data) in body.locals.iter() {
            if data.kind == LocalKind::Arg {
                let ty = self.basic_type_for_layout(&self.locals[local].layout);
                let ptr = self.builder.build_alloca(ty, "");

                self.locals[local].ptr = ptr;
            }
        }

        for (param, val) in body.blocks[first_block].params.iter().zip(self.func.get_param_iter()) {
            self.builder.build_store(self.locals[param.0].ptr, val);
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
            self.func.print_to_stderr();

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
            | Term::Unreachable => {
                self.builder.build_unreachable();
            },
            | Term::Abort => {
                todo!();
            },
            | Term::Return(op) => {
                let value = self.codegen_operand(op).load(self.cx);
                self.builder.build_return(Some(&value));
            },
            | Term::Jump(target) => {
                let block = self.blocks[target.block.0];
                self.builder.build_unconditional_branch(block);
            },
            | Term::Switch { .. } => {
                todo!();
            },
        }
    }

    pub fn codegen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            | Stmt::Init(local) => {
                let repr = &self.body.locals[local.0].repr;
                let ty = self.basic_type_for_repr(repr);
                let ptr = self.builder.build_alloca(ty, "");

                self.locals[local.0].ptr = ptr;
            },
            | Stmt::Drop(local) => {
                let ptr = self.locals[local.0].ptr;

                self.builder.build_free(ptr);
            },
            | Stmt::Assign(place, rvalue) => {
                let place = self.codegen_place(place);
                self.codegen_rvalue(place, rvalue)
            },
            | Stmt::SetDiscriminant(_, _) => todo!(),
            | Stmt::Call { place, func, args } => {
                let place = self.codegen_place(place);
                let func = self.codegen_operand(func).load(self.cx);
                let func = match func {
                    | BasicValueEnum::PointerValue(pv) => CallableValue::try_from(pv).unwrap(),
                    | v => unreachable!("{}", v.to_string()),
                };

                let args = args
                    .iter()
                    .map(|a| self.codegen_operand(a).load(self.cx).into())
                    .collect::<Vec<_>>();
                let res = self
                    .builder
                    .build_call(func, &args, "")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                let res = Value::new(place.layout.clone(), res);

                place.store(self.cx, res);
            },
        }
    }

    pub fn codegen_rvalue(&mut self, place: PlaceRef<'ctx>, rvalue: &Rvalue) {
        match rvalue {
            | Rvalue::Use(op) => {
                let value = self.codegen_operand(op);
                place.store(self.cx, value);
            },
            | Rvalue::Ref(pl) => {
                let ptr = self.codegen_place(pl).ptr;
                let value = Value::new(place.layout.clone(), ptr.as_basic_value_enum());

                place.store(self.cx, value);
            },
            | Rvalue::Cast(op) => {
                let value = self.codegen_operand(op);
                let value = value.cast(self.cx, place.layout.clone());

                place.store(self.cx, value);
            },
            | Rvalue::DefRef(def) => {
                let value = match *def {
                    | hir::DefWithBody::Func(func) => {
                        self.cx.declare_func(func).as_global_value().as_basic_value_enum()
                    },
                    | _ => todo!(),
                };

                let value = Value::new(place.layout.clone(), value);

                place.store(self.cx, value);
            },
            | _ => todo!(),
        }
    }

    pub fn codegen_operand(&mut self, op: &Operand) -> Value<'ctx> {
        match op {
            | Operand::Move(p) | Operand::Copy(p) => self.codegen_place(p).to_value(),
            | Operand::Const(c, r) => self.codegen_const(c, r),
        }
    }

    pub fn codegen_place(&mut self, place: &mir::syntax::Place) -> PlaceRef<'ctx> {
        let mut res = self.locals[place.local.0].clone();

        for proj in place.projection.iter() {
            res = match proj {
                | Projection::Deref => res.deref(self.cx),
                | Projection::Field(i) => res.field(self.cx, *i),
                | Projection::Downcast(ctor) => res.downcast(self.cx, *ctor),
            };
        }

        res
    }

    pub fn codegen_const(&mut self, const_: &Const, repr: &Repr) -> Value<'ctx> {
        let layout = crate::layout::layout_of(self.db, repr);
        let ty = self.basic_type_for_repr(repr);
        let value = match *const_ {
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

                return Value::new_pair(layout, ptr, len);
            },
        };

        Value::new(layout, value)
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

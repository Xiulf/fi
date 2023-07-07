use hir::display::HirDisplay;
use inkwell::values;
use mir::repr::{needs_drop, repr_of, ArrayLen, Repr, ReprKind};

use crate::ctx::CodegenCtx;
use crate::layout::repr_and_layout;

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn get_drop_fn(&mut self, repr: Repr) -> values::FunctionValue<'ctx> {
        if !needs_drop(self.db, repr) {
            return self.get_drop_nop();
        }

        let name = repr.display(self.db).to_string();
        let name = format!("drop.{}", name);

        if let Some(func) = self.module.get_function(&name) {
            return func;
        }

        let ty = self.ptr_type().into();
        let ty = self.context.void_type().fn_type(&[ty], false);
        let func = self.module.add_function(&name, ty, None);
        let prev_block = self.builder.get_insert_block().unwrap();
        let entry_block = self.context.append_basic_block(func, "entry");
        let ptr = func.get_first_param().unwrap().into_pointer_value();
        self.builder.position_at_end(entry_block);
        self.gen_drop_rec(func, ptr, repr);
        self.builder.build_return(None);
        self.builder.position_at_end(prev_block);
        func
    }

    pub fn get_drop_nop(&mut self) -> values::FunctionValue<'ctx> {
        if let Some(func) = self.module.get_function("drop_nop") {
            return func;
        }

        let ptr = self.ptr_type().into();
        let ty = self.context.void_type().fn_type(&[ptr], false);
        self.module.add_function("drop_nop", ty, None)
    }

    fn gen_drop_rec(&mut self, func: values::FunctionValue<'ctx>, ptr: values::PointerValue<'ctx>, repr: Repr) {
        match repr.kind(self.db) {
            | ReprKind::ReprOf(ty) => self.gen_drop_rec(func, ptr, repr_of(self.db, *ty)),
            | ReprKind::Uninhabited
            | ReprKind::Opaque
            | ReprKind::TypeVar(_)
            | ReprKind::Scalar(_)
            | ReprKind::Discr(_)
            | ReprKind::Ptr(..)
            | ReprKind::Func(..) => {},
            | ReprKind::Box(repr) => self.gen_drop_box(ptr, true, *repr),
            | ReprKind::Array(len, repr) => self.gen_drop_array(func, ptr, *len, *repr),
            | ReprKind::Struct(reprs) => self.gen_drop_struct(func, ptr, repr, reprs),
            | ReprKind::Enum(reprs) => self.gen_drop_enum(func, ptr, repr, reprs),
        }
    }

    pub fn gen_drop_box(&mut self, ptr: values::PointerValue<'ctx>, is_ptr: bool, repr: Repr) {
        let inner_layout = repr_and_layout(self.db, repr);
        let box_free = if let Some(func) = self.module.get_function("box_free") {
            func
        } else {
            let ptr = self.ptr_type().into();
            let arg = self.usize_type().into();
            let ty = self.context.void_type().fn_type(&[ptr, arg, ptr], false);
            self.module.add_function("box_free", ty, None)
        };

        let drop_fn = self.get_drop_fn(repr);
        let size = self.usize_type().const_int(inner_layout.size.bytes(), false);
        let ptr = if is_ptr {
            self.builder.build_load(self.ptr_type(), ptr, "")
        } else {
            ptr.into()
        };

        self.builder.build_direct_call(
            box_free,
            &[
                ptr.into(),
                size.into(),
                drop_fn.as_global_value().as_pointer_value().into(),
            ],
            "",
        );
    }

    fn gen_drop_array(
        &mut self,
        _func: values::FunctionValue<'ctx>,
        _ptr: values::PointerValue<'ctx>,
        _len: ArrayLen,
        _repr: Repr,
    ) {
        todo!()
    }

    fn gen_drop_struct(
        &mut self,
        func: values::FunctionValue<'ctx>,
        ptr: values::PointerValue<'ctx>,
        repr: Repr,
        reprs: &[Repr],
    ) {
        let layout = repr_and_layout(self.db, repr);
        let ty = self.basic_type_for_ral(&layout);

        for (i, &repr) in reprs.iter().enumerate() {
            if !needs_drop(self.db, repr) {
                continue;
            }

            let ptr = self.builder.build_struct_gep(ty, ptr, i as u32, "").unwrap();
            self.gen_drop_rec(func, ptr, repr);
        }
    }

    fn gen_drop_enum(
        &mut self,
        func: values::FunctionValue<'ctx>,
        ptr: values::PointerValue<'ctx>,
        repr: Repr,
        reprs: &[Repr],
    ) {
        let discr_layout = repr_and_layout(self.db, Repr::new(self.db, ReprKind::Discr(repr)));
        let discr_ty = self.basic_type_for_ral(&discr_layout).into_int_type();
        let layout = repr_and_layout(self.db, repr);
        let ty = self.basic_type_for_ral(&layout);
        let exit_block = self.context.append_basic_block(func, "");
        let prev_block = self.builder.get_insert_block().unwrap();
        let mut cases = Vec::with_capacity(reprs.len());

        for (i, &repr) in reprs.iter().enumerate() {
            if !needs_drop(self.db, repr) {
                continue;
            }

            let block = self.context.append_basic_block(func, "");
            let case = discr_ty.const_int(i as u64, false);
            self.builder.position_at_end(block);
            let ptr = self.builder.build_struct_gep(ty, ptr, 1, "").unwrap();
            self.gen_drop_rec(func, ptr, repr);
            self.builder.build_unconditional_branch(exit_block);
            cases.push((case, block));
        }

        self.builder.position_at_end(prev_block);
        let discr = self.builder.build_struct_gep(ty, ptr, 0, "").unwrap();
        let discr = self.builder.build_load(discr_ty, discr, "").into_int_value();
        self.builder.build_switch(discr, exit_block, &cases);
        self.builder.position_at_end(exit_block);
    }
}

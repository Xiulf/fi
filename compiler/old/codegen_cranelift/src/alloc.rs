use super::*;

impl<'ctx> ClifBackend<'ctx> {
    pub fn trans_const_alloc(
        fx: &mut FunctionCtx<Self>,
        const_: &mir::Const,
        layout: layout::TyLayout<mir::Ty>,
    ) -> <Self as Backend>::Static {
        let data_id = fx
            .mcx
            .module
            .declare_data(
                &format!("__const_{}", fx.mcx.backend.anon_count),
                cranelift_module::Linkage::Local,
                false,
                false,
            )
            .unwrap();

        fx.mcx.backend.anon_count += 1;

        let mut dcx = cranelift_module::DataContext::new();
        let mut bytes = Vec::with_capacity(layout.size.bytes() as usize);
        let mut i = 0;

        fn rec(
            fx: &mut FunctionCtx<ClifBackend>,
            dcx: &mut cranelift_module::DataContext,
            const_: &mir::Const,
            layout: &layout::TyLayout<mir::Ty>,
            bytes: &mut Vec<u8>,
            i: &mut usize,
        ) {
            match const_ {
                mir::Const::Undefined => bytes.resize(bytes.capacity(), 0),
                mir::Const::Scalar(s) => {
                    bytes.extend(&s.to_le_bytes()[..layout.size.bytes() as usize])
                }
                mir::Const::Tuple(consts) => match &layout.fields {
                    layout::FieldsShape::Arbitrary { offsets } => {
                        let start = *i;

                        for (j, (c, offset)) in consts.iter().zip(offsets).enumerate() {
                            bytes.resize(start + offset.bytes() as usize, 0);
                            *i = start + offset.bytes() as usize;

                            let field = layout.field(fx.lib, fx.db.to_layout_db(), j);

                            rec(fx, dcx, c, &field, bytes, i);
                            *i += field.size.bytes() as usize;
                        }
                    }
                    _ => unimplemented!(),
                },
                mir::Const::FuncAddr(id) => {
                    let func = if let Some((f, _, _)) = fx.func_ids.get(id) {
                        *f
                    } else {
                        ClifBackend::declare_foreign_func(fx.mcx, *id)
                    };

                    let func = fx.module.declare_func_in_data(func, dcx);

                    bytes.resize(*i + layout.size.bytes() as usize, 0);
                    dcx.write_function_addr(*i as u32, func);
                }
                mir::Const::Ref(inner) => {
                    let data_id = ClifBackend::trans_const_alloc(
                        fx,
                        inner,
                        layout.pointee(fx.lib, fx.db.to_layout_db()),
                    );

                    let global = fx.module.declare_data_in_data(data_id, dcx);

                    bytes.resize(*i + layout.size.bytes() as usize, 0);
                    dcx.write_data_addr(*i as u32, global, 0);
                }
                _ => unimplemented!(),
            }
        }

        rec(fx, &mut dcx, const_, &layout, &mut bytes, &mut i);

        dcx.define(bytes.into_boxed_slice());
        fx.module.define_data(data_id, &dcx).unwrap();

        data_id
    }
}

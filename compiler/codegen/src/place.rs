use crate::{ir, mir, FunctionCtx};
use layout::TyLayout;
use std::marker::PhantomData;

pub struct Place<'ctx, V: ir::AnyValue<'ctx>> {
    pub val: V,
    pub extra: Option<V>,
    pub layout: TyLayout<mir::Ty>,
    _marker: PhantomData<&'ctx ()>,
}

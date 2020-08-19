use crate::tcx::Tcx;
use crate::ty::*;

impl<'tcx> Tcx<'tcx> {
    pub fn infer_type(&self, id: &hir::Id) -> Ty<'tcx> {
        let type_ = &self.package.types[id];

        match &type_.kind {
            hir::TypeKind::Err => self.builtin.error,
            hir::TypeKind::Infer => self.new_var(),
            hir::TypeKind::Path { res } => match res {
                hir::Res::Item(id) => self.type_of(id),
                hir::Res::PrimTy(prim) => match prim {
                    hir::PrimTy::Never => self.builtin.never,
                    hir::PrimTy::Bool => self.builtin.bool,
                    hir::PrimTy::Str => self.builtin.str,
                    hir::PrimTy::Int(255, true) => self.new_int(),
                    hir::PrimTy::Int(255, false) => self.new_uint(),
                    hir::PrimTy::Float(255) => self.new_float(),
                    hir::PrimTy::Int(0, true) => self.builtin.isize,
                    hir::PrimTy::Int(8, true) => self.builtin.i8,
                    hir::PrimTy::Int(16, true) => self.builtin.i16,
                    hir::PrimTy::Int(32, true) => self.builtin.i32,
                    hir::PrimTy::Int(64, true) => self.builtin.i64,
                    hir::PrimTy::Int(128, true) => self.builtin.i128,
                    hir::PrimTy::Int(0, false) => self.builtin.usize,
                    hir::PrimTy::Int(8, false) => self.builtin.u8,
                    hir::PrimTy::Int(16, false) => self.builtin.u16,
                    hir::PrimTy::Int(32, false) => self.builtin.u32,
                    hir::PrimTy::Int(64, false) => self.builtin.u64,
                    hir::PrimTy::Int(128, false) => self.builtin.u128,
                    hir::PrimTy::Float(32) => self.builtin.f32,
                    hir::PrimTy::Float(64) => self.builtin.f64,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            hir::TypeKind::Ref { mut_, to } => {
                let to = self.type_of(to);

                self.intern_ty(Type::Ref(*mut_, to))
            }
            hir::TypeKind::Tuple { tys } => {
                let tys = self
                    .arena
                    .alloc_slice_fill_iter(tys.iter().map(|ty| self.type_of(ty)));

                self.intern_ty(Type::Tuple(tys))
            }
            hir::TypeKind::Func { params, ret } => {
                let params = self
                    .arena
                    .alloc_slice_fill_iter(params.iter().map(|p| Param {
                        name: p.name,
                        ty: self.type_of(&p.ty),
                    }));

                let ret = self.type_of(ret);

                self.intern_ty(Type::Func(params, ret))
            }
        }
    }
}

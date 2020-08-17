mod expr;
mod item;
mod ty;
mod unify;
mod verify;

use crate::constraint::*;
use crate::ty::*;
use diagnostics::{Reporter, Span};
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;

pub struct Tcx<'tcx> {
    reporter: &'tcx Reporter,
    arena: &'tcx bumpalo::Bump,
    package: &'tcx hir::Package,
    types: RefCell<BTreeMap<hir::Id, Ty<'tcx>>>,
    constraints: RefCell<Constraints<'tcx>>,
    ty_vars: Cell<usize>,
    pub builtin: BuiltinTypes<'tcx>,
}

pub struct BuiltinTypes<'tcx> {
    pub error: Ty<'tcx>,
    pub never: Ty<'tcx>,
    pub unit: Ty<'tcx>,
    pub bool: Ty<'tcx>,
    pub str: Ty<'tcx>,
    pub typeid: Ty<'tcx>,
    pub u8: Ty<'tcx>,
    pub u16: Ty<'tcx>,
    pub u32: Ty<'tcx>,
    pub u64: Ty<'tcx>,
    pub u128: Ty<'tcx>,
    pub usize: Ty<'tcx>,
    pub i8: Ty<'tcx>,
    pub i16: Ty<'tcx>,
    pub i32: Ty<'tcx>,
    pub i64: Ty<'tcx>,
    pub i128: Ty<'tcx>,
    pub isize: Ty<'tcx>,
    pub f32: Ty<'tcx>,
    pub f64: Ty<'tcx>,
}

impl<'tcx> BuiltinTypes<'tcx> {
    pub fn new(arena: &'tcx bumpalo::Bump) -> Self {
        BuiltinTypes {
            error: arena.alloc(Type::Error),
            never: arena.alloc(Type::Never),
            unit: arena.alloc(Type::Tuple(&[])),
            bool: arena.alloc(Type::Bool),
            str: arena.alloc(Type::Str),
            typeid: arena.alloc(Type::TypeId),
            u8: arena.alloc(Type::UInt(8)),
            u16: arena.alloc(Type::UInt(16)),
            u32: arena.alloc(Type::UInt(32)),
            u64: arena.alloc(Type::UInt(64)),
            u128: arena.alloc(Type::UInt(128)),
            usize: arena.alloc(Type::UInt(0)),
            i8: arena.alloc(Type::Int(8)),
            i16: arena.alloc(Type::Int(16)),
            i32: arena.alloc(Type::Int(32)),
            i64: arena.alloc(Type::Int(64)),
            i128: arena.alloc(Type::Int(128)),
            isize: arena.alloc(Type::Int(0)),
            f32: arena.alloc(Type::Float(32)),
            f64: arena.alloc(Type::Float(64)),
        }
    }
}

impl<'tcx> Tcx<'tcx> {
    pub fn new(
        reporter: &'tcx Reporter,
        arena: &'tcx bumpalo::Bump,
        package: &'tcx hir::Package,
    ) -> Self {
        Tcx {
            reporter,
            arena,
            package,
            types: RefCell::new(BTreeMap::new()),
            constraints: RefCell::new(Constraints::new()),
            builtin: BuiltinTypes::new(arena),
            ty_vars: Cell::new(0),
        }
    }

    pub fn type_of(&self, id: &hir::Id) -> Ty<'tcx> {
        let types = self.types.borrow();

        if let Some(ty) = types.get(id) {
            ty
        } else {
            std::mem::drop(types);

            let ty = if let Some(_) = self.package.items.get(id) {
                let ty = self.infer_item(id);

                self.types.borrow_mut().insert(*id, ty);
                self.check_item(id);

                return ty;
            } else if let Some(_) = self.package.exprs.get(id) {
                self.infer_expr(id)
            } else if let Some(_) = self.package.types.get(id) {
                self.infer_type(id)
            } else {
                panic!("unused id {}", id);
            };

            self.types.borrow_mut().insert(*id, ty);

            ty
        }
    }

    pub fn span_of(&self, id: &hir::Id) -> Span {
        if let Some(item) = self.package.items.get(id) {
            item.span
        } else if let Some(expr) = self.package.exprs.get(id) {
            expr.span
        } else if let Some(ty) = self.package.types.get(id) {
            ty.span
        } else {
            panic!("unused id {}", id);
        }
    }

    pub fn constrain(&self, cs: Constraint<'tcx>) {
        self.constraints.borrow_mut().push(cs);
    }

    pub fn new_var(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.arena.alloc(Type::Var(TypeVar(var)))
    }

    pub fn new_int(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.arena.alloc(Type::VInt(TypeVar(var)))
    }

    pub fn new_uint(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.arena.alloc(Type::VUInt(TypeVar(var)))
    }

    pub fn new_float(&self) -> Ty<'tcx> {
        let var = self.ty_vars.get();

        self.ty_vars.set(var + 1);
        self.arena.alloc(Type::VFloat(TypeVar(var)))
    }
}

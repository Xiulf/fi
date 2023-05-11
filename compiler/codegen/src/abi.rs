use inkwell::types::{self, BasicType};
use mir::repr::{BoxKind, Repr, ReprKind, Signature};

use crate::ctx::CodegenCtx;
use crate::layout::{primitive_size, repr_and_layout, Abi, ReprAndLayout, Size};
use crate::Db;

#[derive(Clone, Copy)]
pub enum PassMode<'ctx> {
    NoPass,
    ByVal(types::BasicTypeEnum<'ctx>),
    ByValPair(types::BasicTypeEnum<'ctx>, types::BasicTypeEnum<'ctx>),
    ByRef { size: Option<Size> },
}

#[derive(Clone, Copy)]
pub enum EmptySinglePair<T> {
    Empty,
    Single(T),
    Pair(T, T),
}

#[derive(Debug, Clone)]
pub struct FnAbi<'ctx> {
    pub args: Box<[ArgAbi<'ctx>]>,
    pub ret: ArgAbi<'ctx>,
    pub is_varargs: bool,
}

#[derive(Debug, Clone)]
pub struct ArgAbi<'ctx> {
    pub layout: ReprAndLayout,
    pub mode: PassMode<'ctx>,
}

impl ArgAbi<'_> {
    pub fn is_indirect(&self) -> bool {
        matches!(self.mode, PassMode::ByRef { .. })
    }
}

impl<T> Iterator for EmptySinglePair<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, EmptySinglePair::Empty) {
            | EmptySinglePair::Empty => None,
            | EmptySinglePair::Single(v) => Some(v),
            | EmptySinglePair::Pair(a, b) => {
                *self = EmptySinglePair::Single(b);
                Some(a)
            },
        }
    }
}

impl<'ctx> CodegenCtx<'_, 'ctx> {
    pub fn compute_fn_abi(&self, sig: &Signature, env: Option<Repr>) -> FnAbi<'ctx> {
        let ret_layout = repr_and_layout(self.db, sig.ret.clone());
        let is_varargs = sig.is_varargs;
        let ret = self.compute_layout_abi(ret_layout);
        let env = env.map(|e| {
            let repr = Repr::new(self.db, ReprKind::Box(BoxKind::Box, e));
            let layout = repr_and_layout(self.db, repr);
            self.compute_layout_abi(layout)
        });

        let args = env
            .into_iter()
            .chain(sig.params.iter().map(|a| {
                let layout = repr_and_layout(self.db, a.clone());
                self.compute_layout_abi(layout)
            }))
            .collect();

        FnAbi { args, ret, is_varargs }
    }

    pub fn compute_layout_abi(&self, layout: ReprAndLayout) -> ArgAbi<'ctx> {
        let pass_mode = self.pass_mode(&layout);

        ArgAbi {
            layout,
            mode: pass_mode,
        }
    }

    pub fn pass_mode(&self, layout: &ReprAndLayout) -> PassMode<'ctx> {
        if layout.is_zst() {
            PassMode::NoPass
        } else if let ReprKind::Box(k @ (BoxKind::Ref | BoxKind::Ptr), el) = layout.repr.kind(self.db) {
            match el.kind(self.db) {
                | ReprKind::Slice(_) => {
                    let a_ty = self.basic_type_for_ral(&layout.field(self.db, 0).unwrap());
                    let b_ty = self.basic_type_for_ral(&layout.field(self.db, 1).unwrap());
                    PassMode::ByValPair(a_ty, b_ty)
                },
                | _ if *k == BoxKind::Ptr => {
                    let ty = self.basic_type_for_ral(&layout.elem(self.db).unwrap());
                    let ty = ty.ptr_type(Default::default()).as_basic_type_enum();
                    PassMode::ByVal(ty)
                },
                | _ => PassMode::ByRef {
                    size: Some(layout.size),
                },
            }
        } else {
            match &layout.abi {
                | Abi::Uninhabited => PassMode::NoPass,
                | Abi::Scalar(_) => PassMode::ByVal(self.basic_type_for_ral(layout)),
                | Abi::ScalarPair(a, b) => {
                    let a_ty = self.basic_type_for_ral(&layout.field(self.db, 0).unwrap());
                    let b_ty = self.basic_type_for_ral(&layout.field(self.db, 1).unwrap());

                    if primitive_size(a.value, &self.target.triple).bits() == 128
                        && primitive_size(b.value, &self.target.triple).bits() == 128
                    {
                        PassMode::ByRef {
                            size: Some(layout.size),
                        }
                    } else {
                        PassMode::ByValPair(a_ty, b_ty)
                    }
                },
                | Abi::Aggregate { sized: true } => PassMode::ByRef {
                    size: Some(layout.size),
                },
                | Abi::Aggregate { sized: false } => PassMode::ByRef { size: None },
            }
        }
    }
}

impl hir::display::HirDisplay for FnAbi<'_> {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        write!(f, "(")?;
        f.write_joined(self.args.iter(), ", ")?;
        write!(f, ") -> {}", self.ret.display(f.db))
    }
}

impl hir::display::HirDisplay for ArgAbi<'_> {
    type Db<'a> = dyn Db + 'a;

    fn hir_fmt(&self, f: &mut hir::display::HirFormatter<Self::Db<'_>>) -> std::fmt::Result {
        use std::fmt::Write as _;
        write!(f, "{} :: {:?}", self.layout.repr.display(f.db), self.mode)
    }
}

impl std::fmt::Debug for PassMode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::NoPass => write!(f, "NoPass"),
            | Self::ByRef { size: _ } => write!(f, "ByRef"),
            | Self::ByVal(ty) => write!(f, "ByVal({})", ty),
            | Self::ByValPair(a, b) => write!(f, "ByValPair({}, {})", a, b),
        }
    }
}

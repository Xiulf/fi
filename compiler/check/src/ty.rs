use crate::list::List;
use hir::ir::{DefId, HirId};

pub type Ty<'tcx> = &'tcx Type<'tcx>;

pub enum Type<'tcx> {
    Error,
    Infer(InferVar),
    Var(TypeVar),
    Func(&'tcx List<Ty<'tcx>>, Ty<'tcx>),
    ForAll(TypeVar, Ty<'tcx>),
    Data(DefId, &'tcx List<Variant<'tcx>>),

    Ptr(Ty<'tcx>),
    Tuple(&'tcx List<Ty<'tcx>>),
    Array(Ty<'tcx>, usize),
    Slice(Ty<'tcx>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InferVar(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(pub HirId);

pub struct Variant<'tcx> {
    pub id: DefId,
    pub tys: &'tcx List<Ty<'tcx>>,
}

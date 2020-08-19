use crate::ty::*;
use diagnostics::Span;
use std::fmt;

pub type Constraints<'tcx> = Vec<Constraint<'tcx>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint<'tcx> {
    Equal(Ty<'tcx>, Span, Ty<'tcx>, Span),
    PtrArith(Ty<'tcx>, Span, Ty<'tcx>, Span),
    IsNum(Ty<'tcx>, Span),
    IsInt(Ty<'tcx>, Span),
    Call(Ty<'tcx>, Span, Vec<Param<'tcx>>, Ty<'tcx>, Span),
    Field(Ty<'tcx>, Span, Ident, Ty<'tcx>, Span),
    Index(Ty<'tcx>, Span, Ty<'tcx>, Span),
}

impl fmt::Display for Constraint<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constraint::Equal(a, _, b, _) => write!(f, "{} == {}", a, b),
            Constraint::PtrArith(a, _, b, _) => write!(f, "{} ~= {}", a, b),
            Constraint::IsNum(ty, _) => write!(f, "{} == int | uint | float", ty),
            Constraint::IsInt(ty, _) => write!(f, "{} == int | uint", ty),
            Constraint::Call(func, _, params, ret, _) => write!(
                f,
                "({})({}) -> {}",
                func,
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ret
            ),
            Constraint::Field(obj, _, field, ty, _) => write!(f, "{}.{} == {}", obj, field, ty),
            Constraint::Index(list, _, ty, _) => write!(f, "{}[_] == {}", list, ty),
        }
    }
}

use super::*;
use check::ty::Ty;
use layout::TyLayout;

pub fn can_return_to_ssa_var(fx: &FunctionCtx<impl Backend>, dest_layout: &TyLayout<Ty>) -> bool {
    match get_pass_mode(fx, dest_layout) {
        PassMode::NoPass | PassMode::ByVal(_) | PassMode::ByValPair(_, _) => true,
        PassMode::ByRef { size: _ } => false,
    }
}

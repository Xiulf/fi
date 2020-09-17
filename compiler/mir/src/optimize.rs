pub mod vars;

use crate::*;

pub fn optimize(package: &mut Package) {
    vars::optimize(package);
}

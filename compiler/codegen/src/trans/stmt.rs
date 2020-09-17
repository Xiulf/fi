use crate::FunctionCtx;
use cranelift_module::Backend;

impl<'a, 'tcx, B: Backend> FunctionCtx<'a, 'tcx, B> {
    pub fn trans_stmt(&mut self, stmt: &mir::Stmt<'tcx>) {
        match stmt {
            mir::Stmt::Nop => {}
            mir::Stmt::Assign(place, rvalue) => {
                let place = self.trans_place(place);

                self.trans_rvalue(place, rvalue);
            }
            mir::Stmt::VarLive(_) => {}
            mir::Stmt::VarDead(_) => {}
        }
    }
}

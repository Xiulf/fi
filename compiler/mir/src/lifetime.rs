use crate::visit::VisitorMut;
use crate::*;
use std::collections::{HashMap, HashSet};

pub fn mark_lifetimes(package: &mut Package) {
    Lifetimes.visit_package(package);
}

struct Lifetimes;

impl<'tcx> VisitorMut<'tcx> for Lifetimes {
    fn visit_body(&mut self, body: &mut Body<'tcx>) {
        // let first_block = body.blocks.get_mut(&BlockId::ENTRY).unwrap();
        //
        // for (id, local) in &body.locals {
        //     if local.kind != LocalKind::Ret && local.kind != LocalKind::Arg {
        //         first_block.stmts.insert(0, Stmt::VarLive(*id));
        //     }
        // }
    }
}

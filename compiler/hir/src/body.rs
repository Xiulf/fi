mod lower;

use arena::{Arena, ArenaMap};
use either::Either;
pub use lower::query;
use rustc_hash::FxHashMap;
use syntax::ptr::AstPtr;
use vfs::File;

use crate::expr::{Expr, ExprId};
use crate::id::ValueId;
use crate::pat::{Pat, PatId};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Body {
    exprs: Arena<Expr>,
    pats: Arena<Pat>,
    params: Box<[PatId]>,
    body_expr: ExprId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BodySourceMap {
    file: File,
    expr_to_src: ArenaMap<ExprId, ExprSrc>,
    src_to_expr: FxHashMap<AstPtr<syntax::ast::Expr>, ExprId>,
    pat_to_src: ArenaMap<PatId, PatSrc>,
    src_to_pat: FxHashMap<AstPtr<syntax::ast::Pat>, PatId>,
}

pub type ExprSrc = Either<AstPtr<syntax::ast::Expr>, SyntheticSugar>;
pub type PatSrc = Either<AstPtr<syntax::ast::Pat>, SyntheticSugar>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntheticSugar(pub ValueId);

pub use crate::symbol::{Ident, Symbol};
pub use diagnostics::Span;

#[derive(Debug)]
pub struct Module {
    pub span: Span,
    pub name: Ident,
}

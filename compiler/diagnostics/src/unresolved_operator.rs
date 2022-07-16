use hir::diagnostic::Diagnostic as _;
use syntax::TextRange;

use super::*;

pub struct UnresolvedOperator<'db, 'd, DB: hir::db::HirDatabase> {
    _db: &'db DB,
    _diag: &'d hir::diagnostic::UnresolvedOperator,
    location: TextRange,
}

impl<'db, 'd, DB: hir::db::HirDatabase> Diagnostic for UnresolvedOperator<'db, 'd, DB> {
    fn title(&self) -> String {
        "unknown operator".into()
    }

    fn range(&self) -> TextRange {
        self.location
    }
}

impl<'db, 'd, DB: hir::db::HirDatabase> UnresolvedOperator<'db, 'd, DB> {
    pub fn new(db: &'db DB, diag: &'d hir::diagnostic::UnresolvedOperator) -> Self {
        use syntax::syntax_kind::*;
        const SYMBOLS: [SyntaxKind; 10] = [
            OPERATOR, ARROW, LEFT_ARROW, DBL_DOT, DOT, COMMA, COLON, PIPE, EQUALS, AT,
        ];

        let parse = db.parse(diag.file);
        let location = diag
            .src
            .to_node(&parse.syntax_node())
            .children_with_tokens()
            .filter(|n| SYMBOLS.contains(&n.kind()))
            .nth(diag.idx)
            .map(|n| n.text_range())
            .unwrap_or_else(|| diag.display_source().value.range());

        Self {
            _db: db,
            _diag: diag,
            location,
        }
    }
}

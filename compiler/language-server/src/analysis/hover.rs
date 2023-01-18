mod render;

use hir::semantics::Semantics;
use hir::InFile;
use syntax::{ast, match_ast, AstNode, SyntaxKind, SyntaxToken, TextSize};
use vfs::FileId;

use super::classify::{self, Symbol};
use super::RangeInfo;
use crate::db::LspDatabase;

pub struct HoverInfo {
    pub text: String,
}

pub fn hover(db: &LspDatabase, file_offset: InFile<TextSize>) -> Option<RangeInfo<HoverInfo>> {
    let sema = Semantics::new(db);
    let file_id = file_offset.file_id;
    let offset = file_offset.value;
    let file = sema.parse(file_id).syntax().clone();
    let token = file.token_at_offset(offset).max_by_key(|t| match t.kind() {
        | SyntaxKind::IDENT | SyntaxKind::INT => 3,
        | SyntaxKind::L_PAREN | SyntaxKind::R_PAREN => 2,
        | k if k.is_keyword() => 2,
        | k if k.is_trivia() => 0,
        | _ => 1,
    })?;

    let symbol = classify::classify_token(&sema, &token);

    tracing::info!("{token:?}, {symbol:?}");
    symbol
        .and_then(|symbol| hover_for_symbol(&sema, file_id, symbol))
        .map(|info| RangeInfo {
            range: token.text_range(),
            info,
        })
        .or_else(|| hover_for_type(&sema, &token))
}

fn hover_for_symbol(sema: &Semantics<LspDatabase>, _file_id: FileId, symbol: Symbol) -> Option<HoverInfo> {
    let text = render::symbol(sema.db, symbol)?;

    Some(HoverInfo { text })
}

fn hover_for_type(sema: &Semantics<LspDatabase>, token: &SyntaxToken) -> Option<RangeInfo<HoverInfo>> {
    let node = token
        .ancestors()
        .take_while(|it| !ast::Item::can_cast(it.kind()))
        .find(|n| ast::Expr::can_cast(n.kind()) || ast::Pat::can_cast(n.kind()) || ast::Type::can_cast(n.kind()))?;

    let text = match_ast! {
        match node {
            Expr(it) => render::type_info(sema, &Result::Ok(it))?,
            Pat(it) => render::type_info(sema, &Result::Err(it))?,
            Type(it) => render::kind_info(sema, &it)?,
            _ => return None,
        }
    };

    Some(RangeInfo {
        range: node.text_range(),
        info: HoverInfo { text },
    })
}

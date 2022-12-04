use lsp_types::{Hover, HoverParams};

use crate::state::LspStateSnapshot;
use crate::util;

pub fn handle_hover(snap: LspStateSnapshot, params: HoverParams) -> anyhow::Result<Option<Hover>> {
    let file_offset = util::file_offset(&snap, params.text_document_position_params)?;
    let info = match snap.analysis.hover(file_offset)? {
        | None => return Ok(None),
        | Some(info) => info,
    };

    let line_index = snap.line_index(file_offset.file_id)?;
    let range = util::lsp_range(&line_index, info.range);
    let markup = lsp_types::MarkupContent {
        kind: lsp_types::MarkupKind::PlainText,
        value: info.info.text,
    };

    let hover = lsp_types::Hover {
        range: Some(range),
        contents: lsp_types::HoverContents::Markup(markup),
    };

    Ok(Some(hover))
}

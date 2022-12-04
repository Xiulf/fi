use std::path::{Component, Path, Prefix};
use std::str::FromStr;

use base_db::input::{FileId, LineCol, LineIndex};
use hir::InFile;
use lsp_types::{TextDocumentPositionParams, Url};
use paths::AbsPathBuf;
use syntax::{TextRange, TextSize};
use vfs::VfsPath;

use crate::state::LspStateSnapshot;

pub fn file_path(uri: &Url) -> anyhow::Result<AbsPathBuf> {
    uri.to_file_path()
        .map(AbsPathBuf::assert)
        .map_err(|_| anyhow::anyhow!("invalid uri: {uri}"))
}

pub fn file_id(snap: &LspStateSnapshot, uri: &Url) -> anyhow::Result<FileId> {
    snap.vfs
        .read()
        .file_id(&VfsPath::PathBuf(file_path(uri)?))
        .ok_or_else(|| anyhow::anyhow!("unknown file: {uri}"))
}

pub fn offset(line_index: &LineIndex, pos: lsp_types::Position) -> TextSize {
    let line_col = LineCol {
        line: pos.line as u32,
        col: pos.character as u32,
    };

    line_index.offset(line_col)
}

pub fn text_range(line_index: &LineIndex, range: lsp_types::Range) -> TextRange {
    let start = offset(line_index, range.start);
    let end = offset(line_index, range.end);

    TextRange::new(start, end)
}

pub fn file_offset(snap: &LspStateSnapshot, tdpp: TextDocumentPositionParams) -> anyhow::Result<InFile<TextSize>> {
    let file_id = file_id(snap, &tdpp.text_document.uri)?;
    let line_index = snap.line_index(file_id)?;
    let offset = offset(&line_index, tdpp.position);

    Ok(InFile::new(file_id, offset))
}

pub fn lsp_range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_position(line_index, range.start()),
        end: lsp_position(line_index, range.end()),
    }
}

pub fn lsp_position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.line_col(offset);

    lsp_types::Position {
        line: line_col.line,
        character: line_col.col,
    }
}

pub fn uri(snapshot: &LspStateSnapshot, file: FileId) -> anyhow::Result<Url> {
    let vfs = snapshot.vfs.read();
    let path = vfs.file_path(file);

    uri_from_path(path.as_path().unwrap())
}

fn uri_from_path(path: impl AsRef<Path>) -> anyhow::Result<Url> {
    let component_has_windows_drive = path.as_ref().components().any(|comp| {
        if let Component::Prefix(c) = comp {
            match c.kind() {
                | Prefix::Disk(_) | Prefix::VerbatimDisk(_) => return true,
                | _ => return false,
            }
        }
        false
    });

    // VSCode expects drive letters to be lowercased, whereas rust will uppercase the drive letters.
    if component_has_windows_drive {
        let url_original = Url::from_file_path(&path)
            .map_err(|_| anyhow::anyhow!("can't convert path to url: {}", path.as_ref().display()))?;

        let drive_partition: Vec<&str> = url_original.as_str().rsplitn(2, ':').collect();

        // There is a drive partition, but we never found a colon.
        // This should not happen, but in this case we just pass it through.
        if drive_partition.len() == 1 {
            return Ok(url_original);
        }

        let joined = drive_partition[1].to_ascii_lowercase() + ":" + drive_partition[0];
        let url = Url::from_str(&joined).expect("This came from a valid `Url`");

        Ok(url)
    } else {
        Ok(Url::from_file_path(&path)
            .map_err(|_| anyhow::anyhow!("can't convert path to url: {}", path.as_ref().display()))?)
    }
}

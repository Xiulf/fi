use std::path::{Component, Path, PathBuf, Prefix};
use std::str::FromStr;

use base_db::input::{FileId, LineCol, LineIndex};
use lsp_types::Url;
use syntax::{TextRange, TextSize};

use crate::state::LspStateSnapshot;

pub fn file_path(uri: &Url) -> anyhow::Result<PathBuf> {
    uri.to_file_path().map_err(|_| anyhow::anyhow!("invalid uri: {uri}"))
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
    let path = vfs.file_path(vfs::FileId(file.0));

    uri_from_path(path)
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

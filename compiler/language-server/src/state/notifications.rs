use base_db::input::LineIndex;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, TextDocumentContentChangeEvent,
};
use vfs::VfsPath;

use super::LspState;
use crate::util;

pub enum Progress {
    Begin,
    Report,
    End,
}

impl LspState {
    pub fn on_did_open_text_document(&mut self, params: DidOpenTextDocumentParams) -> anyhow::Result<()> {
        let path = VfsPath::from(util::file_path(&params.text_document.uri)?);
        let (file_id, _) = self
            .vfs
            .write()
            .set_file_content(path, Some(params.text_document.text.into_bytes().into()));

        self.open_files.insert(file_id);

        Ok(())
    }

    pub fn on_did_close_text_document(&mut self, params: DidCloseTextDocumentParams) -> anyhow::Result<()> {
        let path = VfsPath::from(util::file_path(&params.text_document.uri)?);
        let file_id = self.vfs.read().file_id(&path).unwrap();

        self.open_files.remove(&file_id);

        Ok(())
    }

    pub fn on_did_change_text_document(&mut self, params: DidChangeTextDocumentParams) -> anyhow::Result<()> {
        let path = VfsPath::from(util::file_path(&params.text_document.uri)?);
        let mut vfs = self.vfs.write();
        let file_id = vfs.file_id(&path).unwrap();
        let mut text = vfs
            .file_content(file_id)
            .and_then(|c| String::from_utf8(c.to_vec()).ok())
            .unwrap();

        apply_document_changes(&mut text, params.content_changes);
        vfs.set_file_content(path, Some(text.into_bytes().into()));

        Ok(())
    }

    pub fn report_progress(
        &mut self,
        title: &str,
        state: Progress,
        message: Option<String>,
        fraction: Option<f64>,
    ) -> anyhow::Result<()> {
        let percentage = fraction.map(|f| {
            assert!((0.0..=1.0).contains(&f));
            (f * 100.0) as u32
        });

        let token = lsp_types::ProgressToken::String(format!("shade/{}", title));
        let progress = match state {
            | Progress::Begin => {
                self.send_request::<lsp_types::request::WorkDoneProgressCreate>(
                    lsp_types::WorkDoneProgressCreateParams { token: token.clone() },
                    |_, _| {},
                )?;

                lsp_types::WorkDoneProgress::Begin(lsp_types::WorkDoneProgressBegin {
                    title: title.into(),
                    cancellable: None,
                    message,
                    percentage,
                })
            },
            | Progress::Report => lsp_types::WorkDoneProgress::Report(lsp_types::WorkDoneProgressReport {
                cancellable: None,
                message,
                percentage,
            }),
            | Progress::End => lsp_types::WorkDoneProgress::End(lsp_types::WorkDoneProgressEnd { message }),
        };

        self.send_notification::<lsp_types::notification::Progress>(lsp_types::ProgressParams {
            token,
            value: lsp_types::ProgressParamsValue::WorkDone(progress),
        })
    }
}

fn apply_document_changes(old_text: &mut String, changes: Vec<TextDocumentContentChangeEvent>) {
    let mut line_index = LineIndex::new(old_text);

    enum IndexValid {
        All,
        UpToLine(u32),
    }

    impl IndexValid {
        fn covers(&self, line: u32) -> bool {
            match *self {
                | IndexValid::UpToLine(to) => to > line,
                | _ => true,
            }
        }
    }

    let mut index_valid = IndexValid::All;

    for change in changes {
        match change.range {
            | Some(range) => {
                if !index_valid.covers(range.end.line) {
                    line_index = LineIndex::new(old_text);
                }

                index_valid = IndexValid::UpToLine(range.start.line);
                let range = util::text_range(&line_index, range);
                old_text.replace_range(std::ops::Range::<usize>::from(range), &change.text);
            },
            | None => {
                *old_text = change.text;
                index_valid = IndexValid::UpToLine(0);
            },
        }
    }
}

mod server;

use lsp_server::Message;
use lsp_types::notification::{DidOpenTextDocument, Notification, PublishDiagnostics};
use lsp_types::{DidOpenTextDocumentParams, TextDocumentItem};
use server::Project;

#[test]
fn initialization() {
    let project = Project::new()
        .with_file(
            "fi.toml",
            r#"
[project]
name = "test"
version = "0.1.0"
output = "executable"

[dependencies]
prim = { path = "/mnt/e/Language/fc/lib/prim" }
"#,
        )
        .with_file(
            "src/main.fi",
            r#"
module Main =

main = ()
"#,
        );

    let _server = project.server().wait_until_workspace_loaded();
}

#[test]
fn diagnostics() {
    let project = Project::new()
        .with_file(
            "fi.toml",
            r#"
[project]
name = "test"
version = "0.1.0"
output = "executable"

[dependencies]
prim = { path = "/mnt/e/Language/fc/lib/prim" }
"#,
        )
        .with_file(
            "src/main.fi",
            r#"
module Main =

main = ()
"#,
        );

    let server = project.server().wait_until_workspace_loaded();

    server.notification::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: server.doc_id("src/main.fi").uri,
            language_id: "fi".into(),
            version: 1,
            text: r#"
module Main =

main = )
"#
            .into(),
        },
    });

    server.wait_for_message_cond(1, &|msg| match msg {
        | Message::Notification(n) if n.method == PublishDiagnostics::METHOD => {
            eprintln!("{}", n.params);
            true
        },
        | _ => false,
    });
}

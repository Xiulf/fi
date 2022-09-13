mod analysis;
mod db;
mod diagnostics;
mod state;
mod util;

#[cfg(test)]
mod tests;

use base_db::paths::AbsolutePathBuf;
use lsp_server::Connection;
use lsp_types::{ClientCapabilities, InitializeParams, ServerCapabilities};
use state::Config;

pub fn run() -> anyhow::Result<()> {
    tracing::info!("starting");
    let (connection, io_threads) = Connection::stdio();
    let config = init(&connection)?;

    main_loop(connection, config)?;

    io_threads.join()?;
    tracing::info!("shutdown");
    Ok(())
}

fn init(connection: &Connection) -> anyhow::Result<Config> {
    let (initialize_id, initialize_params) = connection.initialize_start()?;
    tracing::info!("InitializeParams: {}", initialize_params);
    let initialize_params = serde_json::from_value::<InitializeParams>(initialize_params)?;
    let initialize_result = lsp_types::InitializeResult {
        capabilities: server_capabilities(&initialize_params.capabilities),
        server_info: Some(lsp_types::ServerInfo {
            name: "shadow-lsp".into(),
            version: None,
        }),
    };

    let initialize_result = serde_json::to_value(initialize_result)?;

    connection.initialize_finish(initialize_id, initialize_result)?;
    config(&initialize_params)
}

fn config(params: &InitializeParams) -> anyhow::Result<Config> {
    let mut config = Config::default();
    let root_dir = match &params.root_uri {
        | Some(uri) => AbsolutePathBuf::try_from(uri.to_file_path().unwrap()).unwrap(),
        | None => AbsolutePathBuf::try_from(std::env::current_dir()?).unwrap(),
    };

    let workspaces = params
        .workspace_folders
        .as_ref()
        .map(|ws| {
            ws.iter()
                .filter_map(|it| it.uri.to_file_path().ok())
                .filter_map(|path| AbsolutePathBuf::try_from(path).ok())
                .collect::<Vec<_>>()
        })
        .filter(|ws| !ws.is_empty())
        .unwrap_or_else(|| vec![root_dir]);

    config.workspaces = state::workspace::discover_all(&workspaces);

    Ok(config)
}

fn main_loop(connection: Connection, config: Config) -> anyhow::Result<()> {
    state::LspState::new(connection.sender, config).run(connection.receiver)
}

fn server_capabilities(_client_args: &ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            lsp_types::TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(lsp_types::TextDocumentSyncKind::INCREMENTAL),
                will_save: None,
                will_save_wait_until: None,
                save: None,
            },
        )),
        ..Default::default()
    }
}

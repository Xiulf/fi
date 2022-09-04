mod state;

use lsp_server::Connection;
use lsp_types::{ClientCapabilities, InitializeParams, ServerCapabilities};

pub fn run() -> anyhow::Result<()> {
    tracing::info!("starting");
    let (connection, io_threads) = Connection::stdio();

    init(&connection)?;
    main_loop(connection)?;

    io_threads.join()?;
    tracing::info!("shutdown");
    Ok(())
}

fn init(connection: &Connection) -> anyhow::Result<()> {
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

    connection
        .initialize_finish(initialize_id, initialize_result)
        .map_err(Into::into)
}

fn main_loop(connection: Connection) -> anyhow::Result<()> {
    state::LspState::new(connection.sender).run(connection.receiver)
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

#[test]
fn test() -> anyhow::Result<()> {
    use lsp_server::Message::*;
    use lsp_server::Notification;
    let (connection, client) = Connection::memory();

    let server = std::thread::spawn(|| {
        init(&connection)?;
        main_loop(connection)
    });

    let mut queue = lsp_server::ReqQueue::<(), ()>::default();
    let req = queue
        .outgoing
        .register("initialize".into(), lsp_types::InitializeParams::default(), ());
    client.sender.send(Request(req))?;
    client.receiver.recv()?;
    client
        .sender
        .send(Notification(Notification::new("initialized".into(), ())))?;

    let req = queue.outgoing.register("shutdown".into(), (), ());
    client.sender.send(Request(req))?;

    server.join().unwrap()
}

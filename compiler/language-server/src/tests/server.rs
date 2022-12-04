use std::cell::{Cell, RefCell};
use std::fs;
use std::thread::JoinHandle;
use std::time::Duration;

use crossbeam_channel::{after, select};
use lsp_server::{Connection, Message, Notification, Request};
use lsp_types::notification::Exit;
use lsp_types::request::Shutdown;
use lsp_types::{ProgressParams, ProgressParamsValue, Url, WorkDoneProgress};
use paths::AbsPathBuf;
use serde_json::Value;

use crate::state::Config;

pub struct Project<'a> {
    files: Vec<(&'a str, &'a str)>,
}

impl<'a> Project<'a> {
    pub fn new() -> Self {
        Self { files: Vec::new() }
    }

    pub fn with_file(mut self, path: &'a str, text: &'a str) -> Self {
        self.files.push((path, text));
        self
    }

    pub fn server(self) -> Server {
        let tmp_dir = tempdir::TempDir::new("testdir").unwrap();

        for (path, text) in self.files {
            let path = tmp_dir.path().join(path);

            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, text).unwrap();
        }

        let tmp_dir_path: AbsPathBuf = tmp_dir.path().to_path_buf().try_into().unwrap();
        let roots = vec![tmp_dir_path.clone()];
        let workspaces = crate::state::workspace::discover_all(&roots);
        let config = Config {
            workspaces,
            ..Config::default()
        };

        Server::new(tmp_dir, config)
    }
}

pub struct Server {
    next_request_id: Cell<i32>,
    messages: RefCell<Vec<Message>>,
    worker: Option<JoinHandle<()>>,
    client: Connection,
    tmp_dir: tempdir::TempDir,
}

impl Server {
    pub fn new(tmp_dir: tempdir::TempDir, config: Config) -> Self {
        let (connection, client) = Connection::memory();
        let worker = std::thread::spawn(move || {
            crate::main_loop(connection, config).unwrap();
        });

        Self {
            next_request_id: Cell::new(1),
            messages: RefCell::new(Vec::new()),
            worker: Some(worker),
            client,
            tmp_dir,
        }
    }

    pub fn doc_id(&self, rel_path: &str) -> lsp_types::TextDocumentIdentifier {
        let path = self.tmp_dir.path().join(rel_path);

        lsp_types::TextDocumentIdentifier {
            uri: Url::from_file_path(path).unwrap(),
        }
    }

    pub fn wait_until_workspace_loaded(self) -> Server {
        self.wait_for_message_cond(1, &|msg: &Message| match msg {
            | Message::Notification(n) if n.method == "$/progress" => {
                matches!(n.clone().extract::<ProgressParams>("$/progress").unwrap(), ProgressParams {
                    token: lsp_types::ProgressToken::String(ref token),
                    value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(_)),
                } if token == "shade/projects scanned")
            },
            | _ => false,
        });

        self
    }

    pub fn wait_for_message_cond(&self, n: usize, cond: &dyn Fn(&Message) -> bool) {
        let mut total = 0;

        for msg in self.messages.borrow().iter() {
            if cond(msg) {
                total += 1;
            }
        }

        while total < n {
            let msg = self.recv().expect("no response");

            if cond(&msg) {
                total += 1;
            }
        }
    }

    pub fn request<R: lsp_types::request::Request>(&self, params: R::Params) -> R::Result {
        let value = self.send_request_for_value::<R>(params);
        serde_json::from_value(value).unwrap()
    }

    pub fn assert_request_returns_value<R: lsp_types::request::Request>(
        &self,
        params: R::Params,
        expected_response: Value,
    ) {
        let result = self.send_request_for_value::<R>(params);
        assert_eq!(result, expected_response);
    }

    fn send_request_for_value<R: lsp_types::request::Request>(&self, params: R::Params) -> Value {
        let id = self.next_request_id.get();
        self.next_request_id.set(id.wrapping_add(1));

        let r = Request::new(id.into(), R::METHOD.into(), params);
        self.send_and_receive(r)
    }

    pub fn notification<N: lsp_types::notification::Notification>(&self, params: N::Params) {
        let n = Notification::new(N::METHOD.into(), params);
        self.send_notification(n);
    }

    fn send_notification(&self, n: Notification) {
        self.client.sender.send(Message::Notification(n)).unwrap();
    }

    fn send_and_receive(&self, r: Request) -> Value {
        let id = r.id.clone();
        self.client.sender.send(Message::Request(r)).unwrap();

        while let Some(msg) = self.recv() {
            match msg {
                | Message::Request(req) => panic!("did not expect a request as a responseto a request: {:?}", req),
                | Message::Notification(_) => {},
                | Message::Response(res) => {
                    assert_eq!(res.id, id);

                    if let Some(err) = res.error {
                        panic!("received error response as a response to a request: {:?}", err);
                    }

                    return res.result.unwrap();
                },
            }
        }

        panic!("did not receive a response to our request");
    }

    fn recv(&self) -> Option<Message> {
        let timeout = Duration::from_secs(120);
        let msg = select! {
            recv(self.client.receiver) -> msg => msg.ok(),
            recv(after(timeout)) -> _ => panic!("timed out"),
        };

        if let Some(ref msg) = msg {
            self.messages.borrow_mut().push(msg.clone());
        }

        msg
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        self.assert_request_returns_value::<Shutdown>((), Value::Null);
        self.notification::<Exit>(());

        if let Some(worker) = self.worker.take() {
            worker.join().unwrap();
        }
    }
}

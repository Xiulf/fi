mod dispatcher;
mod notifications;
pub mod workspace;

use std::sync::Arc;

use base_db::input::SourceRoot;
use crossbeam_channel::{select, unbounded, Receiver, Sender};
use dispatcher::NotificationDispatcher;
use lsp_server::{ErrorCode, Message, Notification, ReqQueue, Request, Response};
use lsp_types::notification::{
    self, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
};
use lsp_types::request::Shutdown;
use parking_lot::RwLock;
use paths::AbsPathBuf;
use project::Workspace;
use rustc_hash::FxHashSet;
use threadpool::ThreadPool;
use vfs::file_set::FileSetConfig;
use vfs::{FileId, VirtualFileSystem};

use self::dispatcher::RequestDispatcher;
use crate::analysis::{Analysis, AnalysisChange, AnalysisSnapshot};

#[derive(Debug)]
pub enum Task {
    Response(Response),
    Notify(Notification),
}

#[derive(Debug)]
pub enum Event {
    Task(Task),
    Lsp(Message),
}

pub type RequestHandler = fn(&mut LspState, Response);

pub struct LspState {
    pub config: Config,
    pub sender: Sender<Message>,
    pub request_queue: ReqQueue<(), RequestHandler>,
    pub thread_pool: ThreadPool,
    pub task_sender: Sender<Task>,
    pub task_receiver: Receiver<Task>,
    pub analysis: Analysis,
    pub open_files: FxHashSet<FileId>,
    pub workspaces: Arc<Vec<Workspace>>,
    pub file_sets: FileSetConfig,
    pub vfs: Arc<RwLock<VirtualFileSystem>>,
    pub shutdown_requested: bool,
}

pub struct LspStateSnapshot {
    pub analysis: AnalysisSnapshot,
    pub vfs: Arc<RwLock<VirtualFileSystem>>,
    pub workspaces: Arc<Vec<Workspace>>,
}

#[derive(Default)]
pub struct Config {
    pub workspaces: Vec<AbsPathBuf>,
}

impl LspState {
    pub fn new(sender: Sender<Message>, config: Config) -> Self {
        let (task_sender, task_receiver) = unbounded();

        Self {
            config,
            sender,
            task_sender,
            task_receiver,
            analysis: Default::default(),
            open_files: Default::default(),
            workspaces: Default::default(),
            file_sets: Default::default(),
            vfs: Default::default(),
            thread_pool: Default::default(),
            request_queue: Default::default(),
            shutdown_requested: false,
        }
    }

    pub fn snapshot(&self) -> LspStateSnapshot {
        LspStateSnapshot {
            analysis: self.analysis.snapshot(),
            vfs: self.vfs.clone(),
            workspaces: self.workspaces.clone(),
        }
    }

    pub fn run(mut self, receiver: Receiver<Message>) -> anyhow::Result<()> {
        self.fetch_workspaces()?;

        while let Some(event) = self.next_event(&receiver) {
            if let Event::Lsp(Message::Notification(not)) = &event {
                if not.method == notification::Exit::METHOD {
                    return Ok(());
                }
            }

            self.handle_event(event)?;
        }

        Ok(())
    }

    fn next_event(&self, receiver: &Receiver<Message>) -> Option<Event> {
        select! {
            recv(receiver) -> msg => msg.ok().map(Event::Lsp),
            recv(self.task_receiver) -> task => Some(Event::Task(task.unwrap())),
        }
    }

    fn handle_event(&mut self, event: Event) -> anyhow::Result<()> {
        match event {
            | Event::Task(task) => self.handle_task(task)?,
            | Event::Lsp(msg) => match msg {
                | Message::Request(req) => self.handle_request(req)?,
                | Message::Response(res) => self.complete_request(res),
                | Message::Notification(not) => self.handle_notification(not)?,
            },
        }

        let state_changed = self.process_vfs_changes();

        if state_changed {
            self.publish_diagnostics();
        }

        Ok(())
    }

    fn handle_task(&mut self, task: Task) -> anyhow::Result<()> {
        match task {
            | Task::Notify(not) => self.send(not.into()),
            | Task::Response(res) => self.respond(res),
        }
    }

    fn handle_request(&mut self, req: Request) -> anyhow::Result<()> {
        self.register_request(&req);

        if self.shutdown_requested {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::InvalidRequest as i32,
                "shutdown was requested".into(),
            ));
        }

        RequestDispatcher::new(self, req)
            .on_sync::<Shutdown>(|state, _| {
                state.shutdown_requested = true;
                Ok(())
            })?
            .finish();
        Ok(())
    }

    fn register_request(&mut self, req: &Request) {
        self.request_queue.incoming.register(req.id.clone(), ());
    }

    fn complete_request(&mut self, res: Response) {
        let handler = self.request_queue.outgoing.complete(res.id.clone()).unwrap();

        handler(self, res);
    }

    fn handle_notification(&mut self, not: Notification) -> anyhow::Result<()> {
        NotificationDispatcher::new(self, not)
            .on::<DidOpenTextDocument>(Self::on_did_open_text_document)?
            .on::<DidCloseTextDocument>(Self::on_did_close_text_document)?
            .on::<DidChangeTextDocument>(Self::on_did_change_text_document)?
            .finish();
        Ok(())
    }

    fn send(&mut self, msg: Message) -> anyhow::Result<()> {
        self.sender.send(msg)?;
        Ok(())
    }

    fn send_request<R: lsp_types::request::Request>(
        &mut self,
        params: R::Params,
        handler: RequestHandler,
    ) -> anyhow::Result<()> {
        let req = self.request_queue.outgoing.register(R::METHOD.into(), params, handler);

        self.send(Message::Request(req))
    }

    fn send_notification<N: lsp_types::notification::Notification>(&mut self, params: N::Params) -> anyhow::Result<()> {
        let not = Notification::new(N::METHOD.into(), params);
        self.send(Message::Notification(not))
    }

    pub fn respond(&mut self, res: Response) -> anyhow::Result<()> {
        if let Some(()) = self.request_queue.incoming.complete(res.id.clone()) {
            self.send(res.into()).unwrap();
        }

        Ok(())
    }

    fn process_vfs_changes(&mut self) -> bool {
        let changed_files = self.vfs.write().take_changes();

        if changed_files.is_empty() {
            return false;
        }

        let vfs = self.vfs.read();
        let mut change = AnalysisChange::default();
        let mut has_created_or_deleted_entries = false;

        for file in changed_files {
            if file.is_created_or_deleted() {
                has_created_or_deleted_entries = true;
            }

            let bytes = vfs.file_content(file.file_id).map(Vec::from).unwrap_or_default();
            let text = String::from_utf8(bytes).ok().map(Arc::from);

            change.change_file(base_db::input::FileId(file.file_id.0), text);
        }

        if has_created_or_deleted_entries {
            let file_sets = self.file_sets.partition(&vfs);
            let roots = file_sets.into_iter().map(SourceRoot::new).collect();

            change.set_roots(roots);
        }

        self.analysis.apply_change(change);
        true
    }
}

impl Drop for LspState {
    fn drop(&mut self) {
        self.thread_pool.join();
    }
}

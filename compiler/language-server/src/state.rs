use std::time::Instant;

use crossbeam_channel::{select, unbounded, Receiver, Sender};
use lsp_server::{Message, Notification, ReqQueue, Request, Response};
use lsp_types::notification::{self, Notification as _};
use threadpool::ThreadPool;

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
    pub sender: Sender<Message>,
    pub request_queue: ReqQueue<(), RequestHandler>,
    pub thread_pool: ThreadPool,
    pub task_sender: Sender<Task>,
    pub task_receiver: Receiver<Task>,
    pub shutdown_requested: bool,
}

impl LspState {
    pub fn new(sender: Sender<Message>) -> Self {
        let (task_sender, task_receiver) = unbounded();

        Self {
            sender,
            task_sender,
            task_receiver,
            thread_pool: ThreadPool::default(),
            request_queue: ReqQueue::default(),
            shutdown_requested: false,
        }
    }

    pub fn run(mut self, receiver: Receiver<Message>) -> anyhow::Result<()> {
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
        let start_time = Instant::now();

        match event {
            | Event::Task(task) => self.handle_task(task)?,
            | Event::Lsp(msg) => match msg {
                | Message::Request(req) => self.handle_request(req, start_time)?,
                | Message::Response(res) => self.complete_request(res),
                | Message::Notification(not) => self.handle_notification(not)?,
            },
        }

        Ok(())
    }

    fn handle_task(&mut self, task: Task) -> anyhow::Result<()> {
        match task {
            | Task::Notify(not) => self.send(not.into()),
            | Task::Response(res) => self.respond(res),
        }
    }

    fn handle_request(&mut self, req: Request, time: Instant) -> anyhow::Result<()> {
        Ok(())
    }

    fn complete_request(&mut self, res: Response) {
        let handler = self.request_queue.outgoing.complete(res.id.clone()).unwrap();

        handler(self, res);
    }

    fn handle_notification(&mut self, not: Notification) -> anyhow::Result<()> {
        Ok(())
    }

    fn send(&mut self, msg: Message) -> anyhow::Result<()> {
        self.sender.send(msg)?;
        Ok(())
    }

    fn respond(&mut self, res: Response) -> anyhow::Result<()> {
        if let Some(()) = self.request_queue.incoming.complete(res.id.clone()) {
            self.send(res.into());
        }

        Ok(())
    }
}

impl Drop for LspState {
    fn drop(&mut self) {
        self.thread_pool.join();
    }
}

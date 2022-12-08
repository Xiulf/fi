use base_db::Cancelled;
use lsp_server::{ErrorCode, ExtractError, Notification, Request, RequestId, Response};
use serde::de::DeserializeOwned;
use serde::Serialize;

use crate::state::{LspState, LspStateSnapshot, Task};

pub struct RequestDispatcher<'a> {
    state: &'a mut LspState,
    request: Option<Request>,
}

pub struct NotificationDispatcher<'a> {
    state: &'a mut LspState,
    notification: Option<Notification>,
}

impl<'a> RequestDispatcher<'a> {
    pub fn new(state: &'a mut LspState, request: Request) -> Self {
        Self {
            state,
            request: Some(request),
        }
    }

    pub fn on_sync<R>(
        &mut self,
        res: fn(&mut LspState, R::Params) -> anyhow::Result<R::Result>,
    ) -> anyhow::Result<&mut Self>
    where
        R: lsp_types::request::Request + 'static,
        R::Params: DeserializeOwned + 'static,
        R::Result: Serialize + 'static,
    {
        let (id, params) = match self.parse::<R>() {
            | Some(it) => it,
            | None => return Ok(self),
        };

        let res = res(self.state, params);
        let res = result_to_response::<R>(id, res);

        self.state.respond(res)?;
        Ok(self)
    }

    pub fn on<R>(
        &mut self,
        res: fn(LspStateSnapshot, R::Params) -> anyhow::Result<R::Result>,
    ) -> anyhow::Result<&mut Self>
    where
        R: lsp_types::request::Request + 'static,
        R::Params: DeserializeOwned + Send + 'static,
        R::Result: Serialize + 'static,
    {
        let (id, params) = match self.parse::<R>() {
            | Some(it) => it,
            | None => return Ok(self),
        };

        self.state.thread_pool.execute({
            let snapshot = self.state.snapshot();
            let sender = self.state.task_sender.clone();

            move || {
                let res = res(snapshot, params);
                let res = result_to_response::<R>(id, res);

                sender.send(Task::Response(res)).unwrap();
            }
        });

        Ok(self)
    }

    pub fn finish(&mut self) {
        if let Some(req) = self.request.take() {
            tracing::error!("unknown request: {:?}", req);
            let res = Response::new_err(req.id, ErrorCode::MethodNotFound as i32, "unknown request".into());

            self.state.respond(res).unwrap();
        }
    }

    fn parse<R>(&mut self) -> Option<(RequestId, R::Params)>
    where
        R: lsp_types::request::Request + 'static,
        R::Params: DeserializeOwned + 'static,
    {
        let req = match &self.request {
            | Some(req) if req.method == R::METHOD => self.request.take().unwrap(),
            | _ => return None,
        };

        match serde_json::from_value(req.params) {
            | Ok(params) => Some((req.id, params)),
            | Err(err) => {
                let res = Response::new_err(req.id, ErrorCode::InvalidParams as i32, err.to_string());

                self.state.respond(res).ok()?;
                None
            },
        }
    }
}

impl<'a> NotificationDispatcher<'a> {
    pub fn new(state: &'a mut LspState, notification: Notification) -> Self {
        Self {
            state,
            notification: Some(notification),
        }
    }

    pub fn on<N>(&mut self, res: fn(&mut LspState, N::Params) -> anyhow::Result<()>) -> anyhow::Result<&mut Self>
    where
        N: lsp_types::notification::Notification + 'static,
        N::Params: DeserializeOwned + Send + 'static,
    {
        let notification = match self.notification.take() {
            | Some(it) => it,
            | None => return Ok(self),
        };

        let params = match notification.extract::<N::Params>(N::METHOD) {
            | Ok(it) => it,
            | Err(ExtractError::JsonError { method, error }) => {
                panic!("Invalid request\nMethod {method}\n error: {error}");
            },
            | Err(ExtractError::MethodMismatch(not)) => {
                self.notification = Some(not);
                return Ok(self);
            },
        };

        res(self.state, params)?;
        Ok(self)
    }

    pub fn finish(&mut self) {
        if let Some(not) = self.notification.take() {
            if !not.method.starts_with("$/") {
                tracing::error!("unhandled notification: {:?}", not);
            }
        }
    }
}

fn result_to_response<R>(id: RequestId, result: anyhow::Result<R::Result>) -> Response
where
    R: lsp_types::request::Request + 'static,
    R::Params: DeserializeOwned + 'static,
    R::Result: Serialize + 'static,
{
    match result {
        | Ok(res) => Response::new_ok(id, &res),
        | Err(e) if e.downcast_ref::<Cancelled>().is_some() => {
            Response::new_err(id, ErrorCode::ContentModified as i32, "content modified".to_string())
        },
        | Err(e) => Response::new_err(id, ErrorCode::InternalError as i32, e.to_string()),
    }
}

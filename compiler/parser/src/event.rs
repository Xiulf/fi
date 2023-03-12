use crate::error::{ParseError, SyntaxError};
use crate::token::SyntaxKind;
use crate::TreeSink;

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize)]
pub enum Event {
    Token(SyntaxKind),
    Start(SyntaxKind, Option<usize>),
    Finish,
    Error(ParseError),
}

impl Event {
    pub fn tombstone() -> Self {
        Self::Start(SyntaxKind::TOMBSTONE, None)
    }
}

pub fn process(sink: &mut dyn TreeSink, mut events: Vec<Event>) {
    let mut forward_parents = Vec::new();

    for i in 0..events.len() {
        match std::mem::replace(&mut events[i], Event::tombstone()) {
            | Event::Start(SyntaxKind::TOMBSTONE, _) => {},
            | Event::Start(kind, forward_parent) => {
                let mut idx = i;
                let mut fp = forward_parent;

                forward_parents.push(kind);

                while let Some(fwd) = fp {
                    idx += fwd;
                    fp = match std::mem::replace(&mut events[idx], Event::tombstone()) {
                        | Event::Start(kind, forward_parent) => {
                            if kind != SyntaxKind::TOMBSTONE {
                                forward_parents.push(kind);
                            }

                            forward_parent
                        },
                        | _ => unreachable!(),
                    };
                }

                for kind in forward_parents.drain(..).rev() {
                    sink.start_node(kind);
                }
            },
            | Event::Finish => sink.finish_node(),
            | Event::Token(kind) => sink.token(kind),
            | Event::Error(error) => sink.error(SyntaxError::ParseError(error)),
        }
    }
}

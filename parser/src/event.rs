use crate::syntax_kind::SyntaxKind;
use crate::ParseError;
use crate::TreeSink;
use std::mem;

pub enum Event {
    Start { kind: SyntaxKind, forward_parent: Option<u32> },
    Finish,
    Token { kind: SyntaxKind },
    Error { msg: ParseError },
}

impl Event {
    crate fn tombstone() -> Self {
        Event::Start {
            kind: SyntaxKind::TOMBSTONE,
            forward_parent: None,
        }
    }
}

crate fn process(sink: &mut dyn TreeSink, mut events: Vec<Event>) {
    let mut forward_parents = Vec::new();

    for i in 0..events.len() {
        match mem::replace(&mut events[i], Event::tombstone()) {
            | Event::Start {
                kind: SyntaxKind::TOMBSTONE, ..
            } => {},
            | Event::Start { kind, forward_parent } => {
                forward_parents.push(kind);

                let mut idx = i;
                let mut fp = forward_parent;

                while let Some(fwd) = fp {
                    idx += fwd as usize;
                    fp = match mem::replace(&mut events[idx], Event::tombstone()) {
                        | Event::Start { kind, forward_parent } => {
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
            | Event::Token { kind } => sink.token(kind),
            | Event::Error { msg } => sink.error(msg),
        }
    }
}

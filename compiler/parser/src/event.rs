use text_size::TextSize;

use crate::token::SyntaxKind;
use crate::TreeSink;

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Event {
    Token(SyntaxKind, TextSize),
    Start(SyntaxKind),
    Finish,
    Empty,
    Node(Box<[Event]>),
}

pub fn process(sink: &mut dyn TreeSink, event: Event) {
    match event {
        | Event::Token(kind, len) => sink.token(kind, len),
        | Event::Start(kind) => sink.start_node(kind),
        | Event::Finish => sink.finish_node(),
        | Event::Empty => {},
        | Event::Node(events) => {
            for event in events.into_vec() {
                process(sink, event);
            }
        },
    }
}

impl From<(Self, Self)> for Event {
    fn from((a, b): (Self, Self)) -> Self {
        Self::Node(Box::new([a, b]))
    }
}

impl From<((Self, Self), Self)> for Event {
    fn from(((a, b), c): ((Self, Self), Self)) -> Self {
        Self::Node(Box::new([a, b, c]))
    }
}

impl From<(((Self, Self), Self), Self)> for Event {
    fn from((((a, b), c), d): (((Self, Self), Self), Self)) -> Self {
        Self::Node(Box::new([a, b, c, d]))
    }
}

impl From<((((Self, Self), Self), Self), Self)> for Event {
    fn from(((((a, b), c), d), e): ((((Self, Self), Self), Self), Self)) -> Self {
        Self::Node(Box::new([a, b, c, d, e]))
    }
}

impl From<(((((Self, Self), Self), Self), Self), Self)> for Event {
    fn from((((((a, b), c), d), e), f): (((((Self, Self), Self), Self), Self), Self)) -> Self {
        Self::Node(Box::new([a, b, c, d, e, f]))
    }
}

impl FromIterator<Self> for Event {
    fn from_iter<T: IntoIterator<Item = Self>>(iter: T) -> Self {
        let events = <Box<[Event]>>::from_iter(iter);

        if events.is_empty() {
            Self::Empty
        } else {
            Self::Node(events)
        }
    }
}

impl FromIterator<(Self, Self)> for Event {
    fn from_iter<T: IntoIterator<Item = (Self, Self)>>(iter: T) -> Self {
        let iter = iter.into_iter().flat_map(|(a, b)| [a, b]);
        let events = <Box<[Self]>>::from_iter(iter);

        if events.is_empty() {
            Self::Empty
        } else {
            Self::Node(events)
        }
    }
}

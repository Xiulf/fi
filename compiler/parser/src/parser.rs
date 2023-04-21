use text_size::{TextRange, TextSize};

use crate::error::ParseError;
use crate::event::Event;
use crate::token::SyntaxKind;
use crate::token_set::TokenSet;

pub struct Parser<'input> {
    tokens: &'input [(SyntaxKind, TextRange)],
    pos: usize,
    last_offset: TextSize,
    events: Vec<Event>,
}

pub struct Marker {
    pos: usize,
    complete: bool,
}

pub struct CompletedMarker {
    start: usize,
    end: usize,
    kind: SyntaxKind,
}

pub struct Checkpoint {
    pos: usize,
    events: usize,
}

impl<'input> Parser<'input> {
    pub fn new(tokens: &'input [(SyntaxKind, TextRange)]) -> Self {
        Self {
            tokens,
            pos: 0,
            last_offset: TextSize::from(0),
            events: Vec::new(),
        }
    }

    pub fn finish(self) -> Vec<Event> {
        self.events
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    pub fn current(&self) -> Option<SyntaxKind> {
        self.nth(0)
    }

    pub fn nth(&self, n: usize) -> Option<SyntaxKind> {
        self.tokens.get(self.pos + n).map(|t| t.0)
    }

    pub fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub fn at_ts(&self, set: TokenSet) -> bool {
        self.nth_at_ts(0, set)
    }

    pub fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.tokens.get(self.pos + n).map_or(false, |t| t.0 == kind)
    }

    pub fn nth_at_ts(&self, n: usize, set: TokenSet) -> bool {
        self.tokens.get(self.pos + n).map_or(false, |t| set.contains(t.0))
    }

    pub fn is_immediate(&self) -> bool {
        self.tokens
            .get(self.pos)
            .map_or(false, |t| t.1.start() == self.last_offset)
    }

    pub fn nth_immediate(&self, n: usize) -> bool {
        if n == 0 {
            return self.is_immediate();
        }

        let last = match self.tokens.get(self.pos + n - 1) {
            | Some(t) => t.1.end(),
            | None => return false,
        };

        self.tokens.get(self.pos + n).map_or(false, |t| t.1.start() == last)
    }

    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.do_bump(kind);
            true
        } else {
            false
        }
    }

    pub fn eat_ts(&mut self, set: TokenSet) -> bool {
        if self.at_ts(set) {
            self.bump_any();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            true
        } else {
            self.error([kind]);
            false
        }
    }

    pub fn expect_ts(&mut self, set: TokenSet) -> bool {
        if self.eat_ts(set) {
            true
        } else {
            self.error(set);
            false
        }
    }

    pub fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind));
    }

    pub fn bump_any(&mut self) {
        if let Some(current) = self.current() {
            self.do_bump(current);
        }
    }

    pub fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.push_event(Event::tombstone());
        Marker::new(pos)
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint::new(self.pos, self.events.len())
    }

    pub fn error(&mut self, expected: impl IntoIterator<Item = SyntaxKind>) {
        let range = self.tokens.get(self.pos).or_else(|| self.tokens.last()).unwrap().1;
        let error = ParseError::new(range, self.current(), expected);
        self.push_event(Event::Error(error));
    }

    fn do_bump(&mut self, kind: SyntaxKind) {
        self.last_offset = self.tokens[self.pos].1.end();
        self.pos += 1;
        self.push_event(Event::Token(kind));
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }
}

impl Marker {
    fn new(pos: usize) -> Self {
        Self { pos, complete: false }
    }

    pub fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.complete = true;

        match &mut p.events[self.pos] {
            | Event::Start(slot, _) => {
                *slot = kind;
            },
            | _ => unreachable!(),
        }

        let finish_pos = p.events.len();

        p.push_event(Event::Finish);

        CompletedMarker::new(self.pos, finish_pos, kind)
    }

    pub fn abandon(mut self, p: &mut Parser) {
        self.complete = true;

        if self.pos == p.events.len() - 1 {
            match p.events.pop() {
                | Some(Event::Start(SyntaxKind::TOMBSTONE, None)) => {},
                | _ => unreachable!(),
            }
        }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.complete {
            panic!("uncompleted marker at {}", self.pos);
        }
    }
}

impl CompletedMarker {
    fn new(start: usize, end: usize, kind: SyntaxKind) -> Self {
        Self { start, end, kind }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn precede(self, p: &mut Parser) -> Marker {
        let new_pos = p.start();

        match &mut p.events[self.start] {
            | Event::Start(_, forward_parent) => {
                *forward_parent = Some(new_pos.pos - self.start);
            },
            | _ => unreachable!(),
        }

        new_pos
    }

    pub fn undo_completion(self, p: &mut Parser) -> Marker {
        match &mut p.events[self.start] {
            | Event::Start(kind, None) => {
                *kind = SyntaxKind::TOMBSTONE;
            },
            | _ => unreachable!(),
        }

        match &mut p.events[self.end] {
            | slot @ Event::Finish => *slot = Event::tombstone(),
            | _ => unreachable!(),
        }

        Marker::new(self.start)
    }
}

impl Checkpoint {
    fn new(pos: usize, events: usize) -> Self {
        Self { pos, events }
    }

    pub fn restore(self, p: &mut Parser) {
        p.events.truncate(self.events);
        p.pos = self.pos;
    }
}

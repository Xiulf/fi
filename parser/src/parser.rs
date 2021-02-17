use crate::event::Event;
use crate::token_set::TokenSet;
use crate::SyntaxKind;
use crate::{ParseError, TokenSource};
use std::cell::Cell;

crate struct Parser<'t> {
    token_source: &'t mut dyn TokenSource,
    events: Vec<Event>,
    steps: Cell<u32>,
}

impl<'t> Parser<'t> {
    crate fn new(token_source: &'t mut dyn TokenSource) -> Self {
        Parser {
            token_source,
            events: Vec::new(),
            steps: Cell::new(0),
        }
    }

    crate fn finish(self) -> Vec<Event> {
        self.events
    }

    crate fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    crate fn nth(&self, n: usize) -> SyntaxKind {
        let steps = self.steps.get();

        self.steps.set(steps + 1);
        self.token_source.lookahead_nth(n).kind
    }

    crate fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    crate fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.token_source.lookahead_nth(n).kind == kind
    }

    crate fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.do_bump(kind);
            true
        } else {
            return false;
        }
    }

    crate fn at_ts(&self, kinds: TokenSet) -> bool {
        kinds.contains(self.current())
    }

    crate fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;

        self.push_event(Event::tombstone());
        Marker::new(pos)
    }

    crate fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind));
    }

    crate fn bump_any(&mut self) {
        self.do_bump(self.current());
    }

    crate fn error(&mut self, msg: impl Into<String>) {
        let msg = ParseError(msg.into().into());

        self.push_event(Event::Error { msg });
    }

    crate fn err_recover(&mut self, msg: impl Into<String>, recovery: TokenSet) {
        if self.at_ts(recovery) {
            self.error(msg);
        } else {
            let m = self.start();

            self.error(msg);
            self.bump_any();
            m.complete(self, SyntaxKind::ERROR);
        }
    }

    crate fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        } else {
            self.error(format!("expected {:?}", kind));
            false
        }
    }

    fn do_bump(&mut self, kind: SyntaxKind) {
        self.token_source.bump();
        self.push_event(Event::Token { kind });
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }
}

crate struct Marker {
    pos: u32,
    complete: bool,
}

impl Marker {
    fn new(pos: u32) -> Self {
        Marker { pos, complete: false }
    }

    crate fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.complete = true;

        let idx = self.pos as usize;

        match &mut p.events[idx] {
            | Event::Start { kind: slot, .. } => {
                *slot = kind;
            },
            | _ => unreachable!(),
        }

        let finish_pos = p.events.len() as u32;

        p.push_event(Event::Finish);

        CompletedMarker::new(self.pos, finish_pos, kind)
    }

    crate fn abandon(mut self, p: &mut Parser) {
        self.complete = true;

        let idx = self.pos as usize;

        if idx == p.events.len() - 1 {
            match p.events.pop() {
                | Some(Event::Start {
                    kind: SyntaxKind::TOMBSTONE,
                    forward_parent: None,
                }) => {},
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

crate struct CompletedMarker {
    start: u32,
    end: u32,
    kind: SyntaxKind,
}

impl CompletedMarker {
    fn new(start: u32, end: u32, kind: SyntaxKind) -> Self {
        CompletedMarker { start, end, kind }
    }

    crate fn precede(self, p: &mut Parser) -> Marker {
        let new_pos = p.start();
        let idx = self.start as usize;

        match &mut p.events[idx] {
            | Event::Start { forward_parent, .. } => {
                *forward_parent = Some(new_pos.pos - self.start);
            },
            | _ => unreachable!(),
        }

        new_pos
    }

    crate fn undo_completion(self, p: &mut Parser) -> Marker {
        let start = self.start as usize;
        let end = self.end as usize;

        match &mut p.events[start] {
            | Event::Start { kind, forward_parent: None } => *kind = SyntaxKind::TOMBSTONE,
            | _ => unreachable!(),
        }

        match &mut p.events[end] {
            | slot @ Event::Finish => *slot = Event::tombstone(),
            | _ => unreachable!(),
        }

        Marker::new(self.start)
    }

    crate fn kind(&self) -> SyntaxKind {
        self.kind
    }
}

use std::mem;

use parser::syntax_kind::SyntaxKind;
use parser::TreeSink;
use rowan::{TextRange, TextSize};

use crate::parsing::lexer::Token;
use crate::{SyntaxError, SyntaxTreeBuilder};

pub(crate) struct TextTreeSink<'t> {
    text: &'t str,
    tokens: &'t [Token],
    text_pos: TextSize,
    token_pos: usize,
    state: State,
    inner: SyntaxTreeBuilder<'t>,
}

enum State {
    PendingStart,
    Normal,
    PendingFinish,
}

impl<'t> TreeSink for TextTreeSink<'t> {
    fn token(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            | State::PendingStart => unreachable!(),
            | State::PendingFinish => self.inner.finish_node(),
            | State::Normal => {},
        }

        self.eat_trivia();

        let len = self.tokens[self.token_pos].len;

        self.do_token(kind, len);
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        match mem::replace(&mut self.state, State::Normal) {
            | State::PendingStart => {
                self.inner.start_node(kind);
                return;
            },
            | State::PendingFinish => self.inner.finish_node(),
            | State::Normal => {},
        }

        let n_trivias = self.tokens[self.token_pos..]
            .iter()
            .take_while(|it| it.kind.is_trivia())
            .count();
        let leading_trivias = &self.tokens[self.token_pos..self.token_pos + n_trivias];
        let mut trivia_end = self.text_pos + leading_trivias.iter().map(|it| it.len).sum::<TextSize>();

        let n_attached_trivias = {
            let leading_trivias = leading_trivias.iter().rev().map(|it| {
                let next_end = trivia_end - it.len;
                let range = TextRange::new(next_end, trivia_end);

                trivia_end = next_end;
                (it.kind, &self.text[range])
            });

            n_attached_trivias(kind, leading_trivias)
        };

        self.eat_n_trivia(n_trivias - n_attached_trivias);
        self.inner.start_node(kind);
        self.eat_n_trivia(n_attached_trivias);
    }

    fn finish_node(&mut self) {
        match mem::replace(&mut self.state, State::PendingFinish) {
            | State::PendingStart => unreachable!(),
            | State::PendingFinish => self.inner.finish_node(),
            | State::Normal => {},
        }
    }

    fn error(&mut self, error: parser::ParseError) {
        self.inner.error(error, self.text_pos);
    }
}

impl<'t> TextTreeSink<'t> {
    pub(crate) fn new(text: &'t str, tokens: &'t [Token]) -> Self {
        TextTreeSink {
            text,
            tokens,
            text_pos: 0.into(),
            token_pos: 0,
            state: State::PendingStart,
            inner: SyntaxTreeBuilder::new(),
        }
    }

    pub(crate) fn finish(mut self) -> (rowan::GreenNode, Vec<SyntaxError>) {
        match mem::replace(&mut self.state, State::Normal) {
            | State::PendingFinish => {
                self.eat_trivia();
                self.inner.finish_node();
            },
            | State::PendingStart | State::Normal => unreachable!(),
        }

        self.inner.finish_raw()
    }

    fn eat_trivia(&mut self) {
        while let Some(&token) = self.tokens.get(self.token_pos) {
            if !token.kind.is_trivia() {
                break;
            }

            self.do_token(token.kind, token.len);
        }
    }

    fn eat_n_trivia(&mut self, n: usize) {
        for _ in 0..n {
            let token = self.tokens[self.token_pos];
            assert!(token.kind.is_trivia());
            self.do_token(token.kind, token.len);
        }
    }

    fn do_token(&mut self, kind: SyntaxKind, len: TextSize) {
        let range = TextRange::at(self.text_pos, len);
        let text = &self.text[range];

        self.text_pos += len;
        self.token_pos += 1;
        self.inner.token(kind, text);
    }
}

fn n_attached_trivias<'t>(kind: SyntaxKind, trivias: impl Iterator<Item = (SyntaxKind, &'t str)>) -> usize {
    match kind {
        | SyntaxKind::IMPORT
        | SyntaxKind::ITEM_FIXITY
        | SyntaxKind::ITEM_FUN
        | SyntaxKind::ITEM_STATIC
        | SyntaxKind::ITEM_CONST
        | SyntaxKind::ITEM_TYPE
        | SyntaxKind::ITEM_CLASS
        | SyntaxKind::ITEM_MEMBER => {
            let mut res = 0;
            let mut trivias = trivias.enumerate().peekable();

            while let Some((i, (kind, text))) = trivias.next() {
                match kind {
                    | SyntaxKind::WHITESPACE if text.contains("\n\n") => {
                        if let Some((SyntaxKind::COMMENT, peek_text)) = trivias.peek().map(|(_, pair)| pair) {
                            let comment_kind = crate::ast::CommentKind::from_text(peek_text);

                            if let crate::ast::CommentKind::Doc = comment_kind {
                                continue;
                            }
                        }

                        break;
                    },
                    | SyntaxKind::COMMENT => {
                        res = i + 1;
                    },
                    | _ => {},
                }
            }

            res
        },
        | _ => 0,
    }
}

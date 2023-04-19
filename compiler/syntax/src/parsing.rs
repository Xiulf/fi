use cstree::interning::Interner;
use cstree::{TextRange, TextSize};
use diagnostics::{Db, Diagnostics};
use parser::error::SyntaxError;
use parser::token::{SyntaxKind, Token};
use parser::TreeSink;
use vfs::File;

use crate::syntax_node::{SyntaxNode, SyntaxTreeBuilder};
use crate::SyntaxDiagnostic;

pub fn parse_text<I: Interner>(db: &dyn Db, file: File, interner: &mut I) -> SyntaxNode {
    let text = file.text(db).as_deref().unwrap_or_default();
    let tokens = parser::lexer::Lexer::new(text).collect::<Vec<_>>();
    // dbg!(&tokens);
    let events = parser::parse(&tokens);
    let mut tree_sink = TextTreeSink::new(db, file, text, &tokens, interner);

    parser::event::process(&mut tree_sink, events);
    tree_sink.finish()
}

pub struct TextTreeSink<'t, 'i, I> {
    db: &'t dyn Db,
    file: File,
    text: &'t str,
    tokens: &'t [Token],
    token_pos: usize,
    text_pos: TextSize,
    state: State,
    inner: SyntaxTreeBuilder<'i, I>,
}

enum State {
    Normal,
    PendingStart,
    PendingFinish,
}

impl<'t, 'i, I: Interner> TextTreeSink<'t, 'i, I> {
    pub fn new(db: &'t dyn Db, file: File, text: &'t str, tokens: &'t [Token], interner: &'i mut I) -> Self {
        Self {
            db,
            file,
            text,
            tokens,
            state: State::PendingStart,
            token_pos: 0,
            text_pos: TextSize::from(0),
            inner: SyntaxTreeBuilder::new(interner),
        }
    }

    fn finish(mut self) -> SyntaxNode {
        match std::mem::replace(&mut self.state, State::Normal) {
            | State::PendingFinish => {
                self.eat_trivia();
                self.inner.finish_node();
            },
            | _ => unreachable!(),
        }

        self.inner.finish()
    }

    fn eat_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.token_pos) {
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

impl<'t, I: Interner> TreeSink for TextTreeSink<'t, '_, I> {
    fn token(&mut self, kind: SyntaxKind) {
        match std::mem::replace(&mut self.state, State::Normal) {
            | State::PendingStart => unreachable!(),
            | State::PendingFinish => self.inner.finish_node(),
            | State::Normal => {},
        }

        self.eat_trivia();
        let len = self.tokens[self.token_pos].len;
        self.do_token(kind, len)
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        match std::mem::replace(&mut self.state, State::Normal) {
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
        match std::mem::replace(&mut self.state, State::PendingFinish) {
            | State::PendingStart => unreachable!(),
            | State::PendingFinish => self.inner.finish_node(),
            | State::Normal => {},
        }
    }

    fn error(&mut self, error: SyntaxError) {
        Diagnostics::emit(self.db, SyntaxDiagnostic(error, self.file));
    }
}

fn n_attached_trivias<'a>(kind: SyntaxKind, trivias: impl Iterator<Item = (SyntaxKind, &'a str)>) -> usize {
    match kind {
        | SyntaxKind::ITEM_MODULE
        | SyntaxKind::ITEM_IMPORT
        | SyntaxKind::ITEM_FIXITY
        | SyntaxKind::ITEM_VALUE
        | SyntaxKind::ITEM_TYPE
        | SyntaxKind::ITEM_TRAIT
        | SyntaxKind::ITEM_IMPL => {
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

#[test]
fn test_parsing() {
    // let input = r#"
    //     module Core.Cmp =

    //     main = 0

    //     type X =
    //         | Y
    //         | Z

    //     trait Iterator self it =
    //         next :: self -> Option it

    //     impl Iterator Iter Item =
    //         next self = _
    // "#;
    // let input = unindent::unindent(input.trim());
    // let mut interner = cstree::interning::new_interner();
    // let parsed = parse_text(&input, &mut interner);

    // insta::assert_debug_snapshot!(parsed);
}

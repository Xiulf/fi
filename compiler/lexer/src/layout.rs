use super::*;

impl LayoutDelim {
    fn is_indented(self) -> bool {
        match self {
            LayoutDelim::Let | LayoutDelim::LetStmt | LayoutDelim::Where | LayoutDelim::Of | LayoutDelim::Do => true,
            _ => false,
        }
    }
}

impl Pos {
    fn before(self, s: &str) -> Self {
        Pos(self.0, self.1 - s.chars().count(), self.2 - ByteOffset::from_str_len(s))
    }
}

fn is_top_decl(pos: Pos, stack: &[(Pos, LayoutDelim)]) -> bool {
    match stack {
        [(_, LayoutDelim::Root), (pos2, LayoutDelim::Where)] => pos2.1 == pos.1,
        _ => false,
    }
}

struct Collapse(Vec<(Pos, LayoutDelim)>, usize);

impl<'src> Lexer<'src> {
    pub(crate) fn unwind(&mut self) {
        match self.stack[..] {
            [] => {}
            [.., (_, LayoutDelim::Root)] => {
                self.stack.pop().unwrap();
                self.queue.push_back(self.token(TokenType::EOF));
            }
            [.., (_, lyt)] if lyt.is_indented() => {
                self.queue.push_back(self.token(TokenType::LayoutEnd));
                self.stack.pop().unwrap();
                self.unwind();
            }
            [.., _] => {
                self.stack.pop().unwrap();
                self.unwind();
            }
        }
    }

    pub(crate) fn do_layout(&mut self, start: Pos, token: Token) {
        let Token { span, kind } = token;
        let pos = Pos(self.line, self.col, self.pos);
        let _ = self.skip();
        let next_pos = Pos(self.line, self.col, self.pos);
        // println!("{:?}: {:?}", kind, self.text(span));

        if let [.., (_, LayoutDelim::Attr)] = self.stack[..] {
            if let Token {
                kind: TokenType::RightBracket, ..
            } = token
            {
                self.stack.pop().unwrap();
                self.insert_sep(pos);
            }

            return;
        }

        match kind {
            TokenType::Name => match self.text(span) {
                "where" => {
                    if let [.., (_, LayoutDelim::TopDeclHead)] = self.stack[..] {
                        self.stack.pop().unwrap();
                        self.queue.push_back(token);
                        self.insert_start(LayoutDelim::Where, next_pos);
                    } else if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                        self.queue.push_back(token);
                    } else {
                        Collapse::new(self.queue.len()).collapse(
                            start,
                            |tok, pos, lyt| {
                                if let LayoutDelim::Do = lyt {
                                    true
                                } else {
                                    offside_end_p(tok, pos, lyt)
                                }
                            },
                            &mut self.stack,
                            &mut self.queue,
                        );

                        self.queue.push_back(token);
                        self.insert_start(LayoutDelim::Where, next_pos);
                    }
                }
                "data" => {
                    self.insert_default(start, start);
                    self.queue.push_back(token);

                    if is_top_decl(start, &self.stack) {
                        self.stack.push((start, LayoutDelim::TopDecl));
                    } else if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                        self.stack.pop().unwrap();
                    }
                }
                "trait" => {
                    self.insert_default(start, start);
                    self.queue.push_back(token);

                    if is_top_decl(start, &self.stack) {
                        self.stack.push((start, LayoutDelim::TopDeclHead));
                    } else if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                        self.stack.pop().unwrap();
                    }
                }
                "do" => {
                    self.insert_default(start, start);
                    self.queue.push_back(token);

                    if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                        self.stack.pop().unwrap();
                    } else {
                        self.insert_start(LayoutDelim::Do, pos);
                    }
                }
                _ => {
                    self.insert_default(start, start);
                    self.queue.push_back(token);

                    if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                        self.stack.pop().unwrap();
                    }
                }
            },
            TokenType::Pipe => {
                let mut c = Collapse::new(self.queue.len());

                c.collapse(start, offside_end_p, &mut self.stack, &mut self.queue);

                match self.stack[..] {
                    [.., (_, LayoutDelim::Of)] => {
                        self.stack.push((pos, LayoutDelim::CaseGuard));
                    }
                    [.., (_, LayoutDelim::Let)] => {
                        self.stack.push((pos, LayoutDelim::DeclGuard));
                    }
                    [.., (_, LayoutDelim::Where)] => {
                        self.stack.push((pos, LayoutDelim::DeclGuard));
                    }
                    _ => {
                        c.restore(&mut self.stack, &mut self.queue);
                        self.insert_default(start, next_pos);
                    }
                }

                self.queue.push_back(token);
            }
            TokenType::Equals => {
                let mut c = Collapse::new(self.queue.len());

                c.collapse(
                    start,
                    |_, _, lyt| match lyt {
                        LayoutDelim::Where => true,
                        LayoutDelim::Let => true,
                        _ => false,
                    },
                    &mut self.stack,
                    &mut self.queue,
                );

                if let [.., (_, LayoutDelim::DeclGuard)] = self.stack[..] {
                    self.queue.push_back(token);
                } else {
                    c.restore(&mut self.stack, &mut self.queue);
                    self.queue.push_back(token);
                    self.insert_default(start, next_pos);
                }
            }
            TokenType::Comma => {
                Collapse::new(self.queue.len()).collapse(start, indented_p, &mut self.stack, &mut self.queue);
                self.queue.push_back(token);

                if let [.., (_, LayoutDelim::Brace)] = self.stack[..] {
                    self.stack.push((pos, LayoutDelim::Prop));
                }
            }
            TokenType::DoubleDot => {
                Collapse::new(self.queue.len()).collapse(start, offside_p, &mut self.stack, &mut self.queue);
                self.queue.push_back(token);
                self.insert_sep(next_pos);
            }
            TokenType::Dot => {
                self.queue.push_back(token);
                self.insert_default(start, next_pos);

                if let [.., (_, LayoutDelim::Forall)] = self.stack[..] {
                    self.stack.pop().unwrap();
                } else {
                    self.stack.push((pos, LayoutDelim::Prop));
                }
            }
            TokenType::LeftParen => {
                self.insert_default(start, pos);
                self.queue.push_back(token);
                self.stack.push((pos, LayoutDelim::Paren));
            }
            TokenType::LeftBrace => {
                self.insert_default(start, pos);
                self.queue.push_back(token);
                self.stack.push((pos, LayoutDelim::Brace));
                self.stack.push((pos, LayoutDelim::Prop));
            }
            TokenType::LeftBracket => {
                self.insert_default(start, pos);
                self.queue.push_back(token);

                let before = start.before("[");

                if is_top_decl(before, &self.stack) {
                    self.stack.push((pos, LayoutDelim::Attr));
                } else {
                    self.stack.push((pos, LayoutDelim::Square));
                }
            }
            TokenType::RightParen => {
                Collapse::new(self.queue.len()).collapse(pos, indented_p, &mut self.stack, &mut self.queue);

                if let [.., (_, LayoutDelim::Paren)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.queue.push_back(token);
            }
            TokenType::RightBrace => {
                Collapse::new(self.queue.len()).collapse(pos, indented_p, &mut self.stack, &mut self.queue);

                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                if let [.., (_, LayoutDelim::Brace)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.queue.push_back(token);
            }
            TokenType::RightBracket => {
                Collapse::new(self.queue.len()).collapse(pos, indented_p, &mut self.stack, &mut self.queue);

                if let [.., (_, LayoutDelim::Square)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.queue.push_back(token);
            }
            TokenType::Operator => {
                Collapse::new(self.queue.len()).collapse(pos, offside_end_p, &mut self.stack, &mut self.queue);
                self.insert_sep(next_pos);
                self.queue.push_back(token);
            }
            _ => {
                self.queue.push_back(token);
                self.insert_default(pos, next_pos);
            }
        }
    }

    fn insert_default(&mut self, start: Pos, pos: Pos) {
        Collapse::new(self.queue.len()).collapse(start, offside_p, &mut self.stack, &mut self.queue);

        self.insert_sep(pos);
    }

    fn insert_start(&mut self, lyt: LayoutDelim, pos: Pos) {
        match self.stack.iter().find(|s| s.1.is_indented()) {
            Some((start, _)) if pos.1 <= start.1 => {}
            _ => {
                self.stack.push((pos, lyt));
                self.queue.push_back(self.token(TokenType::LayoutStart));
            }
        }
    }

    fn insert_sep(&mut self, pos: Pos) {
        match self.stack[..] {
            [.., (start, LayoutDelim::TopDecl)] if sep_p(pos, start) => {
                self.stack.pop().unwrap();
                self.queue.push_back(self.token(TokenType::LayoutSep));
            }
            [.., (start, LayoutDelim::TopDeclHead)] if sep_p(pos, start) => {
                self.stack.pop().unwrap();
                self.queue.push_back(self.token(TokenType::LayoutSep));
            }
            [.., (start, lyt)] if indent_sep_p(pos, start, lyt) => match lyt {
                LayoutDelim::Of => {
                    self.stack.push((start, LayoutDelim::CaseBinders));
                    self.queue.push_back(self.token(TokenType::LayoutSep));
                }
                _ => {
                    self.queue.push_back(self.token(TokenType::LayoutSep));
                }
            },
            _ => {}
        }
    }
}

fn indented_p(_: Pos, _: Pos, lyt: LayoutDelim) -> bool {
    lyt.is_indented()
}

fn offside_p(tok: Pos, pos: Pos, lyt: LayoutDelim) -> bool {
    lyt.is_indented() && tok.1 < pos.1
}

fn offside_end_p(tok: Pos, pos: Pos, lyt: LayoutDelim) -> bool {
    lyt.is_indented() && tok.1 <= pos.1
}

fn indent_sep_p(tok: Pos, pos: Pos, lyt: LayoutDelim) -> bool {
    lyt.is_indented() && sep_p(tok, pos)
}

fn sep_p(tok: Pos, pos: Pos) -> bool {
    tok.1 == pos.1 && tok.0 != pos.0
}

impl Collapse {
    fn new(tokens: usize) -> Self {
        Collapse(Vec::new(), tokens)
    }

    fn collapse(&mut self, start: Pos, p: fn(Pos, Pos, LayoutDelim) -> bool, stack: &mut Vec<(Pos, LayoutDelim)>, tokens: &mut VecDeque<Token>) {
        match stack[..] {
            [.., (lyt_pos, lyt)] if p(start, lyt_pos, lyt) => {
                self.0.push(stack.pop().unwrap());
                self.collapse(start, p, stack, tokens);

                if lyt.is_indented() {
                    tokens.push_back(Token {
                        span: Span::new(start.2, start.2),
                        kind: TokenType::LayoutEnd,
                    });
                }
            }
            _ => {}
        }
    }

    fn restore(mut self, stack: &mut Vec<(Pos, LayoutDelim)>, tokens: &mut VecDeque<Token>) {
        self.0.reverse();
        stack.append(&mut self.0);
        tokens.truncate(self.1);
    }
}

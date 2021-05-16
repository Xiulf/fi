use crate::error::SyntaxError;
use parser::syntax_kind::*;
use rowan::{TextRange, TextSize};
use std::iter::Peekable;
use std::str::CharIndices;
use unicode_xid::UnicodeXID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: SyntaxKind,
    pub len: TextSize,
}

pub fn tokenize(text: &str) -> (Vec<Token>, Vec<SyntaxError>) {
    if text.is_empty() {
        return (
            vec![Token {
                kind: EOF,
                len: TextSize::default(),
            }],
            Vec::new(),
        );
    }

    let lexer = Lexer::new(text);

    lexer.run()
}

struct Lexer<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
    start: TextSize,
    pos: TextSize,
    line: usize,
    col: usize,
    tokens: Vec<Token>,
    errors: Vec<SyntaxError>,
    start_lyt: Option<Vec<LayoutDelim>>,
    stack: Vec<((usize, usize), LayoutDelim)>,
    is_do: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LayoutDelim {
    Root,
    TopDecl,
    TopDeclHead,
    ClassHead,
    ClassBody,
    Where,
    Prop,
    ClosureHead,
    Case,
    CaseBinders,
    CaseGuard,
    Paren,
    Brace,
    Square,
    Angle,
    If,
    Then,
    Else,
    Of,
    Do,
    Loop,
}

impl LayoutDelim {
    fn is_indented(self) -> bool {
        match self {
            | LayoutDelim::Where | LayoutDelim::ClassBody | LayoutDelim::Of | LayoutDelim::Do | LayoutDelim::Loop => {
                true
            },
            | _ => false,
        }
    }
}

struct Collapse(Vec<((usize, usize), LayoutDelim)>, Vec<usize>, usize);

impl<'src> Lexer<'src> {
    fn new(source: &'src str) -> Self {
        Lexer {
            source,
            chars: source.char_indices().peekable(),
            start: TextSize::default(),
            pos: TextSize::default(),
            line: 0,
            col: 0,
            tokens: Vec::new(),
            errors: Vec::new(),
            start_lyt: None,
            stack: vec![((0, 0), LayoutDelim::Root)],
            is_do: false,
        }
    }

    fn run(mut self) -> (Vec<Token>, Vec<SyntaxError>) {
        while self.pos < TextSize::of(self.source) {
            self.next();
        }

        self.unwind();

        (self.tokens, self.errors)
    }

    fn next(&mut self) {
        let start = (self.line, self.col);
        let ch = self.peek();

        self.advance();

        match ch {
            | ch if ch.is_whitespace() => {
                while self.peek().is_whitespace() {
                    self.advance();
                }

                if let Some(mut lyt) = self.start_lyt.take() {
                    for &lyt in &lyt {
                        self.stack.push(((self.line, self.col), lyt));
                    }

                    let first = lyt.swap_remove(0);

                    if first == LayoutDelim::Do && self.peek() == '|' {
                        self.emit(WHITESPACE);
                        self.is_do = true;
                    } else if first.is_indented() {
                        self.emit(LYT_START);
                    } else {
                        self.emit(WHITESPACE);
                    }
                } else {
                    self.emit(WHITESPACE);
                }
            },
            | '-' if self.peek() == '-' => {
                while self.peek() != '\n' {
                    self.advance();
                }

                self.emit(COMMENT);
            },
            | '-' if self.peek().is_digit(10) => self.number(ch, start),
            | '0'..='9' => self.number(ch, start),
            | '"' => self.string(start, false),
            | 'r' if self.peek() == '"' => {
                self.advance();
                self.string(start, true)
            },
            | '\'' => self.character(start),
            | ch if ch.is_xid_start() => self.name(start),
            | '_' if self.peek().is_xid_continue() => self.name(start),
            | '_' => self.insert_default(start, UNDERSCORE),
            | '(' if is_op_char(self.peek()) => {
                while is_op_char(self.peek()) {
                    self.advance();
                }

                if self.peek() == ')' {
                    self.advance();
                    self.insert_default(start, SYMBOL);
                } else {
                    self.emit(ERROR);
                }
            },
            | '(' => {
                self.insert_default(start, L_PAREN);

                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.stack.push((start, LayoutDelim::Paren));
            },
            | '{' => {
                self.insert_default(start, L_BRACE);
                self.stack.push((start, LayoutDelim::Brace));
                self.stack.push((start, LayoutDelim::Prop));
            },
            | '[' => {
                self.insert_default(start, L_BRACKET);
                self.stack.push((start, LayoutDelim::Square));
            },
            | ')' => {
                Collapse::new(self.tokens.len()).collapse(start, indented_p, &mut self.stack, &mut self.tokens);

                if let [.., (_, LayoutDelim::Paren)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.emit(R_PAREN);
            },
            | '}' => {
                Collapse::new(self.tokens.len()).collapse(start, indented_p, &mut self.stack, &mut self.tokens);

                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                if let [.., (_, LayoutDelim::Brace)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.emit(R_BRACE);
            },
            | ']' => {
                Collapse::new(self.tokens.len()).collapse(start, indented_p, &mut self.stack, &mut self.tokens);

                if let [.., (_, LayoutDelim::Square)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.emit(R_BRACKET);
            },
            | '<' if self.peek() == '-' => {
                self.advance();
                self.emit(LEFT_ARROW);
            },
            | '-' if self.peek() == '>' => {
                self.advance();

                Collapse::new(self.tokens.len()).collapse(
                    start,
                    |s, p, lyt| match lyt {
                        | LayoutDelim::Do => true,
                        | LayoutDelim::Of => false,
                        | _ => offside_end_p(s, p, lyt),
                    },
                    &mut self.stack,
                    &mut self.tokens,
                );

                if let [.., (_, LayoutDelim::CaseBinders | LayoutDelim::CaseGuard)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }

                self.emit(ARROW);
            },
            | '.' if self.peek() == '.' => {
                self.advance();
                self.insert_default(start, DBL_DOT);
            },
            | '.' => {
                self.insert_default(start, DOT);
                self.stack.push((start, LayoutDelim::Prop));
            },
            | ':' if self.peek() == ':' => {
                self.advance();
                self.insert_default(start, DBL_COLON);
            },
            | ':' => {
                self.insert_default(start, COLON);
            },
            | '=' if self.peek() == '>' => {
                self.advance();
                self.insert_default(start, FAT_ARROW);
            },
            | '=' if !is_op_char(self.peek()) => match self.stack[..] {
                | [.., (_, LayoutDelim::TopDeclHead)] => {
                    self.stack.pop().unwrap();
                    self.emit(EQUALS);
                    self.insert_start(LayoutDelim::Where);
                },
                | [.., (_, LayoutDelim::ClassHead)] => {
                    self.stack.pop().unwrap();
                    self.emit(EQUALS);
                    self.insert_start(LayoutDelim::ClassBody);
                },
                | _ => {
                    self.insert_default(start, EQUALS);
                },
            },
            | '?' if !is_op_char(self.peek()) => {
                self.insert_default(start, QMARK);
            },
            | '@' if !is_op_char(self.peek()) => {
                self.insert_default(start, AT);
            },
            | '*' if !is_op_char(self.peek()) => {
                self.insert_default(start, STAR);

                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }
            },
            | '|' if self.is_do => {
                self.emit(PIPE);
                self.stack.pop().unwrap();
                self.stack.push((start, LayoutDelim::ClosureHead));
            },
            | '|' if self.is_closure() => {
                self.stack.pop().unwrap();
                self.emit(PIPE);
                self.insert_start(LayoutDelim::Do);
            },
            | '|' if !is_op_char(self.peek()) => {
                let mut c = Collapse::new(self.tokens.len());

                c.collapse(start, offside_end_p, &mut self.stack, &mut self.tokens);

                match self.stack[..] {
                    | [.., (_, LayoutDelim::Of)] => {
                        self.stack.push((start, LayoutDelim::CaseGuard));
                        self.emit(PIPE);
                    },
                    | _ => {
                        c.restore(&mut self.stack, &mut self.tokens);
                        self.insert_default(start, PIPE);
                    },
                }
            },
            | '/' if self.is_path_sep() => {
                self.insert_default(start, PATH_SEP);
            },
            | ch if is_op_char(ch) => {
                while is_op_char(self.peek()) {
                    self.advance();
                }

                Collapse::new(self.tokens.len()).collapse(start, offside_end_p, &mut self.stack, &mut self.tokens);
                self.insert_sep(start);
                self.emit(OPERATOR);
            },
            | ',' => {
                Collapse::new(self.tokens.len()).collapse(start, indented_p, &mut self.stack, &mut self.tokens);
                self.emit(COMMA);

                if let [.., (_, LayoutDelim::Brace)] = self.stack[..] {
                    self.stack.push((start, LayoutDelim::Prop));
                }
            },
            | _ => {
                self.errors
                    .push(SyntaxError::new(format!("unknown character {:?}", ch), self.span()));
                self.emit(ERROR);
            },
        }
    }

    fn number(&mut self, first: char, start: (usize, usize)) {
        if first == '0' {
            match self.peek() {
                | 'b' => {
                    self.advance();

                    while self.peek().is_digit(2) {
                        self.advance();
                    }

                    self.insert_default(start, INT);
                    return;
                },
                | 'x' => {
                    self.advance();

                    while self.peek().is_digit(16) {
                        self.advance();
                    }

                    self.insert_default(start, INT);
                    return;
                },
                | 'o' => {
                    self.advance();

                    while self.peek().is_digit(8) {
                        self.advance();
                    }

                    self.insert_default(start, INT);
                    return;
                },
                | _ => {},
            }
        }

        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_n(1).is_digit(10) {
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }

            self.insert_default(start, FLOAT);
        } else {
            self.insert_default(start, INT);
        }
    }

    fn string(&mut self, start: (usize, usize), raw: bool) {
        while self.pos < TextSize::of(self.source) {
            match self.peek() {
                | '"' => break,
                | '\\' if !raw => {
                    self.advance();
                    self.escape();
                },
                | _ => self.advance(),
            }
        }

        if self.peek() == '"' {
            self.advance();
            self.insert_default(start, STRING);
        } else {
            self.errors
                .push(SyntaxError::new("unterminated string literal", self.span()));
            self.emit(ERROR);
        }
    }

    fn character(&mut self, start: (usize, usize)) {
        match self.peek() {
            | '\'' => {
                self.advance();
                self.errors
                    .push(SyntaxError::new("empty character literal", self.span()));
            },
            | '\\' => {
                self.advance();
                self.escape();
            },
            | _ => self.advance(),
        }

        if self.peek() == '\'' {
            self.advance();
            self.insert_default(start, CHAR);
        } else {
            self.errors
                .push(SyntaxError::new("unterminated character literal", self.span()));
            self.advance();
        }
    }

    fn escape(&mut self) {
        match self.peek() {
            | '\'' | '"' | '\\' | 'r' | 'n' | '0' | 't' => self.advance(),
            | 'x' => {
                self.advance();

                if !self.peek().is_digit(16) {
                    self.errors
                        .push(SyntaxError::new_at_offset("invalid escape sequence", self.pos));
                }

                self.advance();

                if !self.peek().is_digit(16) {
                    self.errors
                        .push(SyntaxError::new_at_offset("invalid escape sequence", self.pos));
                }

                self.advance();
            },
            | 'u' => {
                self.advance();

                if self.peek() != '{' {
                    self.errors
                        .push(SyntaxError::new_at_offset("invalid escape sequence", self.pos));
                }

                self.advance();

                if !self.peek().is_digit(16) {
                    self.errors
                        .push(SyntaxError::new_at_offset("invalid escape sequence", self.pos));
                }

                self.advance();

                for _ in 0..5 {
                    if self.peek() == '}' {
                        break;
                    }

                    if !self.peek().is_digit(16) {
                        self.errors
                            .push(SyntaxError::new_at_offset("invalid escape sequence", self.pos));
                    }

                    self.advance();
                }

                if self.peek() != '}' {
                    self.errors
                        .push(SyntaxError::new_at_offset("invalid escape sequence", self.pos));
                }

                self.advance();
            },
            | _ => {
                self.errors
                    .push(SyntaxError::new_at_offset("invalid escape sequence", self.pos));
                self.advance();
            },
        }
    }

    fn name(&mut self, start: (usize, usize)) {
        while self.peek().is_xid_continue() {
            self.advance();
        }

        while self.peek() == '\'' {
            self.advance();
        }

        match self.text() {
            | "module" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, MODULE_KW);

                    if self.is_def_start(start) {
                        self.stack.push((start, LayoutDelim::TopDeclHead));
                    }
                }
            },
            | "import" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, IMPORT_KW);
                }
            },
            | "type" => {
                if self.is_top_decl(start) {
                    self.insert_default(start, TYPE_KW);
                    self.stack.push((start, LayoutDelim::TopDecl));
                } else if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, TYPE_KW);
                }
            },
            | "foreign" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, FOREIGN_KW);
                }
            },
            | "fun" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, FUN_KW);

                    // if self.is_def_start(start) {
                    //     self.stack.push((start, LayoutDelim::TopDeclHead));
                    // }
                }
            },
            | "static" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, STATIC_KW);
                }
            },
            | "const" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, CONST_KW);
                }
            },
            | "class" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, CLASS_KW);

                    if self.is_top_decl(start) {
                        self.stack.push((start, LayoutDelim::ClassHead));
                    }
                }
            },
            | "instance" => match self.stack[..] {
                | [.., (_, LayoutDelim::Prop)] => {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                },
                | [.., (_, LayoutDelim::Else)] => {
                    self.emit(INSTANCE_KW);
                    self.stack.pop().unwrap();
                    self.stack.push((start, LayoutDelim::TopDeclHead));
                },
                | _ => {
                    self.insert_default(start, INSTANCE_KW);

                    if self.is_top_decl(start) {
                        self.stack.push((start, LayoutDelim::TopDeclHead));
                    }
                },
            },
            | "infix" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, INFIX_KW);
                }
            },
            | "infixl" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, INFIXL_KW);
                }
            },
            | "infixr" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, INFIXR_KW);
                }
            },
            | "as" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, AS_KW);
                }
            },
            | "do" => match self.stack[..] {
                | [.., (_, LayoutDelim::Prop)] => {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                },
                | [.., (_, LayoutDelim::If)] => {
                    self.stack.pop().unwrap();
                    self.insert_default(start, DO_KW);
                    self.insert_start(LayoutDelim::Do);
                },
                | _ => {
                    self.insert_default(start, DO_KW);
                    self.insert_start(LayoutDelim::Do);
                },
            },
            | "if" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, IF_KW);
                    self.insert_start(LayoutDelim::If);
                }
            },
            | "unless" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, UNLESS_KW);
                    self.insert_start(LayoutDelim::If);
                }
            },
            | "then" => {
                let mut c = Collapse::new(self.tokens.len());

                c.collapse(start, indented_p, &mut self.stack, &mut self.tokens);

                if let [.., (_, LayoutDelim::If)] = self.stack[..] {
                    self.emit(THEN_KW);
                    self.stack.pop().unwrap();
                    self.stack.push((start, LayoutDelim::Then));
                } else {
                    c.restore(&mut self.stack, &mut self.tokens);

                    if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                        self.emit(IDENT);
                        self.stack.pop().unwrap();
                    } else {
                        self.insert_default(start, THEN_KW);
                    }
                }
            },
            | "else" => {
                let mut c = Collapse::new(self.tokens.len());

                c.collapse(start, indented_p, &mut self.stack, &mut self.tokens);

                match self.stack[..] {
                    | [.., (_, LayoutDelim::Then)] => {
                        self.emit(ELSE_KW);
                        self.stack.pop().unwrap();
                    },
                    | _ => {
                        c.restore(&mut self.stack, &mut self.tokens);
                        Collapse::new(self.tokens.len()).collapse(
                            start,
                            |tok, pos, lyt| match lyt {
                                | LayoutDelim::TopDeclHead => true,
                                | _ => offside_p(tok, pos, lyt),
                            },
                            &mut self.stack,
                            &mut self.tokens,
                        );

                        if self.is_top_decl(start) {
                            self.emit(ELSE_KW);
                            self.stack.push((start, LayoutDelim::Else));
                        } else {
                            self.insert_sep(start);

                            if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                                self.emit(IDENT);
                                self.stack.pop().unwrap();
                            } else {
                                self.emit(ELSE_KW);
                            }
                        }
                    },
                }
            },
            | "while" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, WHILE_KW);
                }
            },
            | "until" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, UNTIL_KW);
                }
            },
            | "loop" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, LOOP_KW);
                    self.insert_start(LayoutDelim::Loop);
                }
            },
            | "break" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, BREAK_KW);
                }
            },
            | "next" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, NEXT_KW);
                }
            },
            | "yield" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, YIELD_KW);
                }
            },
            | "return" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, RETURN_KW);
                }
            },
            | "case" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, CASE_KW);
                    self.insert_start(LayoutDelim::Case);
                }
            },
            | "of" => {
                Collapse::new(self.tokens.len()).collapse(start, indented_p, &mut self.stack, &mut self.tokens);

                match self.stack[..] {
                    | [.., (_, LayoutDelim::Prop)] => {
                        self.emit(IDENT);
                        self.stack.pop().unwrap();
                    },
                    | [.., (_, LayoutDelim::Case)] => {
                        self.emit(OF_KW);
                        self.stack.pop().unwrap();
                        self.insert_start(LayoutDelim::Of);
                        self.insert_start(LayoutDelim::CaseBinders);
                    },
                    | _ => {
                        self.insert_default(start, OF_KW);
                    },
                }
            },
            | "let" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, LET_KW);
                }
            },
            | "for" => {
                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.emit(IDENT);
                    self.stack.pop().unwrap();
                } else {
                    self.insert_default(start, FOR_KW);
                }
            },
            | _ => {
                self.insert_default(start, IDENT);

                if let [.., (_, LayoutDelim::Prop)] = self.stack[..] {
                    self.stack.pop().unwrap();
                }
            },
        }
    }

    fn unwind(&mut self) {
        match self.stack[..] {
            | [] => {},
            | [.., (_, LayoutDelim::Root)] => {
                self.stack.pop().unwrap();
                self.emit(EOF);
            },
            | [.., (_, lyt)] if lyt.is_indented() => {
                if let Some(idx) = self.last_ws() {
                    self.tokens[idx].kind = LYT_END;
                } else {
                    self.emit(LYT_END);
                }

                self.stack.pop().unwrap();
                self.unwind();
            },
            | [.., _] => {
                self.stack.pop().unwrap();
                self.unwind();
            },
        }
    }

    fn is_top_decl(&self, pos: (usize, usize)) -> bool {
        if let [(_, LayoutDelim::Root), (start, LayoutDelim::Where)] = self.stack[..] {
            start.1 == pos.1
        } else {
            false
        }
    }

    fn is_def_start(&self, pos: (usize, usize)) -> bool {
        match self.stack[..] {
            | [(start, LayoutDelim::Root)] => start.1 == pos.1,
            | [.., (start, LayoutDelim::Where)] => start.1 == pos.1,
            | _ => false,
        }
    }

    fn is_closure(&self) -> bool {
        matches!(self.stack[..], [.., (_, LayoutDelim::ClosureHead)])
    }

    fn is_path_sep(&mut self) -> bool {
        let next = self.peek();

        if let Some(tok) = self.tokens.last() {
            tok.kind == IDENT && (next.is_xid_start() || (next == '(' && is_op_char(self.peek_n(1))))
        } else {
            false
        }
    }

    fn insert_default(&mut self, start: (usize, usize), kind: SyntaxKind) {
        Collapse::new(self.tokens.len()).collapse(start, offside_p, &mut self.stack, &mut self.tokens);

        self.insert_sep(start);
        self.emit(kind);
    }

    fn insert_start(&mut self, delim: LayoutDelim) {
        if let Some(delims) = self.start_lyt.as_mut() {
            delims.push(delim);
        } else {
            self.start_lyt = Some(vec![delim]);
        }
    }

    fn insert_sep(&mut self, start: (usize, usize)) {
        match self.stack[..] {
            | [.., (pos, LayoutDelim::TopDecl | LayoutDelim::TopDeclHead | LayoutDelim::ClassHead)]
                if sep_p(start, pos) =>
            {
                self.stack.pop().unwrap();

                if let Some(idx) = self.last_ws() {
                    self.tokens[idx].kind = LYT_SEP;
                } else {
                    self.tokens.push(Token {
                        kind: LYT_SEP,
                        len: TextSize::from(0),
                    });
                }
            }
            | [(pos, LayoutDelim::Root)] if sep_p(start, pos) => {
                if let Some(idx) = self.last_ws() {
                    self.tokens[idx].kind = LYT_SEP;
                } else {
                    self.tokens.push(Token {
                        kind: LYT_SEP,
                        len: TextSize::from(0),
                    });
                }
            },
            | [.., (pos, lyt)] if indent_sep_p(start, pos, lyt) => match lyt {
                | LayoutDelim::Of => {
                    self.stack.push((pos, LayoutDelim::CaseBinders));

                    if let Some(idx) = self.last_ws() {
                        self.tokens[idx].kind = LYT_SEP;
                    } else {
                        self.tokens.push(Token {
                            kind: LYT_SEP,
                            len: TextSize::from(0),
                        });
                    }
                },
                | _ => {
                    if let Some(idx) = self.last_ws() {
                        self.tokens[idx].kind = LYT_SEP;
                    } else {
                        self.tokens.push(Token {
                            kind: LYT_SEP,
                            len: TextSize::from(0),
                        });
                    }
                },
            },
            | _ => {},
        }
    }

    fn last_ws(&self) -> Option<usize> {
        for (i, tok) in self.tokens.iter().enumerate().rev() {
            match tok.kind {
                | COMMENT => continue,
                | WHITESPACE => return Some(i),
                | _ => break,
            }
        }

        None
    }

    fn text(&self) -> &'src str {
        &self.source[self.span()]
    }

    fn span(&self) -> TextRange {
        TextRange::new(self.start, self.pos)
    }

    fn emit(&mut self, kind: SyntaxKind) {
        self.tokens.push(Token {
            kind,
            len: self.pos - self.start,
        });

        self.start = self.pos;
        self.is_do = false;
    }

    fn peek(&mut self) -> char {
        self.chars.peek().copied().map(|(_, c)| c).unwrap_or('\0')
    }

    fn peek_n(&self, n: usize) -> char {
        let mut chars = self.chars.clone();
        let mut ch = chars.next();

        for _ in 0..n {
            ch = chars.next();
        }

        ch.map(|(_, c)| c).unwrap_or('\0')
    }

    fn advance(&mut self) {
        if let Some((idx, ch)) = self.chars.next() {
            self.pos = TextSize::from((idx + ch.len_utf8()) as u32);
            self.col += 1;

            if ch == '\n' {
                self.col = 0;
                self.line += 1;
            }
        } else {
            self.pos = TextSize::of(self.source);
            self.col += 1;
        }
    }
}

fn is_op_char(ch: char) -> bool {
    match ch {
        | '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | '~' | '\\' | '/' | '?' | '<' | '>'
        | '|' => true,
        | _ => false,
    }
}

fn indented_p(_: (usize, usize), _: (usize, usize), lyt: LayoutDelim) -> bool {
    lyt.is_indented()
}

fn offside_p(tok: (usize, usize), pos: (usize, usize), lyt: LayoutDelim) -> bool {
    lyt.is_indented() && tok.1 < pos.1
}

fn offside_end_p(tok: (usize, usize), pos: (usize, usize), lyt: LayoutDelim) -> bool {
    lyt.is_indented() && tok.1 <= pos.1
}

fn indent_sep_p(tok: (usize, usize), pos: (usize, usize), lyt: LayoutDelim) -> bool {
    lyt.is_indented() && sep_p(tok, pos)
}

fn sep_p(tok: (usize, usize), pos: (usize, usize)) -> bool {
    tok.1 == pos.1 && tok.0 != pos.0
}

impl Collapse {
    fn new(tokens: usize) -> Self {
        Collapse(Vec::new(), Vec::with_capacity(2), tokens)
    }

    fn collapse(
        &mut self,
        start: (usize, usize),
        p: fn((usize, usize), (usize, usize), LayoutDelim) -> bool,
        stack: &mut Vec<((usize, usize), LayoutDelim)>,
        tokens: &mut Vec<Token>,
    ) {
        match stack[..] {
            | [.., (lyt_pos, lyt)] if p(start, lyt_pos, lyt) => {
                self.0.push(stack.pop().unwrap());

                fn last_ws(tokens: &[Token]) -> Option<usize> {
                    for (i, tok) in tokens.iter().enumerate().rev() {
                        match tok.kind {
                            | COMMENT => continue,
                            | WHITESPACE => return Some(i),
                            | _ => break,
                        }
                    }

                    None
                }

                if lyt.is_indented() {
                    if let Some(idx) = last_ws(tokens) {
                        self.1.push(idx);
                        tokens[idx].kind = LYT_END;
                    } else {
                        tokens.push(Token {
                            kind: LYT_END,
                            len: TextSize::from(0),
                        });
                    }
                }

                self.collapse(start, p, stack, tokens);
            },
            | _ => {},
        }
    }

    fn restore(mut self, stack: &mut Vec<((usize, usize), LayoutDelim)>, tokens: &mut Vec<Token>) {
        self.0.reverse();
        stack.append(&mut self.0);
        tokens.truncate(self.2);

        for idx in self.1.drain(..) {
            tokens[idx].kind = WHITESPACE;
        }
    }
}

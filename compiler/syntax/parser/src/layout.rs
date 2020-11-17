use crate::buffer::{Cursor, Entry, TokenBuffer};
use crate::error::Result;
use crate::parse::{Parse, ParseStream};
use crate::token::Token;
use codespan::{ByteIndex, Files, Location, Span};
use std::sync::Arc;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LytStart(Span);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LytSep(Span);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LytEnd(Span);

pub fn fix_layout(files: &Files<Arc<str>>, buffer: TokenBuffer) -> TokenBuffer {
    let mut tokens = Vec::new();
    let mut stack = Vec::new();
    let mut cursor = buffer.begin();
    let mut pos = Pos {
        offset: ByteIndex::default(),
        loc: Location::new(0, 0),
    };

    let file = cursor.file;

    stack.push((pos, LayoutDelim::Root));

    while !cursor.eof() {
        pos.offset = cursor.span().start();
        pos.loc = files.location(file, cursor.span().start()).unwrap();
        insert_layout(files, cursor, &mut stack, &mut tokens);

        if TArrow::peek(cursor) {
            cursor = cursor.bump().bump();
        } else if TDblDot::peek(cursor) {
            cursor = cursor.bump().bump();
        } else {
            cursor = cursor.bump();
        }
    }

    unwind(pos, &stack, &mut tokens);

    TokenBuffer::new(file, tokens)
}

impl<D> Parse<D> for LytStart {
    fn parse(input: ParseStream<D>) -> Result<Self> {
        input.step(|cursor| match cursor.lyt_start() {
            Some((span, rest)) => Ok((LytStart(span), rest)),
            None => Err(cursor.error("expected the start of a block")),
        })
    }
}

impl Token for LytStart {
    fn peek(cursor: Cursor) -> bool {
        cursor.lyt_start().is_some()
    }

    fn display() -> &'static str {
        "layout start"
    }
}

impl<D> Parse<D> for LytSep {
    fn parse(input: ParseStream<D>) -> Result<Self> {
        input.step(|cursor| match cursor.lyt_sep() {
            Some((span, rest)) => Ok((LytSep(span), rest)),
            None => Err(cursor.error("expected a newline")),
        })
    }
}

impl Token for LytSep {
    fn peek(cursor: Cursor) -> bool {
        cursor.lyt_sep().is_some()
    }

    fn display() -> &'static str {
        "layout separator"
    }
}

impl<D> Parse<D> for LytEnd {
    fn parse(input: ParseStream<D>) -> Result<Self> {
        input.step(|cursor| match cursor.lyt_end() {
            Some((span, rest)) => Ok((LytEnd(span), rest)),
            None => Err(cursor.error("expected the end of a block")),
        })
    }
}

#[derive(Debug, Clone, Copy)]
struct Pos {
    offset: ByteIndex,
    loc: Location,
}

impl Token for LytEnd {
    fn peek(cursor: Cursor) -> bool {
        cursor.lyt_end().is_some()
    }

    fn display() -> &'static str {
        "layout end"
    }
}

type LayoutStack = Vec<(Pos, LayoutDelim)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LayoutDelim {
    Root,
    TopDecl,
    TopDeclHead,
    Attr,
    Prop,
    DeclGuard,
    Case,
    CaseBinders,
    CaseGuard,
    Paren,
    Brace,
    Square,
    If,
    Then,
    Forall,
    Let,
    LetStmt,
    Struct,
    Where,
    Of,
    Do,
}

impl LayoutDelim {
    fn is_indented(&self) -> bool {
        match self {
            LayoutDelim::Let
            | LayoutDelim::LetStmt
            | LayoutDelim::Struct
            | LayoutDelim::Where
            | LayoutDelim::Of
            | LayoutDelim::Do => true,
            _ => false,
        }
    }
}

fn is_top_decl(pos: Pos, stack: &LayoutStack) -> bool {
    match &stack[..] {
        [(_, LayoutDelim::Root), (pos2, LayoutDelim::Where)] => pos2.loc.column == pos.loc.column,
        _ => false,
    }
}

crate::token![punct "->" TArrow/2];
crate::token![punct ".." TDblDot/2];

fn insert_layout(
    files: &Files<Arc<str>>,
    cursor: Cursor,
    stack: &mut LayoutStack,
    tokens: &mut Vec<Entry>,
) {
    crate::token![ident "fn" TFn];
    crate::token![ident "data" TData];
    crate::token![ident "iface" TIface];
    crate::token![ident "where" TWhere];
    crate::token![ident "in" TIn];
    crate::token![ident "let" TLet];
    crate::token![ident "do" TDo];
    crate::token![ident "loop" TLoop];
    crate::token![ident "case" TCase];
    crate::token![ident "of" TOf];
    crate::token![ident "if" TIf];
    crate::token![ident "then" TThen];
    crate::token![ident "else" TElse];
    crate::token![ident "forall" TForall];
    crate::token![punct "=" TEquals/1];
    crate::token![punct "|" TPipe/1];
    crate::token![punct "`" TTick/1];
    crate::token![punct "," TComma/1];
    crate::token![punct "." TDot/1];
    crate::token![punct "(" TLParen/1];
    crate::token![punct ")" TRParen/1];
    crate::token![punct "{" TLBrace/1];
    crate::token![punct "}" TRBrace/1];
    crate::token![punct "[" TLSquare/1];
    crate::token![punct "]" TRSquare/1];

    insert(files, cursor, stack, tokens);

    fn pos(files: &Files<Arc<str>>, file: codespan::FileId, offset: ByteIndex) -> Pos {
        Pos {
            offset,
            loc: files.location(file, offset).unwrap(),
        }
    }

    fn insert(
        files: &Files<Arc<str>>,
        cursor: Cursor,
        stack: &mut LayoutStack,
        tokens: &mut Vec<Entry>,
    ) {
        if let [.., (_, LayoutDelim::Attr)] = stack[..] {
            if TRSquare::peek(cursor) {
                stack.pop().unwrap();
            }

            tokens.push(cursor.entry().clone());
            return;
        }

        if TData::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            let p = pos(files, cursor.file, cursor.span().start());

            if is_top_decl(p, stack) {
                stack.push((p, LayoutDelim::TopDecl));
            }
        } else if TIface::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            let p = pos(files, cursor.file, cursor.span().start());

            if is_top_decl(p, stack) {
                stack.push((p, LayoutDelim::TopDeclHead));
            } else if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                stack.pop().unwrap();
            }
        } else if TWhere::peek(cursor) {
            match stack[..] {
                [.., (_, LayoutDelim::TopDeclHead)] => {
                    stack.pop().unwrap();
                    tokens.push(cursor.entry().clone());
                    insert_start(LayoutDelim::Where, next_pos(files, cursor), stack, tokens);
                }
                _ => {
                    Collapse::new(tokens).collapse(
                        files,
                        |tok_pos, lyt_pos, lyt| {
                            if let LayoutDelim::Do = lyt {
                                true
                            } else {
                                offside_end_p(tok_pos, lyt_pos, lyt)
                            }
                        },
                        cursor,
                        stack,
                        tokens,
                    );

                    tokens.push(cursor.entry().clone());
                    insert_start(LayoutDelim::Where, next_pos(files, cursor), stack, tokens);
                }
            }
        } else if TIn::peek(cursor) {
            let mut c = Collapse::new(tokens);

            c.collapse(
                files,
                |_, _, lyt| match lyt {
                    LayoutDelim::Let => false,
                    _ => lyt.is_indented(),
                },
                cursor,
                stack,
                tokens,
            );

            match stack[..] {
                [.., (_, lyt)] if lyt.is_indented() => {
                    stack.pop().unwrap();
                    tokens.push(Entry::LayoutEnd(span(pos(
                        files,
                        cursor.file,
                        cursor.span().start(),
                    ))));
                    tokens.push(cursor.entry().clone());
                }
                _ => {
                    c.restore(stack, tokens);
                    insert_default(files, cursor, stack, tokens);

                    if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                        stack.pop().unwrap();
                    }
                }
            }
        } else if TLet::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            match stack[..] {
                [.., (_, LayoutDelim::Prop)] => {
                    stack.pop().unwrap();
                }
                [.., (p, LayoutDelim::Do)]
                    if p.loc.column
                        == files
                            .location(cursor.file, cursor.span().start())
                            .unwrap()
                            .column =>
                {
                    insert_start(LayoutDelim::LetStmt, next_pos(files, cursor), stack, tokens);
                }
                _ => {
                    insert_start(LayoutDelim::Let, next_pos(files, cursor), stack, tokens);
                }
            }
        } else if TDo::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                stack.pop().unwrap();
            } else {
                insert_start(LayoutDelim::Do, next_pos(files, cursor), stack, tokens);
            }
        } else if TLoop::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                stack.pop().unwrap();
            } else {
                insert_start(LayoutDelim::Do, next_pos(files, cursor), stack, tokens);
            }
        } else if TCase::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                stack.pop().unwrap();
            } else {
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::Case,
                ));
            }
        } else if TOf::peek(cursor) {
            Collapse::new(tokens).collapse(files, indented_p, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Case)] = stack[..] {
                stack.pop().unwrap();
                tokens.push(cursor.entry().clone());
                insert_start(LayoutDelim::Of, next_pos(files, cursor), stack, tokens);
                stack.push((next_pos(files, cursor), LayoutDelim::CaseBinders));
            } else {
                insert_default(files, cursor, stack, tokens);

                if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                    stack.pop().unwrap();
                }
            }
        } else if TIf::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                stack.pop().unwrap();
            } else {
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::If,
                ));
            }
        } else if TThen::peek(cursor) {
            let mut c = Collapse::new(tokens);

            c.collapse(files, indented_p, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::If)] = stack[..] {
                stack.pop().unwrap();
                tokens.push(cursor.entry().clone());
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::Then,
                ));
            } else {
                c.restore(stack, tokens);
                insert_default(files, cursor, stack, tokens);

                if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                    stack.pop().unwrap();
                }
            }
        } else if TElse::peek(cursor) {
            let mut c = Collapse::new(tokens);

            c.collapse(files, indented_p, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Then)] = stack[..] {
                stack.pop().unwrap();
                tokens.push(cursor.entry().clone());
            } else {
                c.restore(stack, tokens);
                Collapse::new(tokens).collapse(files, offside_p, cursor, stack, tokens);

                if is_top_decl(pos(files, cursor.file, cursor.span().start()), stack) {
                    tokens.push(cursor.entry().clone());
                } else {
                    insert_sep(files, cursor, stack, tokens);
                    tokens.push(cursor.entry().clone());

                    if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                        stack.pop().unwrap();
                    }
                }
            }
        } else if TForall::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                stack.pop().unwrap();
            } else {
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::Forall,
                ));
            }
        } else if TArrow::peek(cursor) {
            Collapse::new(tokens).collapse(
                files,
                |tok_pos, lyt_pos, lyt| match lyt {
                    LayoutDelim::Do => true,
                    LayoutDelim::Of => false,
                    _ => offside_end_p(tok_pos, lyt_pos, lyt),
                },
                cursor,
                stack,
                tokens,
            );

            match stack[..] {
                [.., (_, LayoutDelim::CaseBinders)] | [.., (_, LayoutDelim::CaseGuard)] => {
                    stack.pop().unwrap();
                }
                _ => {}
            }

            tokens.push(cursor.entry().clone());
            tokens.push(cursor.bump().entry().clone());
        } else if TEquals::peek(cursor) {
            let mut c = Collapse::new(tokens);

            c.collapse(
                files,
                |_, _, lyt| match lyt {
                    LayoutDelim::Where => true,
                    LayoutDelim::Let => true,
                    LayoutDelim::LetStmt => true,
                    _ => false,
                },
                cursor,
                stack,
                tokens,
            );

            if let [.., (_, LayoutDelim::DeclGuard)] = stack[..] {
                stack.pop().unwrap();
                tokens.push(cursor.entry().clone());
            } else {
                c.restore(stack, tokens);
                insert_default(files, cursor, stack, tokens);
            }
        } else if TPipe::peek(cursor) {
            let mut c = Collapse::new(tokens);

            c.collapse(files, offside_end_p, cursor, stack, tokens);

            let p = pos(files, cursor.file, cursor.span().start());

            match stack[..] {
                [.., (_, LayoutDelim::Of)] => {
                    stack.push((p, LayoutDelim::CaseGuard));
                    tokens.push(cursor.entry().clone());
                }
                [.., (_, LayoutDelim::Let)] => {
                    stack.push((p, LayoutDelim::DeclGuard));
                    tokens.push(cursor.entry().clone());
                }
                [.., (_, LayoutDelim::LetStmt)] => {
                    stack.push((p, LayoutDelim::DeclGuard));
                    tokens.push(cursor.entry().clone());
                }
                [.., (_, LayoutDelim::Where)] => {
                    stack.push((p, LayoutDelim::DeclGuard));
                    tokens.push(cursor.entry().clone());
                }
                _ => {
                    c.restore(stack, tokens);
                    insert_default(files, cursor, stack, tokens);
                }
            }
        } else if TComma::peek(cursor) {
            Collapse::new(tokens).collapse(files, indented_p, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Brace)] = stack[..] {
                tokens.push(cursor.entry().clone());
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::Prop,
                ));
            } else {
                tokens.push(cursor.entry().clone());
            }
        } else if TDblDot::peek(cursor) {
            Collapse::new(tokens).collapse(files, offside_p, cursor, stack, tokens);
            insert_sep(files, cursor, stack, tokens);
            tokens.push(cursor.entry().clone());
            tokens.push(cursor.bump().entry().clone());
        } else if TDot::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Forall)] = stack[..] {
                stack.pop().unwrap();
            } else {
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::Prop,
                ));
            }
        } else if TLParen::peek(cursor) {
            insert_default(files, cursor, stack, tokens);
            stack.push((
                pos(files, cursor.file, cursor.span().start()),
                LayoutDelim::Paren,
            ));
        } else if TLBrace::peek(cursor) {
            insert_default(files, cursor, stack, tokens);
            stack.push((
                pos(files, cursor.file, cursor.span().start()),
                LayoutDelim::Brace,
            ));
            stack.push((
                pos(files, cursor.file, cursor.span().start()),
                LayoutDelim::Prop,
            ));
        } else if TLSquare::peek(cursor) {
            insert_default(files, cursor, stack, tokens);

            if is_top_decl(pos(files, cursor.file, cursor.span().start()), stack) {
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::Attr,
                ));
            } else {
                stack.push((
                    pos(files, cursor.file, cursor.span().start()),
                    LayoutDelim::Square,
                ));
            }
        } else if TRParen::peek(cursor) {
            Collapse::new(tokens).collapse(files, indented_p, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Paren)] = stack[..] {
                stack.pop().unwrap();
            }

            tokens.push(cursor.entry().clone());
        } else if TRBrace::peek(cursor) {
            Collapse::new(tokens).collapse(files, indented_p, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Prop)] = stack[..] {
                stack.pop().unwrap();
            }

            if let [.., (_, LayoutDelim::Brace)] = stack[..] {
                stack.pop().unwrap();
            }

            tokens.push(cursor.entry().clone());
        } else if TRSquare::peek(cursor) {
            Collapse::new(tokens).collapse(files, indented_p, cursor, stack, tokens);

            if let [.., (_, LayoutDelim::Square)] = stack[..] {
                stack.pop().unwrap();
            }

            tokens.push(cursor.entry().clone());
        } else if crate::punct::Punct::peek(cursor) {
            Collapse::new(tokens).collapse(files, offside_end_p, cursor, stack, tokens);
            insert_sep(files, cursor, stack, tokens);
            tokens.push(cursor.entry().clone());
        } else {
            insert_default(files, cursor, stack, tokens);
        }
    }

    fn insert_default(
        files: &Files<Arc<str>>,
        cursor: Cursor,
        stack: &mut LayoutStack,
        tokens: &mut Vec<Entry>,
    ) {
        Collapse::new(tokens).collapse(files, offside_p, cursor, stack, tokens);
        insert_sep(files, cursor, stack, tokens);
        tokens.push(cursor.entry().clone());
    }

    fn insert_start(
        lyt: LayoutDelim,
        next_pos: Pos,
        stack: &mut LayoutStack,
        tokens: &mut Vec<Entry>,
    ) {
        match stack.iter().find(|s| s.1.is_indented()) {
            Some((pos, _)) if next_pos.loc.column <= pos.loc.column => {}
            _ => {
                stack.push((next_pos, lyt));
                tokens.push(Entry::LayoutStart(span(next_pos)));
            }
        }
    }

    fn insert_sep(
        files: &Files<Arc<str>>,
        cursor: Cursor,
        stack: &mut LayoutStack,
        tokens: &mut Vec<Entry>,
    ) {
        match stack[..] {
            [.., (pos2, LayoutDelim::TopDecl)]
                if sep_p(pos(files, cursor.file, cursor.span().start()), pos2) =>
            {
                stack.pop().unwrap();
                tokens.push(Entry::LayoutSep(span(pos2)));
            }
            [.., (pos2, LayoutDelim::TopDeclHead)]
                if sep_p(pos(files, cursor.file, cursor.span().start()), pos2) =>
            {
                stack.pop().unwrap();
                tokens.push(Entry::LayoutSep(span(pos2)));
            }
            [.., (pos2, lyt)]
                if indent_sep_p(pos(files, cursor.file, cursor.span().start()), pos2, lyt) =>
            {
                match lyt {
                    LayoutDelim::Of => {
                        tokens.push(Entry::LayoutSep(span(pos2)));
                        stack.push((pos2, LayoutDelim::CaseBinders));
                    }
                    _ => {
                        tokens.push(Entry::LayoutSep(span(pos2)));
                    }
                }
            }
            _ => {}
        }
    }

    struct Collapse(Vec<(Pos, LayoutDelim)>, usize);

    impl Collapse {
        fn new(tokens: &[Entry]) -> Self {
            Collapse(Vec::new(), tokens.len())
        }

        fn collapse(
            &mut self,
            files: &Files<Arc<str>>,
            p: fn(Pos, Pos, LayoutDelim) -> bool,
            cursor: Cursor,
            stack: &mut LayoutStack,
            tokens: &mut Vec<Entry>,
        ) {
            match stack[..] {
                [.., (lyt_pos, lyt)]
                    if p(pos(files, cursor.file, cursor.span().start()), lyt_pos, lyt) =>
                {
                    self.0.push(stack.pop().unwrap());
                    self.collapse(files, p, cursor, stack, tokens);

                    if lyt.is_indented() {
                        tokens.push(Entry::LayoutEnd(span(lyt_pos)));
                    }
                }
                _ => {}
            }
        }

        fn restore(mut self, stack: &mut LayoutStack, tokens: &mut Vec<Entry>) {
            self.0.reverse();
            stack.append(&mut self.0);
            tokens.truncate(self.1);
        }
    }

    fn indented_p(_: Pos, _: Pos, lyt: LayoutDelim) -> bool {
        lyt.is_indented()
    }

    fn offside_p(tok_pos: Pos, lyt_pos: Pos, lyt: LayoutDelim) -> bool {
        lyt.is_indented() && tok_pos.loc.column < lyt_pos.loc.column
    }

    fn offside_end_p(tok_pos: Pos, lyt_pos: Pos, lyt: LayoutDelim) -> bool {
        lyt.is_indented() && tok_pos.loc.column <= lyt_pos.loc.column
    }

    fn indent_sep_p(tok_pos: Pos, lyt_pos: Pos, lyt: LayoutDelim) -> bool {
        lyt.is_indented() && sep_p(tok_pos, lyt_pos)
    }

    fn sep_p(tok_pos: Pos, lyt_pos: Pos) -> bool {
        tok_pos.loc.column == lyt_pos.loc.column && tok_pos.loc.line != lyt_pos.loc.line
    }
}

fn unwind(pos: Pos, stack: &[(Pos, LayoutDelim)], tokens: &mut Vec<Entry>) {
    match stack {
        [] => {}
        [.., (_, LayoutDelim::Root)] => tokens.push(Entry::Empty),
        [rest @ .., (_, lyt)] if lyt.is_indented() => {
            tokens.push(Entry::LayoutEnd(Span::new(pos.offset, pos.offset)));
            unwind(pos, rest, tokens);
        }
        [rest @ .., _] => unwind(pos, rest, tokens),
    }
}

fn span(pos: Pos) -> Span {
    Span::new(pos.offset, pos.offset)
}

fn next_pos(files: &Files<Arc<str>>, cursor: Cursor) -> Pos {
    let offset = if cursor.bump().eof() {
        cursor.span().start()
    } else {
        cursor.bump().span().start()
    };

    Pos {
        offset,
        loc: files.location(cursor.file, offset).unwrap(),
    }
}

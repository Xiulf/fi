use markup::MarkupRenderer;
pub use markup::{Markup, Styles};
use relative_path::{RelativePath, RelativePathBuf};
use std::io::{self, Write};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Page {
    pub path: hir::Path,
    pub title: String,
    pub sections: Vec<Section>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Section {
    pub title: String,
    pub entries: Vec<Entry>,
}

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct Entry {
    elems: Vec<EntryElem>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum EntryElem {
    Title(String),
    Code(Code),
    Source(Source),
    Markup(Markup),
    List(Vec<Link>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PageId(salsa::InternId);

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct Nav {
    pub modules: Vec<Link>,
    pub fixities: Vec<Link>,
    pub funcs: Vec<Link>,
    pub statics: Vec<Link>,
    pub consts: Vec<Link>,
    pub types: Vec<Link>,
    pub classes: Vec<Link>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Link {
    name: String,
    page: PageId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Source {
    file: RelativePathBuf,
    line: usize,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Code {
    tokens: Vec<Token>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Token {
    class: &'static str,
    text: String,
}

impl Page {
    pub fn new(path: hir::Path, title: impl Into<String>) -> Self {
        Self {
            path,
            title: title.into(),
            sections: Vec::new(),
        }
    }

    pub fn render(&self, path: &RelativePath) -> io::Result<()> {
        let path = path.join(self.path.to_string());
        let mut w = std::fs::File::create(path.to_path("."))?;

        write!(
            w,
            r##"
                <!DOCTYPE html>
                <html>
                <head>
                    <title>{}</title>
                </head>
                <body>
                    <main>
            "##,
            self.title,
        )?;

        for section in &self.sections {
            section.render(&mut w)?;
        }

        write!(w, "</main></body></html>")
    }
}

impl Section {
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            entries: Vec::new(),
        }
    }

    pub fn render(&self, w: &mut dyn Write) -> io::Result<()> {
        write!(w, r#"<section>"#)?;
        write!(w, r#"<h2>{}</h2>"#, self.title)?;

        for entry in &self.entries {
            entry.render(w)?;
        }

        write!(w, "</section>")
    }
}

impl Entry {
    pub fn new() -> Self {
        Self { elems: Vec::new() }
    }

    pub fn title(&mut self, title: impl Into<String>) {
        self.elems.push(EntryElem::Title(title.into()));
    }

    pub fn source(&mut self, file: RelativePathBuf, line: usize) {
        self.elems.push(EntryElem::Source(Source { file, line }));
    }

    pub fn markup(&mut self, markup: Markup) {
        self.elems.push(EntryElem::Markup(markup));
    }

    pub fn code(&mut self) -> &mut Code {
        self.elems.push(EntryElem::Code(Code::new()));

        match self.elems.last_mut() {
            | Some(EntryElem::Code(code)) => code,
            | _ => unreachable!(),
        }
    }

    pub fn list(&mut self) -> &mut Vec<Link> {
        self.elems.push(EntryElem::List(Vec::new()));

        match self.elems.last_mut() {
            | Some(EntryElem::List(code)) => code,
            | _ => unreachable!(),
        }
    }

    pub fn render(&self, w: &mut dyn Write) -> io::Result<()> {
        write!(w, r#"<div class="entry">"#)?;

        for elem in &self.elems {
            elem.render(w)?;
        }

        write!(w, "</div>")
    }
}

impl EntryElem {
    pub fn render(&self, w: &mut dyn Write) -> io::Result<()> {
        match self {
            | EntryElem::Title(title) => write!(w, r#"<h3>{}</h3>"#, title),
            | EntryElem::Source(src) => {
                write!(
                    w,
                    r#"<aside class="source">Defined in <a href="{}">{}:{}</a></aside>"#,
                    "",
                    src.file.display(),
                    src.line
                )
            },
            | EntryElem::List(links) => {
                write!(w, "<ul>")?;

                for link in links {
                    write!(w, "<li>")?;
                    link.render(w)?;
                    write!(w, "</li>")?;
                }

                write!(w, "</ul>")
            },
            | EntryElem::Code(code) => code.render(w),
            | EntryElem::Markup(markup) => {
                write!(w, r#"<div class="comment">{}</div>"#, "")
            },
        }
    }
}

impl Link {
    pub fn new(name: impl Into<String>, page: PageId) -> Self {
        Self {
            name: name.into(),
            page,
        }
    }

    pub fn render(&self, w: &mut dyn Write) -> io::Result<()> {
        write!(w, r#"<a href="{}">{}</a>"#, "", self.name)
    }
}

impl Code {
    pub fn new() -> Self {
        Code { tokens: Vec::new() }
    }

    pub fn text(&mut self, t: impl Into<String>) {
        self.tokens.push(Token {
            class: "text",
            text: t.into(),
        });
    }

    pub fn keyword(&mut self, w: &'static str) {
        self.tokens.push(Token {
            class: "keyword",
            text: w.into(),
        });
    }

    pub fn constant(&mut self, c: impl Into<String>) {
        self.tokens.push(Token {
            class: "constant",
            text: c.into(),
        });
    }

    pub fn symbol(&mut self, f: impl Into<String>) {
        self.tokens.push(Token {
            class: "symbol",
            text: f.into(),
        });
    }

    pub fn func(&mut self, f: impl Into<String>) {
        self.tokens.push(Token {
            class: "func",
            text: f.into(),
        });
    }

    pub fn ident(&mut self, f: impl Into<String>) {
        self.tokens.push(Token {
            class: "ident",
            text: f.into(),
        });
    }

    pub fn class(&mut self, f: impl Into<String>) {
        self.tokens.push(Token {
            class: "class",
            text: f.into(),
        });
    }

    pub fn type_(&mut self, f: impl Into<String>) {
        self.tokens.push(Token {
            class: "type",
            text: f.into(),
        });
    }

    pub fn render(&self, w: &mut dyn Write) -> io::Result<()> {
        write!(w, "<code>")?;

        for token in &self.tokens {
            if token.class == "text" {
                write!(w, "{} ", token.text)?;
            } else {
                write!(w, r#"<span class="{}">{}</span>"#, token.class, token.text)?;
            }
        }

        write!(w, "</code>")
    }
}

impl salsa::InternKey for PageId {
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }

    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }
}

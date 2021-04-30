pub struct Markup {
    parts: Vec<Part>,
}

enum Part {
    Text(String, Styles),
    Header(String, u8),
    Paragraph(Markup),
    Line,
}

#[derive(Clone, Copy)]
pub struct Styles(u8);

pub struct ParagraphBuilder {
    markup: Markup,
    parts: Vec<Part>,
}

pub trait MarkupRenderer {
    fn render_text(&mut self, text: &String, styles: Styles);
    fn render_header(&mut self, text: &String, level: u8);
    fn render_line(&mut self);
    fn render_newline(&mut self);

    fn render_paragraph(&mut self, inner: &Markup) {
        self.render_markup(inner);
    }

    fn render_markup(&mut self, markup: &Markup) {
        let mut start = true;
        let mut block = false;

        for part in &markup.parts {
            match part {
                | Part::Text(text, styles) => {
                    if block {
                        self.render_newline();
                    }

                    self.render_text(text, *styles);
                    block = false;
                },
                | Part::Header(text, level) => {
                    if !start {
                        self.render_newline();
                    }

                    self.render_header(text, *level);
                    block = true;
                },
                | Part::Line => {
                    if !start {
                        self.render_newline();
                    }

                    self.render_line();
                    block = true;
                },
                | Part::Paragraph(inner) => {
                    if !start {
                        self.render_newline();
                    }

                    self.render_paragraph(inner);
                    block = true;
                },
            }

            start = false;
        }
    }
}

impl Markup {
    pub const fn new() -> Self {
        Markup { parts: Vec::new() }
    }

    pub fn text(mut self, text: impl Into<String>, styles: Styles) -> Self {
        self.parts.push(Part::Text(text.into(), styles));
        self
    }

    pub fn header(mut self, text: impl Into<String>, level: u8) -> Self {
        self.parts.push(Part::Header(text.into(), level));
        self
    }

    pub fn line(mut self) -> Self {
        self.parts.push(Part::Line);
        self
    }

    pub fn paragraph(self) -> ParagraphBuilder {
        ParagraphBuilder {
            markup: self,
            parts: Vec::new(),
        }
    }
}

impl ParagraphBuilder {
    pub fn finish(self) -> Markup {
        let Self { mut markup, parts } = self;

        markup.parts.push(Part::Paragraph(Markup { parts }));
        markup
    }

    pub fn text(mut self, text: String, styles: Styles) -> Self {
        self.parts.push(Part::Text(text, styles));
        self
    }

    pub fn header(mut self, text: String, level: u8) -> Self {
        self.parts.push(Part::Header(text, level));
        self
    }

    pub fn line(mut self) -> Self {
        self.parts.push(Part::Line);
        self
    }
}

impl Styles {
    pub const NONE: Self = Styles(1 << 0);
    pub const BOLD: Self = Styles(1 << 1);
    pub const ITALIC: Self = Styles(1 << 2);
    pub const UNDERLINE: Self = Styles(1 << 3);

    pub fn bold(self) -> bool {
        self.0 & (1 << 1) != 0
    }

    pub fn italic(self) -> bool {
        self.0 & (1 << 2) != 0
    }

    pub fn underline(self) -> bool {
        self.0 & (1 << 3) != 0
    }
}

impl std::ops::BitOr for Styles {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

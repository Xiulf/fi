// use encode::{Encode, Decode};

static mut GLOBAL_SYMBOL_INTERNER: SymbolInterner = SymbolInterner::new();

// #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(usize);

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolData(Box<str>);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: diagnostics::Span,
}

impl Symbol {
    pub fn new(name: impl Into<Box<str>>) -> Symbol {
        unsafe { GLOBAL_SYMBOL_INTERNER.intern(SymbolData(name.into())) }
    }

    pub fn dummy() -> Symbol {
        unsafe { GLOBAL_SYMBOL_INTERNER.intern(SymbolData(Default::default())) }
    }

    pub const fn from_usize(src: usize) -> Symbol {
        Symbol(src)
    }

    pub fn as_static_str(self) -> &'static str {
        unsafe {
            let boxed_str = &GLOBAL_SYMBOL_INTERNER.data[self.0].0;
            let slice = std::slice::from_raw_parts(boxed_str.as_ptr(), boxed_str.len());

            std::str::from_utf8_unchecked(slice)
        }
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        <str as std::fmt::Display>::fmt(&(**self).0, f)
    }
}

impl std::ops::Deref for Symbol {
    type Target = SymbolData;

    fn deref(&self) -> &SymbolData {
        unsafe { &GLOBAL_SYMBOL_INTERNER.data[self.0] }
    }
}

impl std::ops::Deref for SymbolData {
    type Target = str;

    fn deref(&self) -> &str {
        &*self.0
    }
}

pub struct SymbolInterner {
    data: Vec<SymbolData>,
}

impl SymbolInterner {
    pub const fn new() -> SymbolInterner {
        SymbolInterner { data: Vec::new() }
    }

    fn intern(&mut self, value: SymbolData) -> Symbol {
        if let Some(idx) = self.data.iter().position(|d| d == &value) {
            Symbol(idx)
        } else {
            self.data.push(value);

            Symbol(self.data.len() - 1)
        }
    }
}

impl Ident {
    pub fn dummy(file: diagnostics::FileId) -> Ident {
        Ident {
            symbol: Symbol::default(),
            span: diagnostics::Span::empty(file),
        }
    }

    pub fn to_string(&self) -> String {
        self.symbol.to_string()
    }
}

impl<D> parser::parse::Parse<D> for Ident {
    fn parse(input: parser::parse::ParseStream<D>) -> Result<Self, diagnostics::Diagnostic> {
        let ident = input.parse::<parser::ident::Ident>()?;

        Ok(Ident {
            span: ident.span,
            symbol: Symbol::new(ident.name),
        })
    }
}

impl parser::token::Token for Ident {
    fn peek(cursor: parser::buffer::Cursor) -> bool {
        parser::ident::Ident::peek(cursor)
    }

    fn display() -> &'static str {
        "identifier"
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        <Symbol as std::fmt::Display>::fmt(&self.symbol, f)
    }
}

impl std::hash::Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (&*self.symbol).hash(state);
    }
}

impl diagnostics::Spanned for Ident {
    fn span(&self) -> diagnostics::Span {
        self.span
    }
}

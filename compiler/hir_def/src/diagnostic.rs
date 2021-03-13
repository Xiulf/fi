use crate::in_file::InFile;
use std::any::Any;
use std::fmt;
use syntax::ptr::SyntaxNodePtr;

pub trait Diagnostic: Any + Send + Sync + fmt::Debug + 'static {
    fn message(&self) -> String;
    fn display_source(&self) -> InFile<SyntaxNodePtr>;
    fn as_any(&self) -> &(dyn Any + Send + 'static);
}

pub struct DiagnosticSink<'a> {
    callbacks: Vec<Box<dyn FnMut(&dyn Diagnostic) -> Result<(), ()> + 'a>>,
    default_callback: Box<dyn FnMut(&dyn Diagnostic) + 'a>,
}

pub struct DiagnosticSinkBuilder<'a> {
    callbacks: Vec<Box<dyn FnMut(&dyn Diagnostic) -> Result<(), ()> + 'a>>,
}

impl<'a> DiagnosticSink<'a> {
    pub fn new<F: FnMut(&dyn Diagnostic) + 'a>(default_callback: F) -> Self {
        DiagnosticSink {
            callbacks: Vec::new(),
            default_callback: Box::new(default_callback),
        }
    }

    pub fn push(&mut self, d: impl Diagnostic) {
        self._push(&d);
    }

    fn _push(&mut self, d: &dyn Diagnostic) {
        for cb in &mut self.callbacks {
            match cb(d) {
                | Ok(()) => return,
                | Err(()) => (),
            }
        }

        (self.default_callback)(d)
    }
}

impl<'a> DiagnosticSinkBuilder<'a> {
    pub fn new() -> Self {
        DiagnosticSinkBuilder { callbacks: Vec::new() }
    }

    pub fn on<D: Diagnostic, F: FnMut(&D) + 'a>(mut self, mut cb: F) -> Self {
        let cb = move |diag: &dyn Diagnostic| match diag.as_any().downcast_ref::<D>() {
            | Some(d) => {
                cb(d);
                Ok(())
            },
            | None => Err(()),
        };

        self.callbacks.push(Box::new(cb));
        self
    }

    pub fn build<F: FnMut(&dyn Diagnostic) + 'a>(self, default_callback: F) -> DiagnosticSink<'a> {
        DiagnosticSink {
            callbacks: self.callbacks,
            default_callback: Box::new(default_callback),
        }
    }
}

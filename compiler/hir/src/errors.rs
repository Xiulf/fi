use crate::resolve::ModuleId;
use diagnostics::{Diagnostic, Severity};
use syntax::symbol::Ident;

pub enum Error {
    DuplicateDecl(Ident),
    UnknownValue(Option<Ident>, Ident),
    UnknownType(Option<Ident>, Ident),
    CyclicModule(Ident),
    NotExported(ModuleId, Ident),
    NotContained(ModuleId, Ident),
    UnknownModule(Ident),
}

impl Into<Diagnostic> for Error {
    fn into(self) -> Diagnostic {
        match self {
            Error::DuplicateDecl(name) => Diagnostic::new(
                Severity::Error,
                0003,
                format!("duplicate declaration '{}'", name),
            )
            .label(Severity::Error, name.span, None::<String>),
            Error::UnknownValue(Some(obj), name) => Diagnostic::new(
                Severity::Error,
                0004,
                format!("unknown value '{}.{}'", obj, name),
            )
            .label(Severity::Error, obj.span.to(name.span), None::<String>),
            Error::UnknownValue(None, name) => Diagnostic::new(
                Severity::Error,
                0004,
                format!("unknown value '{}'", name),
            )
            .label(Severity::Error, name.span, None::<String>),
            Error::UnknownType(Some(obj), name) => Diagnostic::new(
                Severity::Error,
                0004,
                format!("unknown type '{}.{}'", obj, name),
            )
            .label(Severity::Error, obj.span.to(name.span), None::<String>),
            Error::UnknownType(None, name) => Diagnostic::new(
                Severity::Error,
                0004,
                format!("unknown type '{}'", name),
            )
            .label(Severity::Error, name.span, None::<String>),
            Error::CyclicModule(name) => Diagnostic::new(
                Severity::Error,
                0019,
                format!("cyclic module '{}'", name),
            )
            .label(Severity::Error, name.span, None::<String>),
            Error::NotExported(module, name) => Diagnostic::new(
                Severity::Error,
                0020,
                format!("module '{}' does not exports name '{}'", module, name),
            )
            .label(Severity::Error, name.span, None::<String>),
            Error::NotContained(module, name) => Diagnostic::new(
                Severity::Error,
                0021,
                format!("module '{}' does not contain item '{}'", module, name),
            )
            .label(Severity::Error, name.span, None::<String>),
            Error::UnknownModule(name) => Diagnostic::new(
                Severity::Error,
                0022,
                format!("unknown module '{}'", name),
            )
            .label(Severity::Error, name.span, None::<String>),
        }
    }
}

use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;

pub type Result<T> = std::result::Result<T, Diagnostic<FileId>>;

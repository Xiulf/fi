use diagnostics::Diagnostic;

pub type Result<T> = std::result::Result<T, Diagnostic>;
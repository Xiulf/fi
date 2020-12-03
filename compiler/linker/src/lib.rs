pub mod linker;
use std::ffi::OsStr;
use std::process::Command;

pub fn get_linker(_target: &target_lexicon::Triple) -> Box<dyn linker::Linker> {
    // currently only GccLinker is available
    Box::new(linker::GccLinker::new(Command::new("cc"), false)) as Box<dyn linker::Linker>
}

pub fn extension(output_type: LinkOutputType, target: &target_lexicon::Triple) -> &'static OsStr {
    OsStr::new(match target.operating_system {
        target_lexicon::OperatingSystem::Windows => match output_type {
            LinkOutputType::Exe => "exe",
            LinkOutputType::Lib => "lib",
            LinkOutputType::Dylib => "dll",
        },
        target_lexicon::OperatingSystem::MacOSX { .. } => match output_type {
            LinkOutputType::Exe => "app",
            LinkOutputType::Lib => "a",
            LinkOutputType::Dylib => "dylib",
        },
        _ => match output_type {
            LinkOutputType::Exe => "",
            LinkOutputType::Lib => "a",
            LinkOutputType::Dylib => "so",
        },
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkOutputType {
    Exe,
    Lib,
    Dylib,
}

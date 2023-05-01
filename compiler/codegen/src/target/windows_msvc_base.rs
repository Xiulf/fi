use target_lexicon::{Architecture, BinaryFormat, Environment, OperatingSystem, Triple, Vendor};

use super::{LinkerFlavor, Target};

pub fn opts() -> Target {
    Target {
        triple: Triple {
            architecture: Architecture::Unknown,
            vendor: Vendor::Pc,
            operating_system: OperatingSystem::Windows,
            environment: Environment::Msvc,
            binary_format: BinaryFormat::Coff,
        },
        linker_flavor: LinkerFlavor::Msvc,
        dll_prefix: "",
        ..Default::default()
    }
}

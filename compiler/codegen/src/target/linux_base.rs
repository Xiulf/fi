use target_lexicon::{Architecture, BinaryFormat, Environment, OperatingSystem, Triple, Vendor};

use super::{LinkerFlavor, Target};

pub fn opts() -> Target {
    Target {
        triple: Triple {
            architecture: Architecture::Unknown,
            vendor: Vendor::Unknown,
            operating_system: OperatingSystem::Linux,
            environment: Environment::Gnu,
            binary_format: BinaryFormat::Elf,
        },
        linker_flavor: LinkerFlavor::Ld,
        ..Default::default()
    }
}

use target_lexicon::{OperatingSystem, Triple};

mod linux_base;
mod windows_msvc_base;

#[derive(Debug, PartialEq, Eq)]
pub struct Target {
    pub triple: Triple,
    pub cpu: &'static str,
    pub features: &'static str,
    pub dll_prefix: &'static str,
    pub linker_flavor: LinkerFlavor,
    pub pre_link_args: Vec<&'static str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinkerFlavor {
    Ld,
    Msvc,
}

impl Default for Target {
    fn default() -> Self {
        Self {
            triple: Triple::unknown(),
            cpu: "generic",
            features: "",
            dll_prefix: "lib",
            linker_flavor: LinkerFlavor::Ld,
            pre_link_args: Vec::new(),
        }
    }
}

macro_rules! targets {
    ($(($triple:literal, $target:ident)),* $(,)?) => {
        $(mod $target;)*

        const TARGETS: &[&str] = &[$($triple),*];

        fn load_target(triple: &Triple) -> Target {
            match triple.to_string().as_str() {
                $($triple => return $target::target(),)*
                _ => {},
            }

            let linker_flavor = match triple.environment {
                target_lexicon::Environment::Msvc => LinkerFlavor::Msvc,
                _ => LinkerFlavor::Ld,
            };

            let dll_prefix = match triple.operating_system {
                target_lexicon::OperatingSystem::Windows => "",
                _ => "lib",
            };

            Target {
                triple: triple.clone(),
                dll_prefix,
                linker_flavor,
                ..Default::default()
            }
        }

        pub fn get_targets() -> &'static [&'static str] {
            TARGETS
        }
    };
}

targets! {
    ("x86_64-pc-windows-msvc", x86_64_pc_windows_msvc),
    ("x86_64-unknown-linux-gnu", x86_64_unknown_linux_gnu),
}

impl Target {
    pub fn load(triple: &Triple) -> Target {
        load_target(triple)
    }

    pub fn host_target() -> Target {
        Self::load(&Triple::host())
    }

    pub fn is_windows(&self) -> bool {
        matches!(self.triple.operating_system, OperatingSystem::Windows)
    }
}

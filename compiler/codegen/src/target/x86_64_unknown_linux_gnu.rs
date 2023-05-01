use target_lexicon::Architecture;

use super::Target;

pub fn target() -> Target {
    let mut target = Target {
        cpu: "x86-64",
        ..super::linux_base::opts()
    };

    target.triple.architecture = Architecture::X86_64;
    target
}

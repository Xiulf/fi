pub use codegen::OptimizationLevel;

pub struct Options {
    pub target: codegen::target::Target,
    pub target_dir: std::path::PathBuf,
    pub optimization_level: OptimizationLevel,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            target: codegen::target::Target::host_target(),
            target_dir: std::env::current_dir().unwrap().join("target"),
            optimization_level: codegen::OptimizationLevel::Default,
        }
    }
}

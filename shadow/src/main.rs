fn main() {
    let opts = driver::Opts {
        entry: "test/src/main.shade".into(),
        target_dir: "test/target".into(),
    };

    driver::build(opts);
}

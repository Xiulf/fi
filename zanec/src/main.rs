fn main() {
    let opts = driver::Opts {
        entry: "test/src/main.zane".into(),
        target_dir: "test/target".into(),
    };

    driver::build(opts);
}

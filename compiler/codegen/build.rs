fn main() {
    println!("cargo:rustc-link-search=/mnt/f/Language/shade/target/release");
    println!("cargo:rustc-link-lib=codegen_cranelift");
}

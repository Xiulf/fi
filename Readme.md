# Shade
A programming language using Ruby style syntax. This project is still very much WIP. So far the language can do everything C can and it has Rust like enums.

## Todo
- [ ] Windows support
- [ ] Pattern matching
- [ ] Interfaces, similar to Rust traits or Swift protocols.

## Building
To build the compiler you will need `cargo`, the easiest way to install it is via https://rustup.rs. With cargo installed run:
```shell
$ cargo build
```
To globally install the compiler use:
```shell
$ cargo install
```
To run the test example:
```shell
$ cargo run -- build test --output-type=bin
$ ./test/target/test
```
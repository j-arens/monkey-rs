An interpreter for [Thorsten Ball's Monkey programming langauge](https://monkeylang.org/) implemented in Rust.

---

## Building

Dev (unoptimized).

```sh
cargo build
```

Release (optimized).

```sh
cargo build --release
```

## Usage

Run the REPL.

```sh
# run via cargo
cargo run

# or run the built binary
monkey-rs
```

Optionally configure the REPL to output tokens generated from lexing.

```sh
# run via cargo
cargo run -- --output tokens

# or run the built binary
monkey-rs --output tokens
```

Or output a formatted abstract syntax tree generated from parsing.

```sh
# run via cargo
cargo run -- --output ast

# or run the built binary
monkey-rs --output ast
```

## Testing

Run all tests.

```sh
cargo test
```

Or run a single suite of tests.

```sh
cargo test built_in
cargo test eval
cargo test lexer
cargo test parser
cargo test scope
```

## Formatting

Most of the features defined in `rustfmt.toml` require the Rust nightly channel. To apply, make sure nightly is installed in your toolchain, and run the following:

```sh
cargo +nightly fmt
```

# TypeCrypt Rust (Production Branch)

This crate aims to provide a secure and high-performance implementation of TypeCrypt.

Code is organized under `src/` and will expose a library crate for embedding into applications.

Run the unit tests with:

```bash
cargo test
```

## Command-line tool

The `crypt_tool` binary can be used to encrypt or decrypt data keyed by a
TypeCrypt schema. Invoke it with:

```bash
cargo run --bin crypt_tool -- encrypt|decrypt <type> [hex]
```

Supported `<type>` options include:

- `int`, `str`, and `bool`
- `pair` (defaults to `pair<int,bool>`)
- `list` (defaults to `list<int>`)
- `list<T>` such as `list<int>` or `list<bool>`

For example, to decrypt a ciphertext that was encrypted with `list<int>`:

```bash
cargo run --bin crypt_tool -- decrypt list<int> <hex-ciphertext>
```

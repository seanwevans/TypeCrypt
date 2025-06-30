# TypeCrypt 🔐

**TypeCrypt** is an encryption system where *types themselves are the key*. Decryption is only possible if a submitted value *resolves to the expected type*.

This repo contains four coordinated implementations:

- **Haskell** – the *Theory Branch*: formal specification, correctness, and type-driven logic.
- **Rust** – the *Production Branch*: hardened system for real-world usage, throughput-optimized.
- **Zig** – the *Experimental Branch*: compile-time metaprogramming, radical flexibility, and rapid prototyping.
- **Racket** – the *Assembly Branch*: emits x86\_64 assembly for low-level exploration.

---

## 🔍 Concept

In TypeCrypt, types are not just annotations — they are *structural constraints* that serve as decryption keys. A ciphertext encrypted under a type `T` can only be decrypted by a value of type `T`.

This flips conventional cryptography on its head: instead of using values to unlock data, you must *satisfy a type* to access it.

Each implementation derives a symmetric key directly from a `Type` using the `keyFromType` function.  Both the Haskell and Rust branches now hash a canonical byte encoding of the type with SHA‑256.  Data is encrypted with ChaCha20‑Poly1305 and the `encrypt` function prepends a random nonce to the ciphertext.  `decrypt` verifies that a provided `Value` matches the expected `Type` before attempting to decrypt.  Although the key derivation is still simplistic and **not** ready for real use, it demonstrates the idea of tying access to type satisfaction.

## Development Workflow

Coding style guidelines and test commands for each implementation are documented in [docs/development.md](docs/development.md). Consult that file for instructions on formatting Haskell, Rust, and Zig code as well as how to run each test suite.

For details on how changes should propagate between the theory, production, and experimental branches, read [AGENTS.md](AGENTS.md).

## Building & Testing

Each language implementation lives in its own subdirectory. To compile and verify them run:

- **Haskell**
  ```bash
  cd haskell
  cabal build
  cabal test
  ```
- **Rust**
  ```bash
  cd rust
  cargo test
  ```
- **Zig**
  ```bash
  cd zig
  zig build test
  ```
- **Racket**
  ```bash
  cd racket
  racket main.rkt
  ```

For convenience, you can run all of the above with:

```bash
./run_all_tests.sh
```

## License

TypeCrypt is released under the [MIT License](LICENSE).


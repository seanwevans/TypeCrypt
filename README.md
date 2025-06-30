# TypeCrypt 🔐

**TypeCrypt** is an encryption system where *types themselves are the key*. Decryption is only possible if a submitted value *resolves to the expected type*.

This repo contains three coordinated implementations:

- **Haskell** – the *Theory Branch*: formal specification, correctness, and type-driven logic.
- **Rust** – the *Production Branch*: hardened system for real-world usage, throughput-optimized.
- **Zig** – the *Experimental Branch*: compile-time metaprogramming, radical flexibility, and rapid prototyping.

---

## 🔍 Concept

In TypeCrypt, types are not just annotations — they are *structural constraints* that serve as decryption keys. A ciphertext encrypted under a type `T` can only be decrypted by a value of type `T`.

This flips conventional cryptography on its head: instead of using values to unlock data, you must *satisfy a type* to access it.

---

## Development Workflow

Coding style guidelines and test commands for each implementation are documented in [docs/development.md](docs/development.md). Consult that file for instructions on formatting Haskell, Rust, and Zig code as well as how to run each test suite.

For details on how changes should propagate between the theory, production, and experimental branches, read [AGENTS.md](AGENTS.md).



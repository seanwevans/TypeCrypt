# TypeCrypt: Roadmap

This document outlines the plan for developing TypeCrypt across four branches: Theory (Haskell), Production (Rust), Experimental (Zig), and Assembly (Racket).

TypeCrypt merges type theory with cryptography. This roadmap serves as a living
guide for coordinating the different language implementations. Items are updated
as work progresses and corresponding GitHub issues are resolved.

---

## 🧵 Project Threads

### 🔵 Haskell (Theory Branch)

> _Purpose_: Define the formal semantics of type-as-key encryption.

**Goals:**
- [ ] Define core `Type` and `Value` algebra using GADTs
- [ ] Implement symbolic matching: `matches :: Value -> Type -> Bool`
- [ ] Implement `encrypt :: Type -> ByteString -> Ciphertext`
- [ ] Implement `decrypt :: Type -> Value -> Ciphertext -> Maybe ByteString`
- [ ] Add QuickCheck-based verification
- [ ] Export canonical spec in `spec/Core.hs`
- [ ] Design exportable IR for transpilation to C

---

### 🔴 Rust (Production Branch)

> _Purpose_: Harden the system for real-world deployment and performance.

**Goals:**
- [ ] Mirror Haskell types with Rust enums/traits
- [ ] Implement safe AEAD encryption via `ring` or `sodiumoxide`
- [ ] Build derive-macros for `TypeKey` via reflection
- [ ] Benchmarks with `criterion`
- [ ] Ensure `no_std` compatibility
- [ ] Harden against misuse (e.g., invalid keys, malleability)
- [ ] Optional: WASM-compatible build

---

### 🟡 Zig (Experimental Branch)

> _Purpose_: Test wild ideas, type unification, and inline VM logic.

**Goals:**
- [ ] Implement `Type` as `comptime struct/union` variant
- [ ] Add schema validation via `@Type()` + recursive matcher
- [ ] Implement in-place AEAD encryption (minimal deps)
- [ ] Test compile-time `typehash()` logic
- [ ] Build Zig-native syntax for structural types
- [ ] Explore self-serializing type descriptors

---

### 🟣 Racket (Assembly Branch)

> _Purpose_: Explore direct assembly generation and low-level optimizations.

**Goals:**
- [ ] Emit minimal x86_64 assembly for sample operations
- [ ] Provide a test harness for verifying generated code
- [ ] Integrate with other branches via shared IR

---

## 🧮 Long-Term Goals

- [ ] Transpile Haskell IR to:
  - [ ] Verified C
  - [ ] Zig
  - [ ] WASM
- [ ] Integrate with formal verification (e.g., Agda or F*)
- [ ] Use TypeCrypt for:
  - [ ] Smart contract guards
  - [ ] Encrypted config validation
  - [ ] Embedded system keylocks

---

## 🔁 Development Strategy

- Haskell drives spec and logic
- Rust implements hardened infrastructure
- Zig tests radical or low-level enhancements
- Racket explores direct assembly generation

Each branch feeds into the others iteratively to improve correctness, performance, and flexibility.

## 🤝 Contributing

We welcome pull requests for any of the language branches. Before submitting,
please read `docs/development.md` for formatting requirements and run
`./run_all_tests.sh` to ensure all suites pass.

---

## 📬 Contact

Project steward: *community driven*
Maintained by: *contributors on GitHub*

# Development Guide

This document summarizes the basic coding style used in this repository and how to run the test suites for each language implementation.

## Coding Style

### Haskell
- Use four spaces for indentation; no tabs.
- Run `ormolu -m inplace` before committing to ensure consistent formatting.

### Rust
- Use `rustfmt` with the default configuration. Formatting can be run with `cargo fmt`.

### Zig
- Format all Zig files with `zig fmt`.

## Running Tests

### Haskell
Run the QuickCheck suite via cabal:

```bash
cd haskell
cabal test
```

### Rust
Execute the cargo tests:

```bash
cd rust
cargo test
```

### Zig
The Zig example currently just builds an executable. Run:

```bash
cd zig
zig build
```

## Multi-Language Workflow

This project coordinates changes across the Haskell, Rust and Zig implementations. See `AGENTS.md` in the repository root for details on how updates should flow between these directories.

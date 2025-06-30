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

### Racket
- Use `raco fmt` for formatting. Run `raco fmt <file>` before committing.

## Running Tests

### Haskell
Run the QuickCheck suite via cabal. Make sure the package index is up to date:

```bash
cd haskell
cabal update
cabal test
```

### Rust
Execute the cargo tests:

```bash
cd rust
cargo test
```

### Zig
Run the Zig unit tests via the build system:

```bash
cd zig
zig build test
```

### Racket
Execute the rackunit tests:

```bash
cd racket
raco test test.rkt
```

## Multi-Language Workflow

This project coordinates changes across the Haskell, Rust, Zig and Racket implementations. See `AGENTS.md` in the repository root for details on how updates should flow between these directories.

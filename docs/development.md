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

## Toolchain Requirements for `run_all_tests.sh`

The `./run_all_tests.sh` script orchestrates all test suites and expects the
following executables to be available in your `PATH`:

- `cabal` – Haskell build tool
- `cargo` – Rust package manager
- `zig` – Zig compiler
- `raco` – Racket command runner

If any of these tools are missing the corresponding language tests will be
skipped.

## Multi-Language Workflow

This project coordinates changes across the Haskell, Rust, Zig and Racket implementations. See `AGENTS.md` in the repository root for details on how updates should flow between these directories.

## Continuous Integration

The workflow in `.github/workflows/ci.yml` installs the Haskell, Rust, Zig and Racket toolchains and then runs `./run_all_tests.sh`. After the tests finish it writes a JSONL log entry with the pass/fail result and uploads that file as a build artifact. The log format matches the example in `AGENTS.md`.

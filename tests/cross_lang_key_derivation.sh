#!/usr/bin/env bash
# Validate that key derivation is consistent across implementations
set -euo pipefail

dir="$(cd "$(dirname "$0")/.." && pwd)"
cd "$dir"

# Build all implementations
if ! command -v cabal >/dev/null 2>&1; then
  echo "Skipping cross-language test: cabal not installed"
  exit 0
fi
if ! command -v cargo >/dev/null 2>&1; then
  echo "Skipping cross-language test: cargo not installed"
  exit 0
fi
if ! command -v zig >/dev/null 2>&1; then
  echo "Skipping cross-language test: zig not installed"
  exit 0
fi

( cd haskell && cabal build )
( cd rust && cargo build )
( cd zig && zig build )

# Run helpers
HS_OUTPUT=$(cd haskell && cabal exec runghc -- -isrc ../tests/dump_hs.hs)
RS_OUTPUT=$(cargo run --quiet --manifest-path rust/Cargo.toml --bin dump_keys)
ZIG_OUTPUT=$(zig run tests/dump_zig.zig)

# compare outputs
if [ "$HS_OUTPUT" = "$RS_OUTPUT" ] && [ "$HS_OUTPUT" = "$ZIG_OUTPUT" ]; then
  echo "cross-lang key derivation consistent"
else
  echo "Haskell:"; echo "$HS_OUTPUT"; echo "Rust:"; echo "$RS_OUTPUT"; echo "Zig:"; echo "$ZIG_OUTPUT"
  echo "Mismatch between implementations" >&2
  exit 1
fi

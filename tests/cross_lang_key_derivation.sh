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

# Cross-language encryption roundtrip
PLAINTEXT="cross-test"
HS_CT=$(cd haskell && cabal exec runghc -- -isrc ../tests/crypt_hs.hs encrypt)
RS_CT=$(cargo run --quiet --manifest-path rust/Cargo.toml --bin crypt_tool encrypt)
ZIG_CT=$(zig run tests/crypt_zig.zig -- encrypt)
HS_DEC_RS=$(cargo run --quiet --manifest-path rust/Cargo.toml --bin crypt_tool decrypt "$HS_CT")
RS_DEC_HS=$(cd haskell && cabal exec runghc -- -isrc ../tests/crypt_hs.hs decrypt "$RS_CT")
HS_DEC_ZG=$(cd haskell && cabal exec runghc -- -isrc ../tests/crypt_hs.hs decrypt "$ZIG_CT")
RS_DEC_ZG=$(cargo run --quiet --manifest-path rust/Cargo.toml --bin crypt_tool decrypt "$ZIG_CT")
ZIG_DEC_HS=$(zig run tests/crypt_zig.zig -- decrypt "$HS_CT")
ZIG_DEC_RS=$(zig run tests/crypt_zig.zig -- decrypt "$RS_CT")
if [ "$HS_DEC_RS" != "$PLAINTEXT" ] || [ "$RS_DEC_HS" != "$PLAINTEXT" ] || \
   [ "$HS_DEC_ZG" != "$PLAINTEXT" ] || [ "$RS_DEC_ZG" != "$PLAINTEXT" ] || \
   [ "$ZIG_DEC_HS" != "$PLAINTEXT" ] || [ "$ZIG_DEC_RS" != "$PLAINTEXT" ]; then
  echo "Cross-language encryption mismatch" >&2
  exit 1
fi

# Existing key derivation helpers
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

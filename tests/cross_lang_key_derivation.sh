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

# Cross-language encryption roundtrip for multiple types
PLAINTEXT="cross-test"
for T in int str pair; do
  HS_CT=$(cd haskell && cabal exec runghc -- -isrc ../tests/crypt_hs.hs encrypt "$T")
  RS_CT=$(cargo run --quiet --manifest-path rust/Cargo.toml --bin crypt_tool encrypt "$T")
  ZIG_CT=$(zig run tests/crypt_zig.zig -- encrypt "$T")

  HS_DEC_RS=$(cargo run --quiet --manifest-path rust/Cargo.toml --bin crypt_tool decrypt "$T" "$HS_CT")
  RS_DEC_HS=$(cd haskell && cabal exec runghc -- -isrc ../tests/crypt_hs.hs decrypt "$T" "$RS_CT")
  HS_DEC_ZG=$(cd haskell && cabal exec runghc -- -isrc ../tests/crypt_hs.hs decrypt "$T" "$ZIG_CT")
  RS_DEC_ZG=$(cargo run --quiet --manifest-path rust/Cargo.toml --bin crypt_tool decrypt "$T" "$ZIG_CT")
  ZIG_DEC_HS=$(zig run tests/crypt_zig.zig -- decrypt "$T" "$HS_CT")
  ZIG_DEC_RS=$(zig run tests/crypt_zig.zig -- decrypt "$T" "$RS_CT")

  if [ "$HS_DEC_RS" != "$PLAINTEXT" ] || [ "$RS_DEC_HS" != "$PLAINTEXT" ] || \
     [ "$HS_DEC_ZG" != "$PLAINTEXT" ] || [ "$RS_DEC_ZG" != "$PLAINTEXT" ] || \
     [ "$ZIG_DEC_HS" != "$PLAINTEXT" ] || [ "$ZIG_DEC_RS" != "$PLAINTEXT" ]; then
    echo "Cross-language encryption mismatch for type $T" >&2
    exit 1
  fi

  if [ "$T" != "int" ]; then
    set +e
    ZIG_WRONG_OUTPUT=$(zig run tests/crypt_zig.zig -- decrypt int "$ZIG_CT" 2>/dev/null)
    ZIG_WRONG_STATUS=$?
    set -e
    if [ "$ZIG_WRONG_STATUS" -eq 0 ]; then
      echo "Zig CLI accepted mismatched schema for ciphertext generated as $T" >&2
      echo "Unexpected output: $ZIG_WRONG_OUTPUT" >&2
      exit 1
    fi
  fi
done

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

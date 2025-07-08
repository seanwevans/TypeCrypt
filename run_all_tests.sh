#!/usr/bin/env bash
# Run all TypeCrypt test suites
set -e

if command -v cabal >/dev/null 2>&1; then
  ( cd haskell && cabal update && cabal test )
else
  echo "Skipping Haskell tests: cabal not installed"
fi

if command -v cargo >/dev/null 2>&1; then
  ( cd rust && cargo test )
else
  echo "Skipping Rust tests: cargo not installed"
fi

if command -v zig >/dev/null 2>&1; then
  ( cd zig && zig build test )
else
  echo "Skipping Zig tests: zig compiler not installed"
fi

if command -v raco >/dev/null 2>&1; then
  ( cd racket && raco test test.rkt )
else
  echo "Skipping Racket tests: raco not installed"
fi

# Cross-language key derivation consistency
if [ -x tests/cross_lang_key_derivation.sh ]; then
  ./tests/cross_lang_key_derivation.sh
else
  echo "cross_lang_key_derivation.sh not found" >&2
  exit 1
fi

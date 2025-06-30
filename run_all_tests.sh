#!/usr/bin/env bash
# Run all TypeCrypt test suites
set -e

( cd haskell && cabal update && cabal test )
( cd rust && cargo test )
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

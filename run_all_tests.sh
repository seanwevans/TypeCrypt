#!/usr/bin/env bash
# Run all TypeCrypt test suites
set -e

( cd haskell && cabal test )
( cd rust && cargo test )
( cd zig && zig build )

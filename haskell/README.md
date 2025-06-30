# TypeCrypt Haskell (Theory Branch)

This directory contains the formal specification and type logic implementation.

- `src/Types.hs` defines the core `Type` and `Value` algebra using GADTs.
- `spec/Core.hs` re-exports the canonical logic for other implementations.
- `test/Spec.hs` contains QuickCheck properties.
 - Future modules will build on this to implement symbolic matching and encryption logic.

Run `cabal test` to execute the QuickCheck suite.

# TypeCrypt Haskell (Theory Branch)

This directory contains the formal specification and type logic implementation.

 - `src/Types.hs` defines the core `Type` and `Value` algebra along with a
   `matches` helper for verifying that a `Value` conforms to a `Type`.
 - Future modules will build on this to implement symbolic matching and encryption logic.

Run `cabal test` or `stack test` once tests are added.

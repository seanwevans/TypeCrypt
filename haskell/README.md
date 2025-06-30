# TypeCrypt Haskell (Theory Branch)

This directory contains the formal specification and type logic implementation.

- `src/Types.hs` defines the core `Type` and `Value` algebra using GADTs.
- `spec/Core.hs` re-exports the canonical logic for other implementations.
- `test/Spec.hs` contains QuickCheck properties.
- `encrypt` and `decrypt` use `cryptonite`'s ChaCha20-Poly1305 implementation and tests verify ciphertexts round-trip.

To build and test this implementation:

```bash
cabal build
cabal test
```

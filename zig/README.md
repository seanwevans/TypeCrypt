# TypeCrypt Zig (Experimental Branch)

This directory experiments with compile-time features of Zig to implement TypeCrypt.

## Build

Run `zig build` from this directory to compile the experimental executable. It demonstrates a basic type enumeration and a compile-time `typeHash` utility.

## Key Derivation

The Zig prototype mirrors the canonical byte encoding used in the Haskell and Rust implementations. The `canonicalBytes` function serializes a `Type` using constructor tags `0`–`4`. A `deriveKey` helper hashes this encoding with `SHA‑256` to produce a 32 byte key which future encryption routines will rely on.

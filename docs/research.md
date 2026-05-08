# Research Direction: Predicate and Functional Encryption

TypeCrypt's current implementations are a **demonstration layer**: they derive a
symmetric key from a canonical `Type` encoding, verify that a supplied runtime
`Value` matches that type, and only then attempt AEAD decryption. That is useful
for exploring interfaces and cross-language canonicalization, but the predicate
check is still performed by program logic before decryption.

The research-grade version of TypeCrypt is closer to **predicate encryption** and
**functional encryption**. In those systems, ciphertexts and secret keys are
constructed so that the cryptographic operation itself enforces an access
relation. A ciphertext can be associated with a predicate `P`, a secret key can be
associated with an attribute or value `v`, and decryption succeeds only when
`P(v)` holds. No caller-visible `if matches(value, type)` gate is trusted for
security; the satisfaction relation is embedded in the pairing-, lattice-, or
other assumption-backed construction.

## Why this matters for TypeCrypt

A type predicate such as “record with field `role = admin` and schema version
`3`” can be modeled as an access predicate. Binding decryption to that predicate
cryptographically would make type satisfaction part of the decryption math rather
than an application-side guard. This is the version of TypeCrypt that could offer
a genuinely new cryptographic primitive instead of only a type-aware wrapper
around conventional symmetric encryption.

## Current model versus research target

| Aspect | Current TypeCrypt prototype | Predicate/functional-encryption target |
| --- | --- | --- |
| Enforcement point | Runtime matcher checks `Value -> Type -> Bool` before AEAD decrypt | Decryption algorithm succeeds only when the encoded relation is satisfied |
| Key material | Symmetric key derived from a type descriptor | Master-secret-derived keys tied to attributes, values, functions, or policies |
| Trust boundary | Application must call the matcher correctly | Security proof covers policy satisfaction under cryptographic assumptions |
| Main value | Interface design, canonical encodings, cross-language tests | Cryptographic binding of decryption to predicate/type satisfaction |

## Literature anchors

The design space is related to work on predicate encryption, attribute-based
encryption, and functional encryption, including work by Dan Boneh, Amit Sahai,
Brent Waters, and successors. Useful starting points include:

- Boneh and Waters, “Conjunctive, Subset, and Range Queries on Encrypted Data.”
- Katz, Sahai, and Waters, “Predicate Encryption Supporting Disjunctions,
  Polynomial Equations, and Inner Products.”
- Boneh, Sahai, and Waters, “Functional Encryption: Definitions and Challenges.”

## Roadmap implications

Future research work should treat the current matcher and type hash as a
prototype interface, not as the final security mechanism. A credible TypeCrypt
research branch should:

1. Define a formal mapping from TypeCrypt `Type` constructors to supported
   predicate classes.
2. Specify key-generation, encryption, and decryption algorithms where predicate
   satisfaction is enforced by the construction.
3. State the security model, leakage profile, and supported collusion resistance.
4. Keep the existing Haskell `spec/Core.hs` matcher as an executable reference
   for semantics, while separating it from security claims.
5. Add test vectors that compare semantic satisfaction in the prototype with
   cryptographic success/failure in any future predicate-encryption backend.

# TypeCrypt Project Overview

TypeCrypt is an experiment in **type-driven encryption**. Instead of relying solely on secret keys, ciphertexts are bound to a type description. Data encrypted under a type `T` can only be decrypted when the caller supplies a value that satisfies `T`. This ties access control directly to type checking and opens the door to novel compile‑time guarantees.

The project is organized into three interoperating implementations:

- **Haskell – The Theory Branch**
  - Defines the canonical logic for type/value pairs and symbolic matching.
  - Serves as the source of truth for the type system and exports an intermediate representation.
- **Rust – The Production Branch**
  - Mirrors the Haskell definitions with performance and safety in mind.
  - Provides hardened encryption primitives and integration tests.
- **Zig – The Experimental Branch**
  - Explores new representations using `comptime` features and tests radical ideas.
  - Reports successful patterns back to the other branches.

Changes generally flow from Haskell to Rust to Zig. Haskell establishes the formal semantics; Rust implements them in a production setting; Zig prototypes new concepts that may feed back upstream.

For more on the coordination between these agents see [AGENTS.md](../AGENTS.md). Newcomers can consult the [ROADMAP](../ROADMAP.md) for the current project goals and long‑term plans.

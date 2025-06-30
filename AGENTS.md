# AGENTS.md: Autonomous Interaction Guide for TypeCrypt

This document describes how autonomous agents (AI assistants, CI agents, automated verifiers, etc.) should interact with the TypeCrypt project.

## 🛠 General Workflow

- Format code according to `docs/development.md`.
- Run `./run_all_tests.sh` before committing.
- Include the agent name in commit messages.

---

## 🤖 Agent Classes & Roles

### 1. `agent/theorist`

* **Language**: Haskell
* **Goal**: Maintain the formal type system and ensure logical correctness.
* **Duties**:

  * Update and verify type algebra definitions (`haskell/src/Types.hs`)
  * Run `QuickCheck` and symbolic validation tests
  * Maintain `spec/Core.hs` as the canonical logic spec
  * Export updated IR for downstream transpilation

### 2. `agent/engineer`

* **Language**: Rust
* **Goal**: Harden and productionize the core functionality
* **Duties**:

  * Sync with latest formal types from `agent/theorist`
  * Ensure performance benchmarks using `criterion`
  * Use `sodiumoxide` or `ring` for secure AEAD
  * Maintain `no_std` compatibility
  * Run integration and stress tests

### 3. `agent/prototyper`

* **Language**: Zig
* **Goal**: Explore experimental designs at compile time
* **Duties**:

  * Implement new `Type` representations with `@Type()` or `comptime` logic
  * Test compile-time hashing schemes
  * Report any promising patterns for upstream adoption

---

## 🧭 Agent Protocols

### 🔄 Syncing Across Branches

* Each agent must track `spec/Core.hs` from Haskell as the canonical logic
* Changes to type behavior must propagate from Haskell → Rust → Zig

### ✅ Verification Tasks

* `agent/theorist`: Proves correctness of type/value unification
* `agent/engineer`: Ensures all trait-bound decryptions enforce schema statically
* `agent/prototyper`: Tests schema reflection + generative decoding

### 📦 Output Format

* All agents should emit logs in JSONL:

```json
{
  "agent": "agent/theorist",
  "task": "quickcheck_validation",
  "status": "pass",
  "timestamp": "2025-06-30T18:00:00Z"
}
```

## 🛠 Workflow Checklist

- Ensure all code compiles by running `./run_all_tests.sh`.
- Record results in your JSONL log entry.

---

## 🧠 Example Agent Interaction

A typical flow for introducing a new type:

1. `agent/theorist` defines `Type/Value` pair + symbolic matcher.
2. `agent/engineer` syncs and implements type-aware encryption trait.
3. `agent/prototyper` tests alternative representations or constraints.

---

## ✳️ Notes

* All agents should be stateless and pull state from the file system only.
* Use commit hashes for provenance in inter-agent messaging.
* Avoid divergence: report any contradictions between branches.

---

This document may evolve. Agents should check `AGENTS.md` for the latest protocol changes before executing major tasks.

# TypeCrypt Racket (Assembly Branch)

This directory contains a small Racket prototype that demonstrates emitting x86_64 assembly.

Running `racket main.rkt` will generate an `output.s` file with assembly for a simple addition function.  The module also exposes `emit-sub` for subtraction and a generic `emit-binop` helper that can be extended with new operations.

`emit-binop` checks that its two arguments are integer `Value` structures and then writes a minimal function in AT&T syntax. The emitted code declares a global label, moves the first argument into `rax`, performs the requested operation with `rsi`, and returns. This helper forms the basis for generating additional arithmetic instructions by extending its opcode dispatch table.

To run the rackunit suite:

```bash
cd racket
raco test test.rkt
```

The tests verify that `output.s` contains the expected assembly text.

This assembly prototype feeds into the broader TypeCrypt roadmap as a proving ground for low-level code generation. Lessons here will influence how the Haskell and Rust components emit machine code in later stages.

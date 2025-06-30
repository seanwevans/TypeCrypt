# TypeCrypt Racket (Assembly Branch)

This directory contains a small Racket prototype that demonstrates emitting x86_64 assembly.

Running `racket main.rkt` will generate an `output.s` file with assembly for a simple addition function.  The module also exposes `emit-sub` for subtraction and a generic `emit-binop` helper that can be extended with new operations.

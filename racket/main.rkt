#lang racket

;; Simple Racket script to emit x86_64 assembly for an integer addition

(struct Type (name) #:transparent)
(struct Value (type val) #:transparent)

(provide Type Value TInt matches emit-add emit-sub emit-binop)

(define TInt (Type 'int))

;; Check if a Value matches a Type
(define (matches v t)
  (eq? (Type-name (Value-type v)) (Type-name t)))

;; Internal helper for emitting binary integer operations
(define (emit-binop op v1 v2 #:output [outfile "output.s"])
  (unless (and (matches v1 TInt) (matches v2 TInt))
    (error 'emit-binop "values must be integers"))
  (define instr
    (case op
      [(add) "    add rax, rsi"]
      [(sub) "    sub rax, rsi"]
      [else (error 'emit-binop "unsupported operation")]))
  (with-output-to-file outfile #:exists 'replace
    (lambda ()
      (define label (symbol->string op))
      (displayln "    .text")
      (displayln (string-append "    .global " label))
      (displayln (string-append label ":"))
      (displayln "    mov rax, rdi")
      (displayln instr)
      (displayln "    ret"))))

;; Emit assembly that adds two integers
(define (emit-add v1 v2 #:output [outfile "output.s"])
  (emit-binop 'add v1 v2 #:output outfile))

;; Emit assembly that subtracts the second integer from the first
(define (emit-sub v1 v2 #:output [outfile "output.s"])
  (emit-binop 'sub v1 v2 #:output outfile))

(module+ main
  (emit-add (Value TInt 1) (Value TInt 2)))

#lang racket

;; Simple Racket script to emit x86_64 assembly for an integer addition

(struct Type (name) #:transparent)
(struct Value (type val) #:transparent)

(provide Type Value TInt matches emit-add)

(define TInt (Type 'int))

;; Check if a Value matches a Type
(define (matches v t)
  (eq? (Type-name (Value-type v)) (Type-name t)))

;; Emit assembly that adds two integers and returns the result in rax
(define (emit-add v1 v2)
  (unless (and (matches v1 TInt) (matches v2 TInt))
    (error 'emit-add "values must be integers"))
  (with-output-to-file "output.s" #:exists 'replace
    (lambda ()
      (displayln "    .text")
      (displayln "    .global add")
      (displayln "add:")
      (displayln "    mov rax, rdi")
      (displayln "    add rax, rsi")
      (displayln "    ret"))))

(module+ main
  (emit-add (Value TInt 1) (Value TInt 2)))

#lang racket
(require rackunit "main.rkt")

(module+ test
  (check-true (matches (Value TInt 1) TInt))
  (check-false (matches (Value TInt 1) (Type 'str)))
  (check-exn exn:fail? (lambda () (emit-add (Value TInt 1)
                                             (Value (Type 'str) "oops"))))
  (when (file-exists? "output.s") (delete-file "output.s"))
  (emit-add (Value TInt 1) (Value TInt 2))
  (check-true (file-exists? "output.s"))
  (define add-content (file->string "output.s"))
  (check-true (string-contains? add-content "add rax, rsi"))
  (delete-file "output.s")
  (emit-sub (Value TInt 5) (Value TInt 2))
  (check-true (file-exists? "output.s"))
  (define sub-content (file->string "output.s"))
  (check-true (string-contains? sub-content "sub rax, rsi"))
  (delete-file "output.s"))

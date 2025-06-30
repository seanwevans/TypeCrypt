#lang racket
(require rackunit "main.rkt")

(module+ test
  (check-true (matches (Value TInt 1) TInt))
  (check-false (matches (Value TInt 1) (Type 'str)))
  (check-exn exn:fail? (lambda () (emit-add (Value TInt 1)
                                             (Value (Type 'str) "oops"))))
  (when (file-exists? "output.s") (delete-file "output.s"))
  (emit-add (Value TInt 1) (Value TInt 2))
  (check-true (file-exists? "output.s")))

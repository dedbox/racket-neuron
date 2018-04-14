#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (reprovide module-path ...)
  (begin
    (require module-path ...)
    (provide (all-from-out module-path) ...)))

(define-syntax-rule (reprovide-for-label module-path ...)
  (begin
    (require (for-label module-path ...))
    (provide (for-label (all-from-out module-path ...)))))

(define-syntax-rule (reprovide/for-label module-path ...)
  (begin
    (reprovide module-path ...)
    (reprovide-for-label module-path ...)))

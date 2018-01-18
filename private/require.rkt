#lang racket/base

(provide (all-defined-out))

(require racket/function)

(define-syntax-rule (require-and-provide module-path ...)
  (begin
    (require module-path ...)
    (provide (all-from-out module-path) ...)))

(define-syntax-rule (require-for-label-and-provide module-path ...)
  (begin
    (require (for-label module-path) ...)
    (provide (for-label (all-from-out module-path)) ...)))

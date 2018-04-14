#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (forever body ...)
  (let loop () body ... (loop)))

(define-syntax-rule (while expr body ...)
  (let loop () (when expr body ... (loop))))

(define-syntax-rule (until expr body ...)
  (let loop () (unless expr body ... (loop))))

(define-syntax-rule (apply-values proc expr)
  (call-with-values (λ () expr) proc))

#lang racket/base

(require
 racket/match
 (for-syntax racket/base
             syntax/parse))

(provide bindings)

(define-syntax (bindings stx)
  (define-syntax-class rule
    (pattern [(pat ...) body:expr ...+]))
  (syntax-parse stx
    [(bindings
      r:rule ...+
      (~optional (~seq #:match m:expr ...+))
      (~optional (~seq #:else d:expr ...+)))
     #`(match-lambda*
         [`(r.pat ...) r.body ...] ...
         #,@(or (attribute m) null)
         #,@(if (attribute d) (list #'[_ d ...]) null))]))

(module+ test
  (require rackunit)

  (test-case
    "bindings"
    (define f (bindings [(a) 1] #:match ['(b) 2] #:else 0))
    (check = 1 (f 'a))
    (check = 2 (f 'b))
    (check = 0 (f 'z)))

  (test-case
      "bindings:calc"
    (define vars (make-hasheq))
    (define calc
      (bindings
       [((,a + ,b)) (+ (calc a) (calc b))]
       [((,a ^ ,b)) (expt (calc a) (calc b))]
       [((,a = ,b))
        (hash-set! vars a (calc b))
        (hash-ref vars a)]
       #:match
       [(list (? number? n)) n]
       [(list (? symbol? x)) (hash-ref vars x #f)]
       #:else 'stuck))
    (check = (calc '(a = 2)) 2)
    (check = (calc '(b = ((a ^ 3) + (3 ^ a)))) 17)
    (check = (hash-ref vars 'a) 2)
    (check = (hash-ref vars 'b) 17)))

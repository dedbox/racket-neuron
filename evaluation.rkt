#lang racket/base

(require racket/match)

(provide bind)

(define-syntax bind
  (syntax-rules ()
    [(_ ([q q-exp ...] ...)
        #:match ([p p-exp ...] ...)
        #:else default)
     (match-lambda
       [`q q-exp ...] ...
       [p p-exp ...] ...
       [_ default])]

    [(_ ([q q-exp ...] ...)
        #:match ([p p-exp ...] ...))
     (match-lambda
       [`q q-exp ...] ...
       [p p-exp ...] ...)]

    [(_ ([q q-exp ...] ...)
        #:else default)
     (match-lambda
       [`q q-exp ...] ...
       [_ default])]

    [(_ ([q q-exp ...] ...))
     (match-lambda
       [`q q-exp ...] ...)]))

(module+ test
  (require rackunit)

  (test-case
    "bind"
    (define f (bind ([a 1]) #:match (['b 2]) #:else 0))
    (check = 1 (f 'a))
    (check = 2 (f 'b))
    (check = 0 (f 'z)))

  (test-case
    "bind:calc"
    (define vars (make-hasheq))
    (define calc
      (bind
        ([(,a + ,b) (+ (calc a) (calc b))]
         [(,a ^ ,b) (expt (calc a) (calc b))]
         [(,a = ,b)
          (hash-set! vars a (calc b))
          (hash-ref vars a)])
        #:match
        ([(? number? n) n]
         [(? symbol? x) (hash-ref vars x #f)])
        #:else 'stuck))
    (check = (calc '(a = 2)) 2)
    (check = (calc '(b = ((a ^ 3) + (3 ^ a)))) 17)
    (check = (hash-ref vars 'a) 2)
    (check = (hash-ref vars 'b) 17)))

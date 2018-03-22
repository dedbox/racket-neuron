#lang scribble/doc

@(require "../base.rkt")

@title{Evaluation}

@(defmodule neuron/evaluation #:packages ("neuron"))

@section{Terms}

A @deftech{term} is defined recursively as a literal value or a serializable
composite of sub-terms. For example, the symbol

@racketblock['a-symbol]

and the number

@racketblock[123]

are terms because they are literal values. Furthermore, the structures

@racketblock[
  '(a-symbol 123)
]

and

@racketblock[
  #hasheq((a-symbol . 123))
]

are terms because they are @racket[read]/@racket[write]able composites of
literals.

@section{Steppers}

A @deftech{stepper} is a function that maps one @tech{term} to another. For
example,

@racketblock[
  (case-lambda
    [(a) 1]
    [(b) 2]
    [else 0])
]

maps any term to a number between @racket[0] and @racket[2]. Similarly,

@racketblock[
  (match-lambda
    [1 'a]
    [2 'b]
    [_ 'z])
]

maps any term to @racket['a], @racket['b], or @racket['z]. A more realistic
example is @racket[values], which maps every term to itself; or the function

@racketblock[
  (define step
    (match-lambda
      [(list (? term? e1) (? term? e2)) #:when (not (value? e1))
       (list (step e1) e2)]
      [(list (? value? v1) (? term? e2?)) #:when (not (value? e2))
       (list v1 (step e2))]
      [(list `(λ ,(? symbol? x11) ,(? term? e12)) (? value? v2))
       (substitute e12 x11 v2)]
      [_ 'stuck]))
]

a @tech{term}-based small-@tech{stepper} for the untyped lambda calculus.

The @racket[bind] form offers an alternative to @racket[match-lambda] for
specifying @tech{steppers} with Racket's @secref["match" #:doc '(lib
"scribblings/reference/reference.scrbl")] sub-system.

@defform[
  (bind ([quoted-pat q-exp ...] ...) maybe-match maybe-else)
  #:grammar
  [(maybe-match (code:line)
                (code:line #:match ([pat p-exp ...] ...)))
   (maybe-else (code:line)
               (code:line #:else default))]
]{

  Creates a @racket[match-lambda] with @racket[quoted-pat]s implicitly
  @racket[quasiquote]d. If @racket[pat] clauses are given, they are appended
  to the @racket[quoted-pat] clauses unmodified. If @racket[default] is given,
  a final catch-all clause that returns @racket[default] is added.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define vars (make-hash))
    (define calc
      (bind
        ([(,a + ,b) (+ (calc a) (calc b))]
         [(,a ^ ,b) (expt (calc a) (calc b))]
         [(,a = ,b)
          (hash-set! vars a (calc b))
          (hash-ref vars a)])
        #:match
        ([(? number? n) n]
         [(? symbol? x) (hash-ref vars x)])
        #:else 'stuck))
    (calc '(a = 2))
    (calc '(b = ((a ^ 3) + (3 ^ a))))
    vars
  ]

}

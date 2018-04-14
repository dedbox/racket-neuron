#lang scribble/doc

@(require "../base.rkt")

@title{Evaluation}

@(defmodule neuron/evaluation #:packages ("neuron"))

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

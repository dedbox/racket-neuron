#lang scribble/doc

@(require "../base.rkt")

@title{Evaluation}

@(defmodule neuron/evaluation #:packages ("neuron"))

The @racket[bindings] form is an alternative syntax for @racket[match-lambda*]
for specifying multi-valued @tech{steppers} with Racket's @secref["match"
#:doc '(lib "scribblings/reference/reference.scrbl")] syntax.

@defform[
  (bindings rule ...+ maybe-match maybe-else)
  #:grammar
  [(rule [(quoted-pat ...) body ...+])
   (maybe-match (code:line)
                (code:line #:match [(pat ...) body ...+] ...+))
   (maybe-else (code:line)
               (code:line #:else default-expr))]
]{

  Creates a @racket[match-lambda*] with @var[quoted-pat]s implicitly
  @racket[quasiquote]d. If @var[pat] clauses are given, they are appended to
  the @racket[quoted-pat] clauses unmodified. If a @var[default-expr] is
  given, a final catch-all clause that evaluates @racket[default-expr] is
  added.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define vars (make-hash))
    (define calc
      (bindings
        [((,a + ,b)) (+ (calc a) (calc b))]
        [((,a ^ ,b)) (expt (calc a) (calc b))]
        [((,a = ,b))
         (hash-set! vars a (calc b))
         (hash-ref vars a)]
        #:match
        [(list (? number? n)) n]
        [(list (? symbol? x)) (hash-ref vars x)]
        #:else 'stuck))
    (calc '(a = 2))
    (calc '(b = ((a ^ 3) + (3 ^ a))))
    vars
  ]

}

#lang racket/base

(require neuron/reprovide)

(reprovide
 pict
 racket/math
 racket/sandbox
 scribble/examples
 scribble/manual)

(reprovide-for-label
 racket/base
 racket/contract
 racket/match
 racket/tcp
 json)

(reprovide/for-label
 neuron)

(provide (all-defined-out))

;; Scribble Tech

(define (rtech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

;; Sandboxed Evaluation

(define neuron-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50]
                 [sandbox-eval-limits '(30 50)]
                 [sandbox-make-inspector current-inspector])
    (make-evaluator 'racket #:requires '(neuron))))

(random-seed 1)

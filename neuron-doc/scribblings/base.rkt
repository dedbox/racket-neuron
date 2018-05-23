#lang racket/base

(require
 event
 neuron
 pict
 racket/math
 racket/sandbox
 scribble/examples
 scribble/manual)

(provide
 (all-from-out
  neuron
  pict
  racket/math
  racket/sandbox
  scribble/examples
  scribble/manual))

(require
 (for-label
  event
  neuron
  racket/base
  racket/contract
  racket/match
  racket/tcp
  json))

(provide
 (for-label
  (all-from-out
   event
   neuron
   racket/base
   racket/contract
   racket/match
   racket/tcp
   json)))

(provide (all-defined-out))

;; Scribble Tech

(define (rtech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

(define (ctech . args)
  (apply tech #:doc '(lib "cmx/scribblings/cmx.scrbl") args))

;; Sandboxed Evaluation

(define neuron-evaluator
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string])
       (make-evaluator 'racket #:requires '(neuron event racket/pretty))))))

(random-seed 1)

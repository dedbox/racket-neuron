#lang at-exp racket/base

(require neuron/private/require
         racket/function)

(require-and-provide
 neuron
 pict
 racket/sandbox
 scribble/examples
 scribble/manual)

(require-for-label-and-provide
 neuron
 racket/base
 racket/contract)

(provide (all-defined-out))

(define (layer w h str
               #:t [t (hline w 0)]
               #:r [r (vline 0 h)]
               #:b [b (hline w 0)]
               #:l [l (vline 0 h)]
               #:bg [bg "white"]
               #:fg [fg "black"])
  (define T (curryr ct-superimpose t))
  (define R (curryr rc-superimpose r))
  (define B (curryr cb-superimpose b))
  (define L (curryr lc-superimpose l))
  (cc-superimpose
   (T (R (B (L (filled-rectangle w h #:color bg #:draw-border? #f)))))
   (colorize (text str 'roman) fg)))

(define (racket-tech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

(define neuron-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket/base '(require neuron))))

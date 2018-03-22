#lang at-exp racket/base

(require neuron/private/require
         racket/function)

(require-and-provide
 neuron
 pict
 racket/math
 racket/sandbox
 scribble/examples
 scribble/manual)

(require-for-label-and-provide
 neuron
 json
 racket/base
 racket/contract
 racket/match
 racket/tcp)

(provide (all-defined-out))

;;; Drawing

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

(define (label w h str #:fg [fg "black"])
  (layer w h str #:t (blank) #:r (blank) #:b (blank) #:l (blank) #:fg fg))

;;; Scribble Tech

(define (rtech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

;;; Sandboxed Evaluation

(define neuron-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50]
                 [sandbox-make-inspector current-inspector])
    (make-evaluator 'racket #:requires '(neuron))))

(random-seed 1)

#lang at-exp racket/base

(require scribble/manual)
(provide (all-from-out scribble/manual))

(require (for-label racket/base
                    racket/contract))
(provide (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)))

(require (for-label neuron))
(provide (for-label (all-from-out neuron)))

(require pict
         racket/function)
(provide layer)

(define (layer w h str
               #:t [t (hline (- w 1) 1)]
               #:r [r (vline 1 (- h 1))]
               #:b [b (hline (- w 1) 1)]
               #:l [l (vline 1 (- h 1))])
  (define content (cc-superimpose (blank w h) (text str 'roman)))
  (define T (curry ct-superimpose t))
  (define R (curry rc-superimpose r))
  (define B (curry cb-superimpose b))
  (define L (curry lc-superimpose l))
  (T (R (B (L content)))))

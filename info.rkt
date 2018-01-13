#lang info

(define collection "neuron")

(define deps '("base"))

(define build-deps '("at-exp-lib" "pict-lib" "racket-doc" "scribble-lib"))

(define scribblings
  '(("scribblings/main.scrbl" (multi-page) (library) "neuron")))

#lang info

(define collection "neuron")

(define deps '("base"))

(define build-deps '("scribble-lib" "racket-doc"))

(define scribblings
  '(("scribblings/neuron.scrbl" (multi-page) (library) "neuron")))

#lang info

(define collection "neuron")

(define deps '("base"))

(define build-deps '("racket-doc" "scribble-lib"))

(define scribblings
  '(("scribblings/main.scrbl" (multi-page) (library) "neuron")))

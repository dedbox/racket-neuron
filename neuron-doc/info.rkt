#lang info

(define collection "neuron")

(define deps
  '("base"
    "neuron-lib"))

(define build-deps
  '("at-exp-lib"
    "pict-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/main.scrbl" (multi-page) (library) "neuron")))

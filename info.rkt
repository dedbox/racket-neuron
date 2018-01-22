#lang info

(define collection "neuron")

(define deps '("base"))

(define build-deps
  '("at-exp-lib"
    "pict-lib"
    "racket-doc"
    "rackunit-lib"
    "sandbox-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/main.scrbl" (multi-page) (library) "neuron")))

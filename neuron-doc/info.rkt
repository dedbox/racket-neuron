#lang info

(define collection "neuron")

(define deps
  '("base"))

(define build-deps
  '("cmx"
    "draw-lib"
    "event-lang"
    "neuron-lib"
    "pict-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/neuron.scrbl" (multi-page) (library) "neuron")))

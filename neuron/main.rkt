#lang racket/base

(define-syntax-rule (require-and-provide module-path ...)
  (begin
    (require module-path ...)
    (provide (all-from-out module-path) ...)))

(require-and-provide neuron/process
                     neuron/evaluate
                     neuron/serial-io
                     neuron/serialize
                     neuron/simulate
                     neuron/distribute
                     neuron/decentralize
                     neuron/organize)

#lang scribble/manual

@(require "base.rkt")

@(require (for-label neuron))

@title{Neuron: Decentralized Software Organisms}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

The @racketmodname[neuron] library is a framework for growing decentralized
software organisms.

@defmodule[neuron #:packages ("neuron")]

@centered[
  @vc-append[
    @layer[300 35 #:b (blank)]{Organize}
    @layer[300 35 #:b (blank)]{Decentralize}
    @layer[300 35 #:b (blank)]{Distribute}
    @hc-append[
      @layer[75 35 #:b (blank) #:r (blank)]{Simulate}
      @layer[75 35 #:b (blank) #:r (blank)]{Evaluate}
      @layer[75 35 #:b (blank) #:r (blank)]{Serialize}
      @layer[75 35 #:b (blank)]{Serial I/O}]
    @layer[300 35]{Process}]]

@table-of-contents[]

@include-section["process.scrbl"]
@include-section["serial-io.scrbl"]
@include-section["serialize.scrbl"]
@include-section["evaluate.scrbl"]
@include-section["simulate.scrbl"]
@include-section["distribute.scrbl"]
@include-section["decentralize.scrbl"]
@include-section["organize.scrbl"]

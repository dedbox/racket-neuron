#lang scribble/manual

@(require "base.rkt"
          pict
          racket/format
          scriblib/autobib
          scriblib/bibtex)

@(require (for-label neuron))

@(define-bibtex-cite "scribblings/refs.bib" ~cite citet generate-bibliography
                     #:style number-style)

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

@; -----------------------------------------------------------------------------

@; @itemlist[
@;   @item{@racketmodname[neuron/concurrency] -- lightweight process networks}

@;   @item{@racketmodname[neuron/io] -- byte streams}

@;   @item{@racketmodname[neuron/serializer] -- parsing and printing}

@;   @item{@racketmodname[neuron/distributed] -- fault tolerance}

@;   @item{@racketmodname[neuron/evaluator] -- portability and process mobility}

@;   @item{@racketmodname[neuron/decentralized] -- cooperation and trust}

@;   @item{@racketmodname[neuron/social] -- identity, association, and
@;   accountability}
@; ]

@; @larger{@bold{The Architecture}}

@; The @emph{soma} is ambient structure. It serves the nucleus as a messaging
@; medium.

@; The @emph{nucleus} encodes behavior and mediates message activity.

@; @emph{Glia} are helpers. They provide regularity, stability, and safety.

@; @emph{Myelin} insulates the axon from its environment to maintain the
@; integrity of the channel.

@; @(generate-bibliography)

#lang scribble/manual

@require[@for-label[neuron
                    racket/base]]

@title{Neuron}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@defmodule[neuron]

Decentralized social software

Neuron is a foundation for research, development, distribution, operation, and
consumption of social software.

Neuron is a toolkit for creating decentralized social software systems as
programmable dataflow networks.

Structured messages. Concurrent and distributed. Decentralized processing.
Encryption and capability-based security.

Structure of a Neuron:
Axon - messaging
Dendrite - concurrency
Soma - distribution
Nucleus - computation
Glia - capabilities
Myelin - decentralization

Axon: A message can be any unit of information: a byte, a transport frame, a
string, a file, a photo, a program.

Nucleus: An object is not just a "recursion on the computer," but an
interpreter; a pure but particular kind of abstraction. Each object has
messaging semantics implied by its rules for reducing and emitting messages.

Dendrite: Objects can evolve concurrently, as a local process

Soma: or remotely, as a distributed service.

Glia: Objects have useful properties like identity, equivalence, scarcity,
possession, secrecy, and provenance.

Myelin: Model and exploit real-world social dynamics to drive consensus,
trust, reputation, and agreement.

Imagine a network of filters, each partially evaluating their own portion of a
message, reconfiguring themselves and the network as messages percolate
through.

@itemlist[
  @item{@racketmodname[neuron/axon] message structuring and serialization}
  @item{@racketmodname[neuron/dendrite]}
  @item{@racketmodname[neuron/soma]}
  @item{@racketmodname[neuron/nucleus]}
  @item{@racketmodname[neuron/glia]}
  @item{@racketmodname[neuron/myelin]}]

@local-table-of-contents[]

@include-section["axon.scrbl"]
@include-section["dendrite.scrbl"]
@include-section["soma.scrbl"]
@include-section["nucleus.scrbl"]
@include-section["glia.scrbl"]
@include-section["myelin.scrbl"]

#lang scribble/manual

@(require "base.rkt")

@title[
  #:style '(unnumbered toc)
  #:tag "The Neuron Reference"
]{The Neuron Reference}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@defmodule[neuron #:packages ("neuron-lib")]

This library is structured as a layered framework. Its architecture encourages
reuse and simplifies customization without sacrificing flexibility. Each layer
of the framework provides functionality for subsequent layers to build upon.

The foundational layer defines the concurrency and serialization models used
throughout the library. It contains a collection of protocol-agnostic
constructs for high-level network programming and implementations for some
basic protocols and formats. It also includes a privileged command mechanism
suitable for information flow control. This foundation empowers developers to
grow highly dynamic networks from the REPL by composing relatively simple
components into more complex ones.

@local-table-of-contents[#:style 'immediate-only]

@include-section["reference/control-your-resources.scrbl"]

@section[#:style '(grouper)]{Operate Your Network}
@subsection{Distributed Evaluation}
@subsubsection{Portability}
@subsubsection{Process Mobility}
@subsection{Capabilities}

@section[#:style '(grouper)]{Cooperate with Others}
@subsection{Decentralized Run Time Environments}
@subsection{Decentralized Programs}

@section[#:style '(grouper)]{Grow a Community}
@subsection{Identity}
@subsection{Association}
@subsection{Accountability}
@subsection{Policy}
@subsubsection{Execution}
@subsubsection{Enforcement}

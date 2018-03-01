#lang scribble/manual

@(require "base.rkt")

@title[#:style '(unnumbered)]{The Neuron Technical Report}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

The name ``Neuron'' represents a language for building and operating
decentralized program evaluators, along with the set of Racket libraries that
implement the language. In this document, the name unambiguously refers to one
or the other, never both.

Library design priorities:

@itemlist[
  @item{A program is a data structure}
  @item{All data structures are serializable}
  @item{WHAT ABOUT data lineage and information flow?}
]

@section{Interpreter design, tools and techniques}

Question: what are the ways to define a structure interpreter in Racket?

Question: what is the @emph{recommended} way to define a structure interpreter
in Racket?

Question: what is the @emph{most popular} way to define a structure
interpreter in Racket?

Question: what is the @emph{easiest} way to define a structure interpreter in
Racket?

Question: how can we do better?

Challenge: define ``better.''

#lang scribble/manual

@(require "base.rkt")

@title[#:style '(unnumbered toc)]{The Neuron Reference}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@defmodule[neuron #:packages ("neuron")]

@; @centered[
@;   @vc-append[
@;     @layer[250 35 #:fg "blue"]{Control}
@;     @layer[250 35 #:fg "red"]{Collaborate}
@;     @layer[250 35 #:fg "green"]{Grow}
@;   ]
@; ]

@; @itemlist[
@;   #:style 'ordered
@;   @item{Control your own resources.}
@;   @item{Collaborate with others.}
@;   @item{Grow a community.}
@; ]

@; @centered[
@;   @vc-append[
@;     @layer[300 35 #:fg "blue"]{Process}
@;     @hc-append[
@;       @layer[100 35 #:fg "blue"]{Bytes}
@;       @layer[100 35 #:fg "blue"]{Codecs}
@;       @layer[100 35 #:fg "blue"]{Evaluators}
@;     ]
@;     @layer[300 35 #:fg "red"]{Distribute}
@;     @layer[300 35 #:fg "red"]{Decentralize}
@;     @layer[300 35 #:fg "green"]{Organize}
@;   ]
@; ]

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

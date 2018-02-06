#lang scribble/manual

@(require "base.rkt")

@title{Neuron: Decentralized Software Organisms}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@defmodule[neuron #:packages ("neuron")]

The @racketmodname[neuron] library is a framework for creating and
participating in Internet-scale software ecosystems. Neuron provides a
unified, architecture-agnostic API for IPC and distributed messaging, along
with components for building dynamic data-flow networks, decentralized run
time environments and programs, and process-mobile software agents.


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

@; making distributed interpreters and data flows, 

@; @itemlist[
@;   @item{decentralized -- cooperative networks}
@;   @item{software -- distributed interpreters and data flows}
@;   @item{organism -- signalling and self-sustaining entities}
@; ]

@; Engineering decisions carry weight in the real world.

@; To evolve software, model natural systems.

@; This has worked in the past. See: 80's-90's Perl community. Similar to Racket
@; today. It was like a giant software organism with CPAN at its heart, spreading
@; its influence all over the net. Things were simpler back then. It was easier
@; to appreciate the fractal-like symmetry of it all.

@; I think a lot of people have forgotten that, or weren't around to
@; experience it themselves. At some point, the Web just took over everything.
@; Web server frameworks and toolchains replaced operating system and compiler
@; construction as rites of passage.

@; Then came the shopping carts. Mind numbing boredom. Education is the enemy
@; of shopping carts.

@; it turns out power laws are important for healthy human networks

@; [ecosystems, ecology, economies,  organism, community]

@; Borrowing object oriented terminology, I think like this:

@; - A "message" can be any static value: a byte, a transport frame, a string, a
@;   file, a photo, a program.

@; - An "object" is a lightweight process. Process implement rules for
@;   interpreting and emitting values.

@; - Lightweight processes can run concurrently, in a host process

@; - ... or as a distributed service.

@; - In addition to identity and generic equivalence, processes have other useful
@;   properties like scarcity, possession, secrecy, and provenance.

@; - This model is convenient for expressing high-level interaction patterns like
@;   gathering consensus, evaluating trust or reputation, and entering and
@;   enforcing agreements.

@; With these components, core services could be reduced to compact and elegant
@; scripts. I want the experience of decentralized computing in Neuron to be as
@; compelling as functional programming in Haskel and REPL interaction in Scheme.

@; I imagine chains of filters, partially evaluating their own portions of a
@; larger message, dynamically reconfiguring their behavior as well as their
@; overlay network topology. Portions of the network might be simulated to
@; optimize efficiency or compatibility at run time and on the fly. Then,
@; processes have more reasons to be mobile. And we can start breeding
@; Jarvis-like companion processes that follow us (or other targets) around
@; strategically, offering advice and other assistance autonomously.

@; Like 15 years ago, when "big data" was becoming a common phrase, people would
@; go on about how our ability to collect data is outstripping our ability to
@; understand it. People act like the problem has been solved, but actually we
@; just stopped caring. The data we generate is mostly invisible, hidden behind
@; the decades-long accumulation of slick user interfaces and proprietary back
@; ends. That data is valuable, and I want to tap into it directly.

@; I'm confident `neuron` is not "reactive" and does not enforce the reactor
@; pattern. That'll probably come later, as a pluggable component. For context, I
@; think type checking will also be pluggable.

@; The goal of the `neuron` library is to provide the components for a
@; distributed interpreter and the means to share it with other people. Next, I
@; will build a distributed interpreter for the `neuron` dataflow programming
@; language. It's a combination of pi/network calculus and actor model, where
@; whole "actors" can be communicated across the wire. Every Schemer eventually
@; gets the idea to hook a REPL up to a network socket. This is that, plus a lot
@; of engineering design to make it fun and safe for non-wizards.

@; I want to make it easy to share and integrate functionality at arbitrary
@; granularities. "Cloud" apps are orders of magnitude easier to make this way.
@; They are far cheaper to operate, if done right. I believe they can be "safer,"
@; for many definitions of safety.

@; I expect to find similar advantages in complexity and cost on the front end,
@; though not any time soon.

@; The guiding principles are:
@; * Be a good neighbor.
@; * Trust, but verify.
@; * Grow organically.

@; Roughly, each layer in the API stack "extends" the lower layers into a new
@; dimension of interaction: concurrent (on my device) < distributed (on my
@; network) < decentralized (with other networks) < organized (with other
@; people). That's the roadmap.

@; At first, the priority is interoperability and integration with existing tools
@; and tech. Once the paradigm gains traction, the priority becomes replacing or
@; adding apps of low (dev/op) cost and high impact.

@; Once the dust settles on the process module, Web interop is my next priority.
@; The immediate goal is to make it easy to produce and consume RESTful API
@; endpoints. The next milestone on the `neuron` roadmap is effectively to
@; integrate `net2` for byte streaming.

@table-of-contents[]

@include-section["resources.scrbl"]

@section{Control Your Network}
@subsection{Distributed Evaluation}
@subsubsection{Portability}
@subsubsection{Process Mobility}
@subsection{Capabilities}

@section{Cooperate with Others}
@subsection{Decentralized Run Time Environments}
@subsection{Decentralized Programs}

@section{Grow a Community}
@subsection{Identity}
@subsection{Association}
@subsection{Accountability}
@subsection{Policy}
@subsubsection{Execution}
@subsubsection{Enforcement}

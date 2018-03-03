#lang scribble/manual

@(require "base.rkt")

@title[#:style '(unnumbered)]{The Neuron Technical Report}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

This document is a repository for notes and other documentation that do not
belong in @secref{the Neuron Reference}.

@section{Introduction}

Neuron is a series of Racket libraries that provide a spectrum of
functionality related to the creation, operation, integration, and evolution
of decentralized run time environments and applications. At its core is a
hybrid communication-based concurrency model and a structural pattern-based
DSL for composable evaluators.

@centered[
  @vc-append[
    @hc-append[
      @label[100 35]{Create}
      @vc-append[
        @hc-append[
          @layer[100 35]{Evaluators}
          @layer[100 35]{Processes}
        ]
        @layer[200 35]{Data Flows}
      ]
    ]
    @blank[10]
    @hc-append[
      @label[100 35]{Operate}
      @vc-append[
        @hc-append[
          @layer[100 35]{Consistent}
          @layer[100 35]{Available}
        ]
        @layer[200 35]{Distributed Systems}
      ]
    ]
    @blank[10]
    @hc-append[
      @label[100 35]{Cooperate}
      @vc-append[
        @hc-append[
          @layer[100 35]{Identity}
          @layer[100 35]{Consensus}
          @layer[100 35]{Capabilities}
        ]
        @hc-append[
          @layer[100 35]{Trust}
          @layer[100 35]{Reputation}
          @layer[100 35]{Authorization}
        ]
        @hc-append[
          @layer[200 35]{Agreement}
          @layer[100 35]{Enforcement}
        ]
        @layer[300 35]{Decentralized Applications}
      ]
    ]
    @blank[10]
    @hc-append[
      @label[100 35]{Grow}
      @vc-append[
        @hc-append[
          @vc-append[
            @hc-append[
              @layer[100 35]{Agency}
              @layer[100 35]{Adaptation}
              @layer[100 35]{Reproduction}
            ]
            @layer[300 35]{Organism}
          ]
          @layer[100 70]{Resources}
        ]
        @layer[400 35]{Software Ecosystem}
      ]
    ]
  ]
]

@section{Evaluators}

An evaluator is a fixed-point calculator on terms. More precisely, a
@deftech{term} is either a primitive value or a composite structure over
sub-terms, and an @deftech{evaluator} is a function that applies a stepper to
a term repeatedly until the input and output terms are structurally
equivalent. A @deftech{stepper} is a function that maps an input term to an
output term.

#lang scribble/manual

@(require "base.rkt")

@title[#:style '(unnumbered)]{The Neuron Guide}
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
    10
    @hc-append[
      @label[100 35]{Control}
      @vc-append[
        @hc-append[
          @layer[100 35]{Evaluators}
          @layer[100 35]{Processes}
        ]
        @layer[200 35]{Data Flows}
      ]
    ]
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

@section{Composable Evaluators}

An @deftech{evaluator} calculates the fixed point of a @tech{term}. More
precisely, an evaluator is a function that applies a @tech{stepper} to a
@tech{term} repeatedly until the output equals the input.

@subsection{Terms}

@margin-note{In the future, terms may become syntax objects to enable source
tracking.}

A @deftech{term} is defined recursively as a literal value or a serializable
composite of sub-terms. For example, the symbol

@racketblock['a-symbol]

and the number

@racketblock[123]

are terms because they are literal values. Furthermore, the structures

@racketblock[
  '(a-symbol 123)
]

and

@racketblock[
  #hasheq((a-symbol . 123))
]

are terms because they are @racket[read]/@racket[write]able composites of
literals.

@subsection{Steppers}

A @deftech{stepper} is a function that maps an input @tech{term} to an output
@tech{term}. For example,

@racketblock[
  (match-lambda
    [1 'a]
    [2 'b]
    [_ 'z])
]

is a stepper because it maps any term to @racket['a], @racket['b], or
@racket['c]. Similarly,

@racketblock[
  (case-lambda
    [(a) 1]
    [(b) 2]
    [else 0])
]

is a stepper because it maps any term to a number between @racket[0] and
@racket[2]. More realistically, the @racket[values] procedure is a stepper
because it maps every term to itself, and the function

@racketblock[
(define step
  (match-lambda
    [(cons (? term? e1) (? term? e2)) #:when (not (value? e1))
     (cons (step e1) e2)]
    [(cons (? value? v1) (? term? e2?)) #:when (not (value? e2))
     (cons v1 (step e2))]
    [(cons `(λ ,x11 ,e12) (? value? v2))
     (substitute e12 x11 v2)]
    [_ 'stuck]))
]

is a stepper because it implements the small-step semantics of the untyped
lambda calculus on @tech{terms}.

@section{Communication-based Concurrency}

Neuron provides a concurrency model based on lightweight actor-style
@tech{process}es communicating over first-class named synchronous
@racket-tech{channels}. A @tech{process} is a thread that can clean up after
itself and keep secrets. Concretely, processes imbue threads with life cycle
hooks and two orthogonal lines of communication.

@subsection{The Process Life Cycle}

When a @tech{process} is created, several @tech{hooks} and @tech{handlers} may
be installed. A @deftech{hook} is a function to be invoked automatically at
specific points in the life time of a @tech{process}, and a @deftech{handler}
is a function to be invoked manually by another @tech{process}.

@; nodes
@(define starting-box (label 70 35 "starting"))
@(define alive-box (label 70 35 "alive"))
@(define stopping-box (label 70 35 "stopping"))
@(define dying-box (label 70 35 "dying"))
@(define dead-box (label 70 35 "dead"))

@(define life-cycle-diagram
   @vc-append[
     @starting-box
     @blank[35]
     @alive-box
     @blank[35]
     @hc-append[
       105
       @stopping-box
       @dying-box
     ]
     @blank[25]
     @dead-box
   ])

@; edges
@(set! life-cycle-diagram
  (pin-arrow-line
   10
   life-cycle-diagram
   starting-box cb-find
   alive-box ct-find))
@(set! life-cycle-diagram
  (pin-arrow-line
   10
   life-cycle-diagram
   alive-box lc-find
   stopping-box ct-find
   #:start-angle pi
   #:end-angle (- (/ pi 2))
   #:start-pull 1/2
   #:end-pull 1/2))
@(set! life-cycle-diagram
  (pin-arrow-line
   10
   life-cycle-diagram
   alive-box rc-find
   dying-box ct-find
   #:start-angle 0
   #:end-angle (- (/ pi 2))
   #:start-pull 1/2
   #:end-pull 1/2))
@(set! life-cycle-diagram
  (pin-arrow-line
   10
   life-cycle-diagram
   stopping-box cb-find
   dead-box lc-find
   #:start-angle (- (/ pi 2))
   #:end-angle 0
   #:start-pull 1/2
   #:end-pull 1/2))
@(set! life-cycle-diagram
  (pin-arrow-line
   10
   life-cycle-diagram
   dying-box cb-find
   dead-box rc-find
   #:start-angle (- (/ pi 2))
   #:end-angle pi
   #:start-pull 1/2
   #:end-pull 1/2))

@centered[@life-cycle-diagram]

@subsection{Command Handlers}

@subsection{Channels and Events}

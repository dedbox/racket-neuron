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

maps any term to @racket['a], @racket['b], or @racket['c]. Similarly,

@racketblock[
  (case-lambda
    [(a) 1]
    [(b) 2]
    [else 0])
]

maps any term to a number between @racket[0] and @racket[2]. A more realistic
example is @racket[values], which maps every term to itself; or the function

@racketblock[
(define step
  (match-lambda
    [(list (? term? e1) (? term? e2)) #:when (not (value? e1))
     (list (step e1) e2)]
    [(list (? value? v1) (? term? e2?)) #:when (not (value? e2))
     (list v1 (step e2))]
    [(list `(λ ,(? symbol? x11) ,(? term? e12)) (? value? v2))
     (substitute e12 x11 v2)]
    [_ 'stuck]))
]

a @tech{term}-based small-@tech{stepper} for the untyped lambda calculus.

@section{Communication-based Concurrency}

Neuron uses a concurrency model based on lightweight actor-style
@tech{process}es communicating over first-class named synchronous
@racket-tech{channels}. A @tech{process} is a like a thread that can clean up
after itself and keep ``secrets.'' Concretely, processes imbue
@racket-tech{threads} with life cycle hooks and two orthogonal lines of
communication.

@subsection{The Process Life Cycle}

When a @tech{process} is created, @tech{hooks} and @tech{handlers} may be
installed. A @deftech{hook} is a function to be invoked automatically at
specific points in the life time of a @tech{process}.

@; nodes
@(define starting-box (label 70 35 "starting"))
@(define alive-box (label 70 35 "alive"))
@(define stopping-box (label 70 35 "stopping"))
@(define dying-box (label 70 35 "dying"))
@(define dead-box (label 70 35 "dead"))
@(define blank-box (label 70 35 ""))

@; layout
@(define life-cycle-diagram
   (vl-append
    35
    starting-box
    alive-box
    (hc-append 35 stopping-box dying-box)
    (hc-append 35 blank-box dead-box)))

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
   alive-box cb-find
   stopping-box ct-find))
@(set! life-cycle-diagram
  (pin-arrow-line
   10
   life-cycle-diagram
   stopping-box rc-find
   dying-box lc-find))
@(set! life-cycle-diagram
  (pin-arrow-line
   10
   life-cycle-diagram
   dying-box cb-find
   dead-box ct-find))
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

@centered[@life-cycle-diagram]

@margin-note{In the future, a @emph{paused} state and an @emph{on-pause} hook
might be added.}

A @tech{process} is created in the starting state when another @tech{process}
attempts to spawn a new thread of execution. The requesting @tech{process}
blocks until the new @tech{process} is alive and a fresh @tech{process
descriptor} for it has been returned.

A @tech{process} stays alive until its thread of execution terminates. A
@tech{process} can end itself, either by reaching the end of its program or by
issuing a @racket[quit] or @racket[die] command. A @tech{process} can also use
the @racket[stop] and @racket[kill] commands to end any @tech{process} it
holds a @tech{process descriptor} for.

When a @tech{process} is terminated by @racket[quit] or @racket[stop], it
enters the stopping state while it calls its @tech{on-stop hook}. When a
@tech{process} reaches the end of its program or @tech{on-stop hook}, or is
terminated by a @racket[die] or @racket[kill] command, it enters the dying
state while it calls its @tech{on-dead hook}. A @tech{process} is dead when
its @tech{on-dead hook} returns.

@examples[
  #:eval neuron-evaluator
  #:label "Example:"
  (wait (start (start (process (λ () (displayln 'ALIVE)))
                      #:on-stop (λ () (displayln 'STOP-1))
                      #:on-dead (λ () (displayln 'DEAD-1)))
               #:on-stop (λ () (displayln 'STOP-2))
               #:on-dead (λ () (displayln 'DEAD-2))))
]

The @tech{on-dead hook} is for freeing resources no longer needed by any
@tech{process}. Neuron uses the @tech{on-dead hook} internally to terminate
network listeners and @racket[kill] sub-@tech{process}es. This @tech{hook}
runs unconditionally and can't be canceled.

The @tech{on-stop hook} is for extra or optional clean-up tasks. Neuron uses
the @tech{on-stop hook} to close @racket-tech{ports}, terminate network
connections, and @racket[stop] sub-@tech{process}es. For example, a
@tech{codec} closes its @racket-tech{input port} and @racket-tech{output port}
when stopped---but not when killed, so it can be swapped out mid-stream or
restarted after errors have been handled.

The @racket[deadlock] function waits for the current @tech{process} to
terminate, causing the computation to diverge. It can be used as a termination
``latch'' to prevent the current @tech{process} from ending until stopped or
killed.

@examples[
  #:eval neuron-evaluator
  #:label "Example:"
  (kill (start (start (process deadlock)
                      #:on-stop (λ () (displayln 'STOP-1))
                      #:on-dead (λ () (displayln 'DEAD-1)))
               #:on-stop (λ () (displayln 'STOP-2))
               #:on-dead (λ () (displayln 'DEAD-2))))
]

@subsection{Command Handlers}

Applying a @tech{process descriptor} to an argument list invokes its
@tech{command handler}, a simple dispatch mechanism. Because the @tech{command
handler} is installed while a @tech{process} is starting, it can have direct
access to the internal state of the @tech{process} via the constructing
closure.

Neuron uses the @tech{command handler} to provide simple properties and
methods.

@examples[
  #:eval neuron-evaluator
  #:label "Example:"
  (define π
    (let ([env #hash([(a b) . 1]
                     [(c) . 2])])
      (start (process deadlock)
             #:command (λ args (hash-ref env args #f)))))
  (π 'a 'b)
  (π 'c)
  (π 'd)
]

@tech{Evaluators} and @tech{steppers} can be used as @tech{command handlers},
enabling @tech{term}-based DSLs for programmable @tech{process} interactions.

@subsection{Unbuffered Channels}

@subsection{Information Flow Control}

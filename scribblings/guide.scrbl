#lang scribble/manual

@(require "base.rkt")

@title[#:style '(unnumbered)]{The Neuron Guide}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

This guide provides examples, tutorials, notes and other documentation that do
not belong in @secref{the Neuron Reference}.

@section{Introduction}

Neuron is a series of Racket libraries that provide a consistent API over a
spectrum of functionality related to the creation, operation, integration, and
evolution of concurrent, distributed, and decentralized run time environments
and applications. At its core is a communication-based concurrency model and a
structural pattern-based DSL for working with composable evaluators.

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

Racket evaluators

Steppers

@section{Communication-based Concurrency}

Neuron uses a concurrency model of lightweight processes communicating over
first-class named synchronous @rtech{channels}. @tech{Process}es extend
@rtech{threads} with support for life cycle hooks and two orthogonal lines of
communication. In other words, a @tech{process} is like a @rtech{thread} that
can clean up after itself and keep ``secrets.''

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
@tech{process} can terminate itself, either by reaching the end of its program
or by issuing a @racket[quit] or @racket[die] command. A @tech{process} can
also use the @racket[stop] or @racket[kill] command to terminate any
@tech{process} it holds a @tech{process descriptor} for.

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
the @tech{on-stop hook} to close @rtech{ports}, terminate network connections,
and @racket[stop] sub-@tech{process}es. For example, a @tech{codec} closes its
@rtech{input port} and @rtech{output port} when stopped---but not when killed,
so it can be swapped out mid-stream or restarted after errors have been
handled.

The @racket[deadlock] function waits for the current @tech{process} to
terminate, allowing the computation to diverge efficiently. It can be used as
a termination ``latch'' to prevent the current @tech{process} from ending
until stopped or killed.

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

@tech{Steppers} can be used as @tech{command handlers}, enabling
@tech{term}-based DSLs for out-of-band @tech{process} control.

@subsection{Unbuffered Channels}

@tech{Process}es can also communicate by exchanging values through their
@tech{input channel} and @tech{output channel}. Most of the @tech{process}
constructors provided by Neuron are designed for channel-based data flow
networking, hence the distinction between ``in-band'' channel-mediated
exchanges versus ``out-of-band'' command invocations.

The @racket[server] command creates a @tech{process} that follows the
request-reply pattern for in-band exchanges. This is useful for providing
in-band access to the @tech{command handler} of a @tech{process}.

@examples[
  #:eval neuron-evaluator
  #:label "Example:"
  (define π (start (process deadlock) #:command add1))
  (define cmd (server π))
  (call cmd 1)
]

@subsection{Data Flow Control}

Processes can also be combined in non-trivial ways to provide restricted or
revocable access to others.

@examples[
  #:eval neuron-evaluator
  #:label "Example of restriction:"
  (define π (sexp-codec (string-socket #:in "12 34 56" #:out #t)))
  (define to-π (proxy-to π))
  (define from-π (proxy-from π))
  (recv from-π)
  (give to-π 'abc)
  (get-output-string (π 'socket))
  (sync/timeout 0 (recv-evt to-π))
  (sync/timeout 0 (give-evt from-π))
]

@examples[
  #:eval neuron-evaluator
  #:label "Example of revocation:"
  (define π (sexp-codec (string-socket #:in "12 34 56")))
  (define π-ref (proxy π))
  (recv π-ref)
  (code:line (kill π-ref) (code:comment "after sharing π-ref"))
  (code:line (recv π-ref) (code:comment "no longer works"))
  (code:line (recv π) (code:comment "but π still does"))
]

@subsection{Information Flow Control}

@subsection{Working with Threads}

@tech{Process}es and @rtech{threads} can work together.

@examples[
  #:eval neuron-evaluator
  #:label "Example:"
  (define π
    (process
     (λ ()
       (sync
        (apply evt-set
               (for/list ([i 10])
                 (thread
                  (λ ()
                    (sleep (/ (random) 10.0))
                    (emit i)))))))))
  (for/list ([_ 10]) (recv π))
]

#lang scribble/manual

@(require "base.rkt"
          (prefix-in : "drawings.rkt"))

@title[
  #:style '(unnumbered)
  #:tag "The Neuron Guide"
]{The Neuron Guide}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

This guide provides examples, tutorials, notes and other documentation that do
not belong in @secref{The Neuron Reference}.

@section{Introduction}

Neuron is a series of Racket libraries that provide a consistent API over a
spectrum of functionality related to the creation, operation, integration, and
evolution of concurrent, distributed, and decentralized run time environments
and applications. At its core is a communication-based concurrency model and a
structural pattern-based DSL for working with composable evaluators.

@(:named-picts
  [@:val{Control}
   (:block-diagram
    [@:block{Stepper} @:block{Process}]
    [    @:block{Data Flow} 'cont     ])]
  [@:val{Operate}
   (:block-diagram
    [@:block{Consistency} @:block{Availability}]
    [     @:block{Distributed System} 'cont    ])]
  [@:val{Cooperate}
   (:block-diagram
    [@:block{Identity} @:block{Consensus}  @:block{Capability}   ]
    [@:block{Trust}    @:block{Reputation} @:block{Authorization}]
    [       @:block{Agreement} 'cont       @:block{Enforcement}  ]
    [       @:block{Decentralized Application} 'cont 'cont       ])]
  [@:val{Grow}
   (:block-diagram
    [(:blocks
      [@:block{Agency} @:block{Adaptation} @:block{Reproduction}]
      [@:block{Organism} 'cont 'cont])
     @:block{Resources}]
    [@:block{Software Ecosystem} 'cont])])

@section{Communication-based Concurrency}

Neuron uses a concurrency model of lightweight processes communicating over
named synchronous @tech{exchangers}. Neuron processes extend Racket
@rtech{threads} with support for life cycle hooks and two orthogonal lines of
communication. In other words, a @tech{process} is like a @rtech{thread} that
can clean up after itself and keep ``secrets.''

@subsection{The Process Life Cycle}

When a @tech{process} is created, @tech{hooks} and @tech{handlers} may be
installed. A @deftech{hook} is a function to be invoked automatically at
specific points in the life time of a @tech{process}.

@(centered
  (let ()
    (define starting @:val{starting})
    (define alive @:val{alive})
    (define stopping @:val{stopping})
    (define dying @:val{dying})
    (define dead @:val{dead})
    (define blank1 @:val{ })
    (define blank2 @:val{ })
    (define diagram
      (hc-append
       60
       (vc-append 40 starting blank1)
       (vc-append 40 alive blank2)
       (vc-append 40 stopping dying)
       (vc-append 40 blank1 dead)))
    (set! diagram (pin-arrow-line 10 diagram starting rc-find alive    lc-find))
    (set! diagram (pin-arrow-line 10 diagram alive    rc-find stopping lc-find))
    (set! diagram (pin-line          diagram alive    cb-find blank2   cc-find))
    (set! diagram (pin-arrow-line 10 diagram blank2   cc-find dying    lc-find))
    (set! diagram (pin-arrow-line 10 diagram stopping cb-find dying    ct-find))
    (set! diagram (pin-arrow-line 10 diagram dying    rc-find dead     lc-find))
    diagram))

A @tech{process} is created in the starting state when another @tech{process}
attempts to spawn a new thread of execution. The requesting @tech{process}
blocks until the new @tech{process} is alive and a fresh @tech{process
descriptor} for it has been returned.

A @tech{process} stays alive until its thread of execution terminates. A
@tech{process} can terminate itself, either by reaching the end of its program
or by issuing a @racket[quit] or @racket[die] command. A @tech{process} can
also use the @racket[stop] or @racket[kill] command to terminate any
@tech{process} it holds a @tech{process descriptor} for.

When a @tech{process} reaches the end of its program or is terminated by
@racket[quit] or @racket[stop], it enters the stopping state while it calls
its @tech{on-stop hook}. When a @tech{process} reaches the end of its
@tech{on-stop hook} or is terminated by a @racket[die] or @racket[kill]
command, it enters the dying state while it calls its @tech{on-dead hook}. A
@tech{process} is dead when its @tech{on-dead hook} returns.

@examples[
  #:eval neuron-evaluator
  #:label #f
  (wait (start (start (process (λ () (displayln 'ALIVE)))
                      #:on-stop (λ () (displayln 'STOP-1))
                      #:on-dead (λ () (displayln 'DEAD-1)))
               #:on-stop (λ () (displayln 'STOP-2))
               #:on-dead (λ () (displayln 'DEAD-2))))
]

The @tech{on-dead hook} is for freeing resources no longer needed by any
@tech{process}. Neuron uses the @tech{on-dead hook} internally to terminate
network listeners and @racket[kill] sub-processes. This @tech{hook} runs
unconditionally and can't be canceled.

The @tech{on-stop hook} is for extra or optional clean-up tasks. Neuron uses
the @tech{on-stop hook} to close @rtech{ports}, terminate network connections,
and @racket[stop] sub-processes. For example, some @tech{codecs} close
@rtech{input ports} and @rtech{output ports} when stopped but not when killed
so they can be swapped out mid-stream or restarted after errors have been
handled.

The @racket[deadlock] function waits for the current @tech{process} to
terminate, allowing the computation to diverge efficiently. It can be used as
a termination ``latch'' to prevent the current @tech{process} from ending
until stopped or killed.

@examples[
  #:eval neuron-evaluator
  #:label #f
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
  #:label #f
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
@tech{term}-based DSLs for privileged control.

@subsection[#:tag "guide:Data Flow"]{Data Flow}

Processes can be combined to provide restricted or revocable access to others.

@examples[
  #:eval neuron-evaluator
  #:label "Restriction:"
  (define π (sexp-codec (string-socket #:in "12 34 56" #:out #t)))
  (define to-π (proxy-to π))
  (define from-π (proxy-from π))
  (recv from-π)
  (give to-π 'abc)
  (get-output-string (π 'socket))
  (or (sync/timeout 0 (recv-evt to-π))
      (sync/timeout 0 (give-evt from-π)))
]

@examples[
  #:eval neuron-evaluator
  #:label "Revocation:"
  (define A
    (process
     (λ ()
       (define π-ref (take))
       (displayln `(IN-A ,(recv π-ref)))
       (emit) (take) (code:comment "B kills π-ref")
       (displayln `(IN-A ,(recv π-ref))))))
  (define B
    (process
     (λ ()
       (define π (sexp-codec (string-socket #:in "12 34 56")))
       (define π-ref (proxy π))
       (give A π-ref)
       (recv A) (code:comment "A reads live π-ref")
       (kill π-ref)
       (give A) (wait A) (code:comment "A reads dead π-ref")
       (displayln `(IN-B ,(recv π))))))
   (sync (evt-set A B #:then void))
]

@subsection{Working with Threads}

Processes and @rtech{threads} can be combined.

@examples[
  #:eval neuron-evaluator
  #:label "Multiple producers:"
  (define (producer i)
    (thread (λ () (sleep (/ (random) 10.0)) (emit i))))
  (define (make-producers)
    (apply evt-set (for/list ([i 10]) (producer i))))
  (define π (process (λ () (sync (make-producers)))))
  (for/list ([_ 10])
    (recv π))
]

@examples[
  #:eval neuron-evaluator
  #:label "Multiple consumers:"
  (define (consumer)
    (thread (λ () (write (take)))))
  (define (make-consumers)
    (apply evt-set (for/list ([_ 10]) (consumer))))
  (define π (process (λ () (sync (make-consumers)))))
  (for ([i 10])
    (give π i))
]

@section[#:tag "guide:Evaluation"]{Evaluation}

A @deftech{term} is defined recursively as a literal value or a serializable
composite of sub-terms. For example, the symbol

@racketblock['a-symbol]

and the number

@racketblock[123]

are terms because they are literal values. The structures

@racketblock[
  '(a-symbol 123)
]

and

@racketblock[
  #hasheq((a-symbol . 123))
]

are also terms because they are @racket[read]/@racket[write]able composites of
literals.

A @deftech{stepper} is a function that maps one @tech{term} to another. For
example,

@racketblock[
  (case-lambda
    [(a) 1]
    [(b) 2]
    [else 0])
]

maps any term to a number between @racket[0] and @racket[2]. Similarly,

@racketblock[
  (match-lambda
    [1 'a]
    [2 'b]
    [_ 'z])
]

maps any term to @racket['a], @racket['b], or @racket['z]. A more realistic
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

a small-@tech{stepper} for the untyped lambda calculus.

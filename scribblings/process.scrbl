#lang scribble/doc

@(require "base.rkt")

@title{Process}

@(defmodule neuron/process #:packages ("neuron"))

@(define (racket-tech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@(define neuron-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base '(require neuron))))

@; Dendrites are synaptic sites. Several of a neuron's dendrites may fire
@; simultaneously.

A @deftech{process} is a @racket-tech{thread}-like concurrency primitive.
Processes extend the Racket @racket-tech{thread} model with four new features:

@itemlist[
  @item{A pair of unbuffered @racket-tech{channels} built in: an
    @deftech{input channel} and an @deftech{output channel}.}
  @item{An @deftech{on-stop hook} to call when a process ends gracefully, but
    not when it dies abruptly.}
  @item{An @deftech{on-dead hook} to call unconditionally when a process
    terminates.}
  @item{An out-of-band @tech{command handler}.}
]

Unhandled exceptions are fatal. Attempting to @racket[wait] on a process
killed by an unhandled exception raises @racket[unhandled-exception].

@examples[#:eval neuron-evaluator
  (define π (start (λ () (raise 'VAL))))
  (eval:error (wait π))
]

A process can be applied as a procedure, which invokes its @deftech{command
handler}. The @tech{command handler} can be any procedure. The default
@tech{command handler} immediately and unconditionally raises
@racket[unhandled-command].

@examples[#:eval neuron-evaluator
  (define H (hash 'prop1 1 'method2 (λ _ 2)))
  (define π (start deadlock #:command (λ vs
                                        (or (hash-ref H (car vs) #f)
                                            (raise (unhandled-command vs))))))
  (π 'prop1)
  ((π 'method2) 5)
  (eval:error (π 'x 'y))
]

A process can be used as a @racket-tech{synchronizable event}. A process is
@racket-tech{ready for synchronization} when @racket[wait] would not block.
The synchronization result is the process itself.

@section{Starting and Stopping Processes}

Processes are created explicitly by the @racket[start] function.

@defproc[(current-process) process?]{
  Returns the @tech{process descriptor} for the currently executing process.
}

@defproc[(quit [v any/c] ...) void?]{
  Gracefully terminates the current process, ignoring any arguments.
}

@defproc[(die [v any/c] ...) void?]{
  Immediately terminates the current process, ignoring any arguments.
}

@defproc[(deadlock [v any/c] ...) void?]{
  Hangs the current process, ignoring any arguments.
}

@defproc[(process? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{process}, @racket[#f]
  otherwise.
}

@defproc[(dead? [π process?]) boolean?]{
  Returns @racket[#t] if @racket[π] has terminated, @racket[#f] otherwise.
}

@defproc[(alive? [π process?]) boolean?]{
  Returns @racket[#t] if @racket[π] is not dead, @racket[#f] otherwise.
}

@defproc[(start [thunk (-> any)]
                [#:on-stop on-stop (-> any) void]
                [#:on-dead on-dead (-> any) void]
                [#:command handler (or/c procedure? (listof procedure?)) null]
                ) process?]{
  Calls @racket[thunk] with no arguments in a new process. Installs
  @racket[on-stop] as its @tech{on-stop hook}, @racket[on-dead] as its
  @tech{on-dead hook}, and @racket[handler] as its @tech{command handler}.
  Returns immediately with a @deftech{process descriptor} value.
}

@defproc[(stop [π process?]) void?]{
  Gracefully terminates the execution of @racket[π] if it is running. Blocks
  until @racket[wait] would not block. If @racket[π] is already dead,
  @racket[stop] has no effect.
}

@defproc[(kill [π process?]) void?]{
  Immediately terminates the execution of @racket[π] if it is running. Blocks
  until @racket[wait] would not block. If @racket[π] is already dead,
  @racket[kill] has no effect.
}

@defproc[(wait [π process?]) void?]{
  Blocks execution of the current process until @racket[π] is dead.
}

@defstruct*[unhandled-exception ([value any/c]) #:transparent]{
  Raised when attempting to @racket[wait] on a process killed by an unhandled
  exception.
}

@defstruct*[unhandled-command ([args (listof any/c)]) #:transparent]{
  Raised when a @tech{command handler} cannot determine how to handle
  @racket[args].
}

@section{Inter-Process Communication}

@defproc[(give [π process?]
               [v any/c (void)]) boolean?]{
  Blocks until @racket[π] is ready to accept the value @racket[v] on its
  @tech{input channel}, or until @racket[π] is dead. Returns @racket[#t] if
  @racket[π] accepted @racket[v], @racket[#f] otherwise.
}

@defproc[(take) any/c]{
  Blocks until a sender is ready to provide a value on the @tech{input
  channel} of the current process. Returns the provided value.
}

@defproc[(try-take) any/c]{
  Returns the value, if any, provided on the @tech{input channel} of the
  current process, or @racket[#f] if no value is available.
}

@defproc[(emit [v any/c (void)]) void?]{
  Blocks until a receiver is ready to accept the value @racket[v] through the
  @tech{output channel} of the current process.
}

@defproc[(recv [π process?]) any/c]{
  Blocks until @racket[π] is ready to provide a value through its @tech{output
  channel}, or until @racket[π] is dead. Returns the provided value or
  @racket[eof].
}

@defproc[(try-recv [π process?]) any/c]{
  Returns the value, if any, provided on the @tech{output channel} of
  @racket[π], or @racket[#f] if no value is available.
}

@defproc[(call [π process?] [v any/c (void)]) any/c]{
  Gives @racket[v] to @racket[π] and then immediately @racket[recv]s from
  @racket[π]. Returns the received value.
}

@defproc[(give-evt [π process?] [v any/c (void)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[π] is ready to accept
  the value @racket[v] on its @tech{input channel}, or until @racket[π] is
  dead. The @racket-tech{synchronization result} is @racket[#t] if @racket[π]
  accepted @racket[v], @racket[#f] otherwise.
}

@defproc[(take-evt) evt?]{
  Returns a constant @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when a sender is ready to provide a
  value on the @tech{input channel} of the current process. The
  @racket-tech{synchronization result} is the provided value.
}

@defproc[(emit-evt [v any/c (void)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when a receiver is ready to accept
  the value @racket[v] through the @tech{output channel} of the current
  process.
}

@defproc[(recv-evt [π process?]) evt?]{
  Returns a constant @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[π] is ready to provide
  a value through its @tech{output channel}, or until @racket[π] is dead. The
  @racket-tech{synchronization result} is the provided value or @racket[eof].
}

@section{Control Flow}

@defform[(forever body ...)]{
  Evaluates @racket[body]s repeatedly.
}

@defform[(while expr body ...)]{
  Evaluates @racket[body]s repeatedly for as long as @racket[expr] evaluates
  to @racket[#t].
}

@defform[(until expr body ...)]{
  Evaluates @racket[body]s repeatedly for as long as @racket[expr] evalutes to
  @racket[#f].
}

@defproc[(evt-set [evt evt?] ...) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all @racket[evt]s have become
  @racket-tech{ready for synchronization}. The @racket-tech{synchronization
  result} of @racket[evt-set] is a list of the @racket-tech{synchronization
  results} of @racket[evt]s in the order specified.
}

@defproc[(evt-sequence [make-evt (-> evt?)] ...+) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all events generated by
  @racket[make-evt]s have become @racket-tech{ready for synchronization}.
  Calls each @racket[make-evt] in the order specified, with no arguments, and
  immediately @racket[sync]s the result. The @racket-tech{synchronization
  result} of @racket[evt-sequence] is the same as the
  @racket-tech{synchronization result} of the last event generated.
}

@defproc[(evt-series [#:init init any/c (void)]
                     [make-evt (-> any/c evt?)] ...+
                     ) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all events generated by
  @racket[make-evt]s have become @racket-tech{ready for synchronization}.
  Calls each @racket[make-evt] in the order specified, with a single argument,
  and immediately @racket[sync]s the result. Applies @racket[make-evt] first
  to @racket[init], then to the @racket-tech{synchronization result} of the
  previous event. The @racket-tech{synchronization result} of
  @racket[evt-series] is the same as the @racket-tech{synchronization result}
  of the last event generated.
}

@defproc[(evt-loop [#:init init any/c (void)]
                   [next-evt (-> any/c evt?)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that is never
  @racket-tech{ready for synchronization}. Repeatedly calls @racket[next-evt]
  with a single argument and immediately @racket[sync]s the result. Applies
  @racket[next-evt] first to @racket[init], then to the
  @racket-tech{synchronization result} of the previous event.
}

@defproc[(server [proc (-> any/c any/c)]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler (or/c procedure? (listof procedure?)) null]
                 ) process?]{
  Returns a @deftech{server process}. Applies @racket[proc] to each value
  taken and emits the result.
}

@defproc[(sink [proc (-> any/c void?)]
               [#:on-stop on-stop (-> any) void]
               [#:on-dead on-dead (-> any) void]
               [#:command handler (or/c procedure? (listof procedure?)) null]
               ) process?]{
  Returns a @deftech{sink process}. Applies @racket[proc] to each value taken
  and ignores the result.
}

@defproc[(source [proc (-> any/c)]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler (or/c procedure? (listof procedure?)) null]
                 ) process?]{
  Returns a @deftech{source process}. Calls @racket[proc] with no arguments
  repeatedly and emits each result.
}

@defproc[(socket [snk process?]
                 [src process?]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler (or/c procedure? (listof procedure?)) null]
                 ) process?]{
  Returns a @deftech{socket process}. Forwards to @racket[snk] and from
  @racket[src]. Stops @racket[snk] and @racket[src] when it stops. Dies when
  @racket[snk] or @racket[src] die.

  Commands:
  @itemlist[
    @item{@racket['sink] -- returns @racket[snk]}
    @item{@racket['source] -- returns @racket[src]}
  ]
}

@defproc[(proxy [π process?]
                [#:on-take on-take (-> any/c any/c) values]
                [#:on-emit on-emit (-> any/c any/c) values]
                [#:on-stop on-stop (-> any) void]
                [#:on-dead on-dead (-> any) void]
                [#:command handler (or/c procedure? (listof procedure?)) π]
                ) process?]{
  Returns a @deftech{proxy process}. Forwards values to and from @racket[π].
  Calls @racket[on-take] and @racket[on-emit] appropriately. Stops @racket[π]
  when it stops. Dies when @racket[π] dies.
}

@defproc[(pipe [π process?] ...
               [#:on-stop on-stop (-> any) void]
               [#:on-dead on-dead (-> any) void]
               [#:command handler (or/c procedure? (listof procedure?)) null]
               ) process?]{
  Returns a @deftech{pipe process}. Calls @racket[π]s in series, implicitly
  starting with @racket[take] and ending with @racket[emit]. Stops all
  @racket[π]s when it stops. Dies when any @racket[π] dies.
}

@defproc[(bridge [π1 process?]
                 [π2 process?]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler (or/c procedure? (listof procedure?)) null]
                 ) process?]{
  Returns a @deftech{bridge process}. Forwards from @racket[π1] to
  @racket[π2], and vice versa. Stops @racket[π1] and @racket[π2] when it
  stops. Dies when @racket[π1] or @racket[π2] die.
}

@defproc[(managed [π process?]
                  [#:on-take-eof on-take-eof (-> any) quit]
                  [#:on-emit-eof on-emit-eof (-> any) quit]
                  [#:on-stop on-stop (-> any) void]
                  [#:on-dead on-dead (-> any) void]
                  [#:command handler (or/c procedure? (listof procedure?)) null]
                  ) process?]{
  Returns a @deftech{managed process}, a specialized @tech{proxy process}.
  Forwards non-@racket[eof] values to and from @racket[π]. Calls
  @racket[on-take-eof] or @racket[on-emit-eof] appropriately when @racket[eof]
  is encountered. Stops @racket[π] when it stops. Dies when @racket[π] dies.
}

@defproc[(shutdown [π process?]) void?]{
  Gives @racket[eof] to @racket[π] and blocks until it dies.
}

@defproc[(shutdown-evt [π process?]) evt?]{
  Gives @racket[eof] to @racket[π] and returns a @racket-tech{synchronizable
  event} that becomes @racket-tech{ready for synchronization} when π dies.
}

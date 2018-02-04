#lang scribble/doc

@(require "base.rkt")

@title{Concurrency}

@(defmodule neuron/concurrency #:packages ("neuron"))

A @deftech{process} is a @racket-tech{thread}-like concurrency primitive.
Processes change Racket's @racket-tech{thread} model in the following ways:

@itemlist[
  @item{Drop the built in @secref["threadmbox" #:doc '(lib
    "scribblings/reference/reference.scrbl")].}
  @item{Add a pair of unbuffered @racket-tech{channels}: an @deftech{input
    channel} and an @deftech{output channel}.}
  @item{Add an out-of-band @tech{command handler}.}
  @item{Add an @deftech{on-stop hook} that is called when a process ends
    gracefully, but not when it dies abruptly.}
  @item{Add an @deftech{on-dead hook} that is called unconditionally when a
    process terminates.}
]

A process can be applied as a procedure, which invokes its @deftech{command
handler}. The @tech{command handler} is a list of procedures, and the result
of the command is the same as the result of the first procedure in the list to
return a value other than @racket[unhandled]. When a procedure returns
@racket[unhandled], the next procedure is tried. If the last procedure in the
list returns @racket[unhandled] or the list is empty,
@racket[unhandled-command] is raised.

@examples[
  #:eval neuron-evaluator
  #:label #f
  (define H (hash 'property-A 1
                  'method-B (λ _ 2)))
  (define π
    (start (process deadlock)
           #:command (λ vs (hash-ref H (car vs) unhandled))))
  (π 'property-A)
  ((π 'method-B) 5)
  (eval:error (π 'x 'y))
]

A process can be used as a @racket-tech{synchronizable event}. A process is
@racket-tech{ready for synchronization} when @racket[dead?] would return
@racket[#t]. The synchronization result is the process itself.

Unhandled exceptions are fatal. Attempting to synchronize a process killed by
an unhandled exception re-raises the exception.

@examples[
  #:eval neuron-evaluator
  #:label #f
  (define π (process (λ () (raise 'VAL))))
  (eval:error (sync π))
]

@section{Starting and Stopping Processes}

Processes are created explicitly by the @racket[process] function. Use
@racket[start] to install hooks.

@defstruct*[unhandled-command ([args (listof any/c)]) #:transparent]{
  Raised when a @tech{command handler} procedure cannot determine how to
  handle @racket[args].
}

@defthing[unhandled symbol?]{
  Return this unreadable symbol from your @tech{command handler} to indicate
  that it does not know how to handle a command.
}

@defproc[(process? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{process}, @racket[#f]
  otherwise.
}

@defproc[(process [thunk (-> any)]) process?]{
  Calls @racket[thunk] with no arguments in a new @tech{process}. Returns
  immediately with a @deftech{process descriptor} value.
}

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

@defform[(start π-expr hook ...)
         #:grammar
         [(hook (code:line #:on-stop on-stop)
                (code:line #:on-dead on-dead)
                (code:line #:command handler))]]{
  Installs @racket[hook]s into any processes created by @racket[π-expr].
}

@defproc[(stop [π process?]) void?]{
  Gracefully terminates the execution of @racket[π] if it is running. Blocks
  until @racket[π] is dead. If @racket[π] is already dead, @racket[stop] has
  no effect.
}

@defproc[(kill [π process?]) void?]{
  Immediately terminates the execution of @racket[π] if it is running. Blocks
  until @racket[π] is dead. If @racket[π] is already dead, @racket[kill] has
  no effect.
}

@defproc[(dead? [π process?]) boolean?]{
  Returns @racket[#t] if @racket[π] has terminated, @racket[#f] otherwise.
}

@defproc[(alive? [π process?]) boolean?]{
  Returns @racket[#t] if @racket[π] is not dead, @racket[#f] otherwise.
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

@defform[(apply-values proc expr)]{
  Evaluates @racket[expr] and applies @racket[proc] to the values produced.
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

@defproc[(server [proc (-> any/c any/c)]) process?]{
  Returns a @deftech{server} process. Applies @racket[proc] to each value
  taken and emits the result.
}

@defproc[(sink [proc (-> any/c any)]) process?]{
  Returns a @deftech{sink} process. Applies @racket[proc] to each value taken
  and ignores the result.
}

@defproc[(source [proc (-> any/c)]) process?]{
  Returns a @deftech{source} process. Calls @racket[proc] with no arguments
  repeatedly and emits each result.
}

@defproc[(socket [snk process?] [src process?]) process?]{
  Returns a @deftech{socket} process. Forwards to @racket[snk] and from
  @racket[src]. Stops @racket[snk] and @racket[src] when it stops. Dies when
  both @racket[snk] and @racket[src] die.

  Commands:
  @itemlist[
    @item{@racket['sink] -- returns @racket[snk]}
    @item{@racket['source] -- returns @racket[src]}
  ]
}

@defproc[(service [key-proc (-> any/c any/c)]
                  [#:on-drop on-drop (-> any/c any/c any) void]
                  [#:on-service-stop on-svc-stop (-> any/c any/c any) void]
                  ) process?]{
  Returns a @deftech{service} process. Associates each value given with a key
  generated by applying @racket[key-proc] to the value. Emits generated keys.
  Applies @racket[on-drop] to each key-value pair it drops. Applies
  @racket[on-svc-stop] to every key-value pair when it stops.

  Commands:

  @itemlist[
    @item{@racket['keys] -- returns a list of keys in use}
    @item{@racket['values] -- returns a list of values in use}
    @item{@racket['get key] -- returns the value associated with key}
    @item{@racket['drop key] -- drops @racket[key]; returns @racket[#t] if key
      was in use, @racket[#f] otherwise.}
  ]
}

@defproc[(simulator [proc (-> real? any)] [#:rate rate real? 10]) process?]{
  Returns a @deftech{simulator} process. Repeatedly calls @racket[proc] at a
  frequency of @racket[rate] times per second. Applies @racket[proc] to a
  single argument containing the number of milliseconds since the last call.
}

@defproc[(proxy [π process?]
                [#:on-take on-take (-> any/c any/c) values]
                [#:on-emit on-emit (-> any/c any/c) values]
                ) process?]{
  Returns a @deftech{proxy} process. Forwards values to and from @racket[π].
  Calls @racket[on-take] and @racket[on-emit] appropriately. Stops @racket[π]
  when it stops. Dies when @racket[π] dies.
}

@defproc[(pipe [π process?] ...+) process?]{
  Returns a @deftech{pipe} process. Calls @racket[π]s in series, implicitly
  starting with @racket[take] and ending with @racket[emit]. Stops all
  @racket[π]s when it stops. Dies when any @racket[π] dies.
}

@defproc[(bridge [π1 process?] [π2 process?]) process?]{
  Returns a @deftech{bridge} process. Forwards from @racket[π1] to
  @racket[π2], and vice versa. Stops @racket[π1] and @racket[π2] when it
  stops. Dies when @racket[π1] or @racket[π2] die.

  Commands:

  @itemlist[
    @item{@racket['process 1] -- returns @racket[π1]}
    @item{@racket['process 2] -- returns @racket[π2]}
  ]

  A bridge will attempt to forward unrecognized commands---first to
  @racket[π1], then to @racket[π2]---before raising
  @racket[unhandled-command].
}

@defproc[(managed [π process?]
                  [#:on-take-eof on-take-eof (-> process? any) stop]
                  [#:on-emit-eof on-emit-eof (-> process? any) stop]
                  ) process?]{
  Returns a @deftech{managed} process. Forwards non-@racket[eof] values to and
  from @racket[π]. Calls @racket[on-take-eof] or @racket[on-emit-eof]
  appropriately when @racket[eof] is encountered. Stops @racket[π] when it
  stops. Dies when @racket[π] dies.
}

@defproc[(shutdown [π process?]) void?]{
  Gives @racket[eof] to @racket[π] and blocks until it dies.
}

@defproc[(shutdown-evt [π process?]) evt?]{
  Gives @racket[eof] to @racket[π] and returns a @racket-tech{synchronizable
  event} that becomes @racket-tech{ready for synchronization} when @racket[π]
  dies.
}

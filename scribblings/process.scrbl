#lang scribble/doc

@(require "base.rkt")

@title{Process}

@(defmodule neuron/process #:packages ("neuron"))

@(define (racket-tech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@; Dendrites are synaptic sites. Several of a neuron's dendrites may fire
@; simultaneously.

A @deftech{process} is a @racket-tech{thread}-like concurrency primitive.
Processes extend the Racket @racket-tech{thread} model with four features:

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
killed by an unhandled exception raises @racket[exn:unhandled].

@; @examples[
@racketblock[
  (define π (start (λ () (raise 'VAL))))
  (wait π)
]

A process can be applied as a procedure, which invokes its @deftech{command
handler}. The @tech{command handler} can be any procedure.

Example:
@; @examples[
@racketblock[
  (define π (start deadlock #:command (hash 'prop1 1 'method2 (λ _ 2))))
  (π 'prop1)
  ((π 'method2) 5)
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
                [#:command handler procedure? void]
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

@defstruct*[exn:unhandled ([value any/c]) #:transparent]{
  Raised when attempting to @racket[wait] on a process killed by an unhandled
  exception.
}

@section{Inter-Process Communication}

@defthing[msg/c flat-contract?]{
  A flat contract that accepts any non-@racket[eof] value.
}

@defproc[(give [π process?]
               [v msg/c (void)]) boolean?]{
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
  current process, or @racket[#f] immediately if no value is available.
}

@defproc[(emit [v msg/c (void)]) void?]{
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
  @racket[π], or @racket[#f] immediately if no value is available.
}

@defproc[(give-evt [π process?] [v msg/c (void)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(give π v)] would not
  block. The @racket-tech{synchronization result} is the same as the
  @racket[give] result.
}

@defproc[(take-evt) evt?]{
  Returns a constant @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(take)] would not
  block. The @racket-tech{synchronization result} is the same as the
  @racket[take] result.
}

@defproc[(emit-evt [v msg/c (void)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(emit v)] would not
  block. The @racket-tech{synchronization result} is the same as the
  @racket[emit] result.
}

@defproc[(recv-evt [π process?]) evt?]{
  Returns a constant @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(recv π)] would not
  block. The @racket-tech{synchronization result} is the same as the
  @racket[recv] result.
}

@section{Control Flow}

@defform[(forever body ...)]{
  Repeatedly evaluates @racket[body]s forever.
}

@defform[(while expr body ...)]{
  Repeatedly evaluates @racket[body]s for as long as @racket[expr] evaluates
  to @racket[#t].
}

@defform[(until expr body ...)]{
  Repeatedly evaluates @racket[body]s for as long as @racket[expr] evalutes to
  @racket[#f].
}

@defproc[(all-evts [evt evt?] ...) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all @racket[evt]s have become
  @racket-tech{ready for synchronization}. The @racket-tech{synchronization
  result} of @racket[all-evts] is a list of the @racket-tech{synchronization
  results} of @racket[evt]s.
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

@defproc[(evt-series [make-evt (-> any/c evt?)] ...+) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all events generated by
  @racket[make-evt]s have become @racket-tech{ready for synchronization}.
  Calls each @racket[make-evt] in the order specified, with a single argument,
  and immediately @racket[sync]s the result. Applies @racket[make-evt] first
  to @racket[(void)], then to the @racket-tech{synchronization result} of the
  previous event. The @racket-tech{synchronization result} of
  @racket[evt-series] is the same as the @racket-tech{synchronization result}
  of the last event generated.
}

@defproc[(evt-loop [next-evt (-> any/c evt?)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that is never
  @racket-tech{ready for synchronization}. Repeatedly calls @racket[next-evt]
  with a single argument and immediately @racket[sync]s the result. Applies
  @racket[next-evt] first to @racket[(void)], then to the
  @racket-tech{synchronization result} of the previous event.
}

@defproc[(server [proc (-> msg/c msg/c)]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler procedure? void]
                 ) process?]{
  Returns a @deftech{server process}. Applies @racket[proc] to each value
  taken and emits the result.
}

@defproc[(sink [proc (-> msg/c void?)]
               [#:on-stop on-stop (-> any) void]
               [#:on-dead on-dead (-> any) void]
               [#:command handler prodecure? void]
               ) process?]{
  Returns a @deftech{sink process}. Applies @racket[proc] to each value taken
  and ignores the result.
}

@defproc[(source [proc (-> any/c)]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler procedure? void]
                 ) process?]{
  Returns a @deftech{source process}. Calls @racket[proc] with no arguments
  repeatedly and emits each result.
}

@defproc[(socket [snk process?]
                 [src process?]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler procedure? void]
                 ) process?]{
  Returns a @deftech{socket process}. Gives to @racket[snk] what it takes.
  Emits what it receives from @racket[src]. Stops @racket[snk] and
  @racket[src] when it stops. Dies when @racket[snk] or @racket[src] die.
}

@defproc[(pipe [π process?] ...
               [#:on-stop on-stop (-> any) void]
               [#:on-dead on-dead (-> any) void]
               [#:command handler procedure? void]
               ) process?]{
  Returns a @deftech{pipe process}. Gives what it takes to the first
  @racket[π]. Iteratively gives to the next @racket[π] what it receives from
  the previous. Emits what it receives from the last @racket[π]. Stops
  all @racket[π]s when it stops. Dies when any @racket[π] dies.
}

@defproc[(bridge [π1 process?]
                 [π2 process?]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler procedure? void]
                 ) process?]{
  Returns a @deftech{bridge process}. Gives to @racket[π2] what it receives
  from @racket[π1] and vice versa. Stops @racket[π1] and @racket[π2] when it
  stops. Dies when @racket[π1] or @racket[π2] die.
}

@defproc[(filter [π process?]
                 [on-take (-> msg/c msg/c) values]
                 [on-emit (-> msg/c msg/c) values]
                 [#:on-stop on-stop (-> any) void]
                 [#:on-dead on-dead (-> any) void]
                 [#:command handler procedure? void]
                 ) process?]{
  Returns a @deftech{filter process}. Applies @racket[on-take] to each value
  taken and gives the result to @racket[π]. Applies @racket[on-emit] to each
  value emitted by @racket[π] and emits the result. Stops @racket[π] when it
  stops. Dies when @racket[π] dies.
}

@defproc[(managed [π process?]
                  [#:on-eof on-eof (-> process? any) stop]
                  [#:on-stop on-stop (-> any) void]
                  [#:on-dead on-dead (-> any) void]
                  [#:command handler procedure? void]
                  ) process?]{
  Returns a @deftech{managed process}. Gives what it takes to @racket[π].
  Emits what it receives from @racket[π]. Applies @racket[on-eof] to
  @racket[π] when it takes or emits @racket[eof]. Stops @racket[π] when it
  stops. Dies when @racket[π] dies.
}

@defproc[(shutdown [π process?]) void?]{
  Gives @racket[eof] to @racket[π]. Blocks until @racket[π] is dead.
}

@defproc[(shutdown-evt [π process?]) evt?]{
  Returns a @racket-tech{synchronizable event} that becomes @racket-tech{ready
  for synchronization} when @racket[(shutdown π)] would not block. The
  @racket-tech{synchronization result} is the same as the @racket[shutdown]
  result.
}

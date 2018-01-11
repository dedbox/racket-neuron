#lang scribble/doc

@(require "base.rkt")

@title{Process}

@(defmodule neuron/process #:packages ("neuron"))

@(define (racket-tech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

@; Dendrites are synaptic sites. Several of a neuron's dendrites may fire
@; simultaneously.

A @deftech{process} is a @racket-tech{thread}-like concurrency abstraction.
Under the hood, processes communicate via @racket-tech{channel}
synchronization. Every process has an @deftech{input channel} and an
@deftech{output channel}.

A process can be used as a @racket-tech{synchronizable event}. A process is
@racket-tech{ready for synchronization} when @racket[wait] would not block.
The synchronization result is the process itself.

@section{Starting and Stopping Processes}

@defproc[(process [thunk (-> any)]
                  [#:on-stop on-stop (-> any) void]
                  [#:command handler (-> any) void]
                  ) process?]{
  Calls @racket[thunk] with no arguments in a new process with
  @deftech{command handler} @racket[handler]. If the process terminates
  gracefully, @racket[on-stop] will be called with no arguments before the
  process dies. Returns immediately with a @deftech{process descriptor} value.

  Unhandled exceptions are fatal to processes. Attempting to @racket[sync] on
  a process killed by an unhandled exception will raise an exception.
}

@defproc[(process? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{process}, @racket[#f]
  otherwise.
}

@defproc[(current-process) process?]{
  Returns the @tech{process descriptor} for the currently executing process.
}

@defproc[(command [π process?]
                  [v any/c]
                  ) any]{
  Applies the @tech{command handler} of @racket[π] to @racket[v] inside the
  calling process and returns the result.
}

@defproc[(quit [v any/c] ...) void?]{
  Gracefully terminates the current process. Each @racket[v] argument is
  ignored.
}

@defproc[(die [v any/c] ...) void?]{
  Immediately terminates the current process. Each @racket[v] argument is
  ignored.
}

@defproc[(stop [π process?]) void?]{
  Gracefully terminates the execution of @racket[π] if it is running. If the
  process is already dead, @racket[stop] has no effect.
}

@defproc[(kill [π process?]) void?]{
  Immediately terminates the execution of @racket[π] if it is running. If the
  process is already dead, @racket[kill] has no effect.
}

@defproc[(dead? [π process?]) boolean?]{
  Returns @racket[#t] if @racket[π] has terminated, @racket[#f] otherwise.
}

@defproc[(alive? [π process?]) boolean?]{
  Returns @racket[#t] if @racket[π] is not dead, @racket[#f] otherwise.
}

@defproc[(wait [π process?]) void?]{
  Blocks execution of the current process until @racket[π] has terminated.
}

@defproc[(deadlock [v any/c] ...) void?]{
  Deadlocks the current process. Each @racket[v] argument is ignored.
}

@defproc[(dead-evt [π process?]) evt?]{
  Returns a @racket-tech{synchronizable event} that is @racket-tech{ready for
  synchronization} if and only if @racket[π] is dead. The
  @racket-tech{synchronization result} of a dead event is the dead event
  itself.
}

@section{Inter-Process Communication}

@defproc[(give [π process?]
               [v msg/c (void)]) boolean?]{
  Blocks until @racket[π] is ready to accept the value @racket[v] on its
  @tech{input channel}, or until @racket[π] is dead. Returns @racket[#t] if
  @racket[π] accepted @racket[v], @racket[#f] otherwise.
}

@defproc[(give-evt [π process?] [v msg/c (void)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(give π v)] would not
  block. The @racket-tech{synchronization result} is the same as the
  @racket[give] result.
}

@defproc[(take) any/c]{
  Blocks until a sender is ready to provide a value on the @tech{input
  channel} of the current process. Returns the provided value.
}

@defproc[(try-take) any/c]{
  Returns the value, if any, provided on the @tech{input channel} of the
  current process, or @racket[#f] immediately if no value is available.
}

@defproc[(take-evt) evt?]{
  Returns a constant @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(take)] would not
  block. The @racket-tech{synchronization result} is the same as the
  @racket[take] result.
}

@defproc[(emit [v msg/c (void)]) void?]{
  Blocks until a receiver is ready to accept the value @racket[v] through the
  @tech{output channel} of the current process.
}

@defproc[(emit-evt [v msg/c (void)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(emit v)] would not
  block. The @racket-tech{synchronization result} is void.
}

@defproc[(recv [π process?]) any]{
  Blocks until @racket[π] is ready to provide a value through its @tech{output
  channel}, or until @racket[π] is dead. Returns the provided value or
  @racket[eof].
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
  Returns a single event that is @racket-tech{ready for synchronization} when
  all @racket[evt]s are @racket-tech{ready for synchronization}. The
  @racket-tech{synchronization result} is a list of the
  @racket-tech{synchronization results} of @racket[evt]s.
}

@defproc[(seq-evts [make-evt (-> any/c evt?)] ...+) evt?]{
  Returns a single event that is @racket-tech{ready for synchronization} when
  a series of events is @racket-tech{ready for synchronization}. Applies the
  first @racket[make-evt] to @racket[(void)] and the remaining
  @racket[make-evt]s to the @racket-tech{synchronization result} of the prior
  event. The @racket-tech{synchronization result} is the same as the final
  @racket-tech{synchronization result} of the event series.
}

@defproc[(loop-evts [make-evt (-> any/c evt?)] ...+) evt?]{
  Returns an event that is never @racket-tech{ready for synchronization}. The
  returned event is equivalent to a @racket[seq-evts] event that loops over
  @racket[make-evt]s forever.
}

@defproc[(serve [proc (-> msg/c msg/c)]
                [#:on-stop on-stop (-> any) void]
                [#:command handler (-> any) void]
                ) process?]{
  Constructs a @deftech{server process}. Applies @racket[proc] to each value
  taken and emits the result.
}

@defproc[(sink [proc (-> msg/c void?)]
               [#:on-stop on-stop (-> any) void]
               [#:command handler (-> any) void]
               ) process?]{
  Constructs a @deftech{sink process}. Applies @racket[proc] to each value
  taken and ignores the result.
}

@defproc[(source [proc (-> any/c)]
                 [#:on-stop on-stop (-> any) void]
                 [#:command handler (-> any) void]
                 ) process?]{
  Constructs a @deftech{source process}. Calls @racket[proc] with no arguments
  repeatedly and emits each result.
}

@defproc[(socket [snk process?]
                 [src process?]
                 [#:command handler (-> any) void]
                 ) process?]{
  Constructs a @deftech{socket process}. Gives what it takes to @racket[snk].
  Emits what it receives from @racket[src]. Stops @racket[snk] and
  @racket[src] when it stops. Dies when @racket[snk] or @racket[src] die.
}

@defproc[(pipe [π process?] ...
               [#:command handler (-> any) void]
               ) process?]{
  Constructs a @deftech{pipe process}. Gives what it takes to the first
  @racket[π]. Gives what it receives from the prior @racket[π] to the next
  @racket[π]. Emits what it takes from the last @racket[π]. Stops @racket[π]s
  when it stops. Dies when any @racket[π] dies.
}

@defproc[(bridge [π1 process?]
                 [π2 process?]
                 [#:command handler (-> any) void]
                 ) process?]{
  Constructs a @deftech{bridge process}. Gives what it receives from
  @racket[π1] to @racket[π2] and vice versa. Stops @racket[π1] and @racket[π2]
  when it stops. Dies when @racket[π1] or @racket[π2] die.
}

@defproc[(proxy [π process?]
                [on-take (-> msg/c msg/c) values]
                [on-emit (-> msg/c msg/c) values]
                [#:command handler (-> any) void]
                ) process?]{
  Constructs a @deftech{proxy process}. Applies @racket[on-take] to each value
  taken and gives the result to @racket[π]. Applies @racket[on-emit] to each
  value emitted by @racket[π] and emits the result. Stops @racket[π] when it
  stops. Dies when @racket[π] dies.
}

@defproc[(managed [π process?]
                  [#:on-eof on-eof (-> any) stop]
                  [#:on-stop on-stop (-> any) void]
                  [#:command handler (-> any) void]
                  ) process?]{
  Constructs a @deftech{managed process}. Applies @racket[on-eof] to
  @racket[π] when it takes or emits @racket[eof]. Stops @racket[π] when it
  stops. Dies when @racket[π] dies.
}

@defproc[(shutdown [π process?]) void?]{
  Gives @racket[eof] to @racket[π]. Blocks until π is dead.
}

@defproc[(shutdown-evt [π process?]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when @racket[(shutdown π)] would not
  block. The @racket-tech{synchronization result} is void.
}

@section{Contracts}

@defthing[msg/c flat-contract?]{
  A flat contract that recognizes all non-@racket[eof] values.
}

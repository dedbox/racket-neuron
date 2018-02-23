#lang scribble/doc

@(require "base.rkt")

@title{Concurrency}

@(defmodule neuron/concurrency #:packages ("neuron"))

A @deftech{process} is a @racket-tech{thread}-like concurrency primitive.
Processes are made from @racket-tech{threads} by replacing the
@seclink["threadmbox" #:doc '(lib
"scribblings/reference/reference.scrbl")]{thread mailbox} with a few other
features:

@itemlist[
  @item{Two unbuffered @racket-tech{channels}: an @deftech{input channel} and
    an @deftech{output channel}.}
  @item{An out-of-band @tech{command handler}.}
  @item{An @deftech{on-stop hook} that is called when a process ends
    gracefully, but not when it dies abruptly.}
  @item{An @deftech{on-dead hook} that is called unconditionally when a
    process terminates.}
]

A process can be applied as a procedure, which invokes its @deftech{command
handler}. The @tech{command handler} is a list of procedures, and the result
of a command is the same as the result of the first procedure in the list to
return a value other than @racket[unhandled]. If every procedure returns
@racket[unhandled] or the list is empty, @racket[unhandled-command] is raised.

@examples[
  #:eval neuron-evaluator
  #:label #f
  (define (dispatch k . _)
    (hash-ref (hasheq 'A 1 'B (λ _ 2)) k unhandled))
  (define π (start (process deadlock) #:command dispatch))
  (π 'A)
  ((π 'B) 5)
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
@racket[start] to install hooks and handlers.

@defthing[unhandled symbol?]{
  Return this value from your @tech{command handler} to indicate that it will
  not handle a command.
}

@defstruct*[unhandled-command ([process process?]
                               [args (listof any/c)]) #:transparent]{
  Raised when a @tech{command handler} cannot determine how to handle
  @racket[args].
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

@defform[(start π-expr hooks-and-handlers ...)
         #:grammar
         [(hooks-and-handlers
           (code:line #:on-stop on-stop)
           (code:line #:on-dead on-dead)
           (code:line #:command handler))]]{
  Installs @racket[hooks-and-handlers] into all processes created in the
  lexical scope of @racket[π-expr].

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π
      (start (process deadlock)
             #:on-stop (λ () (displayln 'STOP1))
             #:on-dead (λ () (displayln 'DEAD1))
             #:on-stop (λ () (displayln 'STOP2))
             #:on-dead (λ () (displayln 'DEAD2))
             #:command add1))
    (π 1)
    (stop π)
  ]
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

@defproc[(wait [π process?]) void? #:value (void (sync π))]{
  Blocks until @racket[π] is @racket-tech{ready for synchronization}. 
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
  Evaluates @racket[expr] and then applies @racket[proc] to the resulting
  values.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (apply-values list (values 1 2 3))
  ]
}

@defproc[(evt-set [evt evt?] ...) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all @racket[evt]s are
  @racket-tech{ready for synchronization}. The @racket-tech{synchronization
  result} is a list of the @racket-tech{synchronization results} of
  @racket[evt]s in the order specified.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (sync
     (evt-set
      (wrap-evt (thread (λ () (sleep 0.1) (write 1))) (λ _ 1))
      (wrap-evt (thread (λ () (write 2))) (λ _ 2))))
  ]
}

@defproc[(evt-sequence [make-evt (-> evt?)] ...+) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all events generated by
  @racket[make-evt]s are @racket-tech{ready for synchronization}. Calls each
  @racket[make-evt] in the order specified and immediately @racket[sync]s the
  result. The @racket-tech{synchronization result} is the same as the
  @racket-tech{synchronization result} of the last event generated.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (sync
     (evt-sequence
      (λ () (wrap-evt (thread (λ () (sleep 0.1) (write 1))) (λ _ 1)))
      (λ () (wrap-evt (thread (λ () (write 2))) (λ _ 2)))))
  ]
}

@defproc[(evt-series [#:init init any/c (void)]
                     [make-evt (-> any/c evt?)] ...+
                     ) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that becomes
  @racket-tech{ready for synchronization} when all events generated by
  @racket[make-evt]s have become @racket-tech{ready for synchronization}.
  Calls each @racket[make-evt] in the order specified and immediately
  @racket[sync]s the result. Applies @racket[make-evt] first to @racket[init],
  then to the @racket-tech{synchronization result} of the previous event. The
  @racket-tech{synchronization result} is the same as the
  @racket-tech{synchronization result} of the last event generated.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (sync
     (evt-series
      #:init 1
      (λ (x) (wrap-evt (thread (λ () (write x))) (λ _ (+ x 2))))
      (λ (x) (wrap-evt (thread (λ () (write x))) (λ _ (+ x 4))))))
  ]
}

@defproc[(evt-loop [#:init init any/c (void)]
                   [next-evt (-> any/c evt?)]) evt?]{
  Returns a fresh @racket-tech{synchronizable event} that is never
  @racket-tech{ready for synchronization}. Repeatedly calls @racket[next-evt]
  and immediately @racket[sync]s the result. Applies @racket[next-evt] first
  to @racket[init], then to the @racket-tech{synchronization result} of the
  previous event.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (with-handlers ([number? values])
      (sync
       (evt-loop
        #:init 1
        (λ (x)
          (if (> x 5) (raise x) (wrap-evt always-evt (λ _ (+ x 1))))))))
  ]
}

@defproc[(server [proc (-> any/c any/c)]) process?]{
  Returns a @deftech{server} process. Applies @racket[proc] to each value
  taken and emits the result.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (server add1))
    (call π 1)
    (call π -1)
  ]
}

@defproc[(proxy [π process?]
                [#:on-take on-take (-> any/c any/c) values]
                [#:on-emit on-emit (-> any/c any/c) values]
                ) process?]{
  Returns a @deftech{proxy} process. Forwards values to and from @racket[π].
  Calls @racket[on-take] and @racket[on-emit] at the appropriate times. Stops
  @racket[π] when it stops. Dies when @racket[π] dies.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π
      (proxy (server (curry * 3))
             #:on-take add1
             #:on-emit sub1))
    (call π 2)
  ]
}

@defproc[(proxy-to-evt [π process?]
                       [#:on-take on-take (-> any/c any/c) values]
                       ) evt?]{
  Returns a constant @racket-tech{synchronizable event} that takes a value and
  applies @racket[on-take] to it, then gives the result to @racket[π]. Becomes
  @racket-tech{ready for synchronization} when @racket[π] either accepts the
  transformed value or dies. The @racket-tech{synchronization result} is
  @racket[#t] if @racket[π] accepts the value, @racket[#f] otherwise.
}

@defproc[(proxy-from-evt [π process?]
                         [#:on-emit on-emit (-> any/c any/c) values]
                         ) evt?]{
  Returns a constant @racket-tech{synchronizable event} that receives a value
  from @racket[π] and applies @racket[on-emit] to it, then emits the result.
  Becomes @racket-tech{ready for synchronization} when another process accepts
  the emitted value.
}

@defproc[(proxy-evt [π process?]
                    [#:on-take on-take (-> any/c any/c) values]
                    [#:on-emit on-emit (-> any/c any/c) values])
         evt?]{
  Returns a constant @racket-tech{synchronizable event} equivalent to

  @racketblock[
    (choice-evt
     (proxy-to-evt π #:on-take on-take)
     (proxy-from-evt π #:on-emit on-emit))
  ]

  Becomes @racket-tech{ready for synchronization} when @racket[π] is dead. The
  @racket-tech{synchronization result} is @racket[π].
}

@defproc[(sink [proc (-> any/c any)]) process?]{
  Returns a @deftech{sink} process. Applies @racket[proc] to each value taken
  and ignores the result.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define i 0)
    (define π (sink (λ (x) (set! i (+ i x)))))
    (give π 1)
    (give π 2)
    i
  ]
}

@defproc[(source [proc (-> any/c)]) process?]{
  Returns a @deftech{source} process. Calls @racket[proc] repeatedly and emits
  each result.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (source random))
    (recv π)
    (recv π)
  ]
}

@defproc[(stream [snk process?] [src process?]) process?]{
  Returns a @deftech{stream} process. Forwards to @racket[snk] and from
  @racket[src]. Stops @racket[snk] and @racket[src] when it stops. Dies when
  both @racket[snk] and @racket[src] die.

  Commands:
  @itemlist[
    @item{@racket['sink] -- returns @racket[snk]}
    @item{@racket['source] -- returns @racket[src]}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π-out (server add1))
    (define π-in (sink (compose (curry give π-out) add1)))
    (call (stream π-in π-out) 1)
  ]
}

@defproc[(simulator [proc (-> real? any)] [#:rate rate real? 10]) process?]{
  Returns a @deftech{simulator} process. Repeatedly calls @racket[proc] at a
  frequency of up to @racket[rate] times per second. Applies @racket[proc] to
  the period corresponding to @racket[rate], in milliseconds.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define i 0)
    (define t (current-inexact-milliseconds))
    (wait
     (simulator
      (λ (p)
        (printf "~a ~a\n" p (- (current-inexact-milliseconds) t))
        (when (> i 2) (die))
        (set! i (add1 i))
        (sleep 0.25))))
  ]
}

@defproc[(pipe [π process?] ...+) process?]{
  Returns a @deftech{pipe} process. Calls @racket[π]s in series, implicitly
  starting with @racket[take] and ending with @racket[emit]. Stops all
  @racket[π]s when it stops. Dies when any @racket[π] dies.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π
      (pipe (server add1)
            (server (curry * 3))
            (server sub1)))
    (call π 2)
  ]
}

@defproc[(bridge [π1 process?] [π2 process?]) process?]{
  Returns a @deftech{bridge} process. Forwards from @racket[π1] to
  @racket[π2], and vice versa. Stops @racket[π1] and @racket[π2] when it
  stops. Dies when @racket[π1] or @racket[π2] die.

  Commands:

  @itemlist[
    @item{@racket[1] -- returns @racket[π1]}
    @item{@racket[2] -- returns @racket[π2]}
  ]

  A bridge will attempt to forward unrecognized commands---first to
  @racket[π1], then to @racket[π2]---before raising
  @racket[unhandled-command].

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (wait
     (bridge
      (server add1)
      (process (λ () (emit 1) (writeln (take))))))
  ]
}

@defproc[(service [key-proc (-> process? any/c)]
                  [#:on-drop on-drop (-> any/c process? any) void])
         process?]{
  Returns a @deftech{service} process. Associates processes to keys generated
  by @racket[key-proc]. When given @racket[(list key v)], forwards @var[v]
  to @var[key]. Emits @racket[(list key v)] when the @tech{process}
  associated with @var[key] emits @var[v]. Applies @racket[on-drop] to each
  key-@tech{process} pair it drops. Drops each @tech{process} that dies. Drops
  every @tech{process} when it stops.

  Commands:

  @itemlist[
    @item{@racket['peers] -- returns an alist of active peers}
    @item{@racket['add] @var[π] -- adds @tech{process} @racket[π] to the set
      of active peers; returns the key associated with @racket[π]}
    @item{@racket['get] @var[key] -- returns the @tech{process} associated
      with @var[key], or @racket[#f] if no such @tech{process} exists}
    @item{@racket['drop] @var[key] -- drops the @tech{process} associated with
      @var[key]; returns @racket[#t] if @var[key] was in use, @racket[#f]
      otherwise.}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define svc
      (service (λ (π) (π))
               #:on-drop (λ (k _) (writeln k))))
    (svc 'add (start (server (curry + 1)) #:command (λ _ 1)))
    (svc 'add (start (server (curry + 2)) #:command (λ _ 2)))
    (svc 'add (start (server (curry + 3)) #:command (λ _ 3)))
    (call svc (list 1 5))
    (call svc (list 3 5))
    (svc 'drop 2)
    (stop svc)
  ]
}

@defproc[(managed [π process?]
                  [#:pre-take-eof pre-take-eof (-> process? any) stop]
                  [#:post-take-eof post-take-eof (-> process? any) void]
                  [#:pre-emit-eof pre-emit-eof (-> process? any) void]
                  [#:post-emit-eof post-emit-eof (-> process? any) stop]
                  ) process?]{
  Returns a @deftech{managed} process. Forwards non-@racket[eof] values to and
  from @racket[π]. Applies @racket[pre-take-eof], @racket[post-take-eof],
  @racket[pre-emit-eof], and @racket[post-emit-eof] at the appropriate times.
  Stops @racket[π] when it stops. Dies when @racket[π] dies.
}

@defproc[(shutdown [π process?]) void?]{
  Gives @racket[eof] to @racket[π] and blocks until it dies.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (managed (server add1)))
    (call π 1)
    (shutdown π)
    (dead? π)
  ]
}

@defproc[(shutdown-evt [π process?]) evt?]{
  Gives @racket[eof] to @racket[π] and returns a @racket-tech{synchronizable
  event} that becomes @racket-tech{ready for synchronization} when @racket[π]
  dies. The @racket-tech{synchronization result} is @racket[π].
}

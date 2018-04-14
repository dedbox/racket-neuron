#lang scribble/doc

@(require "../base.rkt")

@title{Concurrency}

@(defmodule neuron/concurrency #:packages ("neuron"))

A @deftech{process} is a @rtech{thread}-like concurrency primitive. Processes
are made from @rtech{threads} by replacing the @seclink["threadmbox" #:doc
'(lib "scribblings/reference/reference.scrbl")]{thread mailbox} with a few
other features:

@itemlist[
  @item{A pair of @tech{exchangers}: one for transmitting and another for
    receiving.}
  @item{An out-of-band @tech{command handler}.}
  @item{An @deftech{on-stop hook} that is called when a process ends
    gracefully, but not when it dies abruptly.}
  @item{An @deftech{on-dead hook} that is called unconditionally when a
    process terminates.}
]

A process can be applied as a procedure, which invokes its @deftech{command
handler}, or @deftech{handler}. The @tech{command handler} is a list of
procedures, and the result of a command is the same as the result of the first
procedure in the list to return a value other than @racket[unhandled]. If
every procedure returns @racket[unhandled] or the list is empty,
@racket[unhandled-command] is raised.

@examples[
  #:eval neuron-evaluator
  #:label #f
  (define π
    (start
     (process deadlock)
     #:command (bind ([A 1]
                      [B (λ _ 2)])
                     #:else unhandled)))
  (π 'A)
  ((π 'B) 5)
  (eval:error (π '(x y)))
]

A process can be used as a @rtech{synchronizable event}. A process is
@rtech{ready for synchronization} when @racket[dead?] would return
@racket[#t]. The synchronization result is the process itself.

Unhandled exceptions are fatal. Attempting to synchronize a process killed by
an unhandled exception re-raises the exception.

@examples[
  #:eval neuron-evaluator
  #:label #f
  (eval:error (sync (process (λ () (raise 'VAL)))))
]

@section{Mediated Exchange}

@margin-note{@secref{The Neuron Technical Report} explains the difference
between exchangers and @rtech{channels}.}

An @deftech{exchanger} is a @rtech{channel}-based primitive that both
synchronizes a pair of threads and passes a value from one to the other.
Exchangers are synchronous, fair, and support multiple senders and receivers,
but can not be used as @rtech{synchronizable events} directly.

@; In any exchange, one thread puts a value and another thread get it. The
@; @racket[channel-get] and @racket[channel-put] operations model this data flow
@; explicitly. Unfortunately, channels offer no way to tell which side initiates
@; the exchange. Exchangers enable this ability by making the initiating side
@; provide the channel for the exchange.

The participants of an exchange can be characterized by two orthogonal
factors: control flow and data flow. In an exchange, one side waits for the
other to initiate. If the initiating side is transmitting, then the waiting
side is receiving. Similarly, if the initiating side is receiving, then the
waiting side is transmitting. With this distinction, forwarding exchangers
with precise control flow semantics can be defined.

@defproc[(exchanger? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is an @tech{exchanger}, @racket[#f]
  otherwise.

}

@defproc[(make-exchanger) exchanger?]{

  Creates and returns a new exchanger.

}

@defproc[(offer [ex1 exchanger?] [#:to ex2 exchanger?]) void?]{

  Blocks until @var[ex2] is ready to accept @var[ex1].

}

@defproc[(accept [#:from ex exchanger?]) exchanger?]{

  Blocks until an exchanger is offered to @var[ex].

}

@defproc[(put [v any/c] [#:into ex exchanger?]) void?]{

  Blocks until an exchanger is ready to get @var[v] from @var[ex].

}

@defproc[(get [#:from ex exchanger?]) any/c]{

  Blocks until an exchanger puts a value into @var[ex].

}

@subsection{Process Exchangers}

@defproc[(giver [tx exchanger?] [rx exchanger?] [v any/c]) void?]{

  Offers @var[tx] to @var[rx], then puts @var[v] into @var[tx].

}

@defproc[(taker [rx exchanger?]) any/c]{

  Gets a value from an exchanger accepted from @var[rx].

}

@defproc[(receiver [rx exchanger?] [tx exchanger?]) any/c]{

  Offers @var[rx] to @var[tx], then gets a value from @var[rx].

}

@defproc[(emitter [tx exchanger?] [v any/c]) void?]{

  Puts @var[v] into an exchanger accepted from @var[tx].

}

@defproc[(forwarder [ex1 exchanger?] [ex2 exchanger?]) void?]{

  Offers an exchanger accepted from @var[ex1] to @var[ex2].

}

@defproc[
  (filterer [ex1 exchanger?]
            [ex2 exchanger?]
            [#:with proc (-> any/c any/c)])
  void?
]{

  Forwards a value from @var[ex1] to @var[ex2]. Applies @var[proc] to the
  value being forwarded.

}

@defproc[
  (coupler [rx exchanger?] [tx exchanger?] [ex exchanger? (make-exchanger)])
  void?
]{

  Offers @var[ex] to @var[rx] and @var[tx].

}

@defproc[(giver-evt [tx exchanger?] [rx exchanger?] [v any/c]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when @racket[(giver #,(var tx) #,(var rx) #,(var v))] would
  not block.

}

@defproc[(taker-evt [rx exchanger?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(taker #,(var rx))] would not block, and
  the @rtech{synchronization result} is the value taken through @var[rx].

}

@defproc[(receiver-evt [rx exchanger?] [tx exchanger?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(receiver #,(var rx) #,(var tx))] would
  not block, and the @rtech{synchronization result} is the value received
  through @var[rx].

}

@defproc[(emitter-evt [tx exchanger?] [v any/c]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when @racket[(emitter #,(var tx) #,(var v))] would not
  block.

}

@defproc[(forwarder-evt [ex1 exchanger?] [ex2 exchanger?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(forwarder #,(var ex1) #,(var ex2))] would
  not block.

}

@defproc[
  (filterer-evt [ex1 exchanger?]
                [ex2 exchanger?]
                [#:with proc (-> any/c any/c)])
  evt?
]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(filterer #,(var ex1) #,(var ex2) #:with
  #,(var proc))] would not block.

}

@defproc[
  (coupler-evt [rx exchanger?] [tx exchanger?] [ex exchanger? (make-exchanger)])
  evt?
]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(coupler #,(var rx) #,(var tx) #,(var
  ex))] would not block.

}

@section{Control Flow}

@defform[(forever body ...)]{

  Evaluates @var[body]s repeatedly.

}

@defform[(while expr body ...)]{

  Evaluates @var[body]s repeatedly for as long as @var[expr] evaluates to
  @racket[#t].

}

@defform[(until expr body ...)]{

  Evaluates @var[body]s repeatedly for as long as @var[expr] evalutes to
  @racket[#f].

}

@defform[(apply-values proc expr)]{

  Evaluates @var[expr] and then applies @var[proc] to the resulting values.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (apply-values list (values 1 2 3))
  ]
}

@defproc[(evt-set [evt evt?] ...) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when all @var[evt]s are @rtech{ready for synchronization}.
  The @rtech{synchronization result} is a list of the @rtech{synchronization
  results} of @var[evt]s in the order specified.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (sync
     (evt-set
      (wrap-evt (thread (λ () (sleep 0.1) (write 1))) (λ _ 1))
      (wrap-evt (thread (λ () (write 2))) (λ _ 2))))
  ]
}

@defproc[
  (evt-sequence [make-evt (-> evt?)] ...+
                [#:then make-result (-> any/c any) values])
  evt?
]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when all events generated by @var[make-evt]s are
  @rtech{ready for synchronization}. Calls each @var[make-evt] in the order
  specified and immediately @racket[sync]s the result. Wtaps the last
  @var[make-evt] in a @racket[handle-evt] that applies the
  @rtech{synchronization result} of the previous event to @var[make-result].
  The @rtech{synchronization result} of the sequence is the
  @rtech{synchronization result} of its final event.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (sync
     (evt-sequence
      (λ () (wrap-evt (thread (λ () (sleep 0.1) (write 1))) (λ _ 1)))
      (λ () (wrap-evt (thread (λ () (write 2))) (λ _ 2)))))
  ]
}

@defproc[
  (evt-series [#:init init any/c (void)]
              [make-evt (-> any/c evt?)] ...+
              [#:then make-result (-> any/c any) values])
  evt?
]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when all events generated by @var[make-evt]s have become
  @rtech{ready for synchronization}. Calls each @var[make-evt] in the order
  specified and immediately @racket[sync]s the result. Applies @var[make-evt]
  first to @var[init], then to the @rtech{synchronization result} of the
  previous event. Wraps the last @var[make-evt] in a @racket[handle-evt] that
  applies the @rtech{synchronization result} of the previous event to
  @var[make-result]. The @rtech{synchronization result} of the series is the
  @rtech{synchronization result} of its final event.

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

@defproc[
  (evt-loop [#:init init any/c (void)]
            [next-evt (-> any/c evt?)]) evt?
]{

  Returns a fresh @rtech{synchronizable event} that is never @rtech{ready for
  synchronization}. Repeatedly calls @var[next-evt] and immediately
  @racket[sync]s the result. Applies @var[next-evt] first to @var[init], then
  to the @rtech{synchronization result} of the previous event.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (eval:error
      (sync
       (evt-loop
        #:init 1
        (λ (x)
          (if (> x 5)
              (raise x)
              (wrap-evt always-evt (λ _ (+ x 1))))))))
  ]
}

@section{Starting and Stopping Processes}

Processes are created explicitly by the @racket[process] function. Use
@racket[start] to install hooks and handlers.

@defthing[unhandled symbol?]{

  Return this value from a @tech{command handler} to indicate that it will not
  handle a command.

}

@defstruct*[
  unhandled-command ([process process?]
                     [args (listof any/c)]) #:transparent
]{

  Raised when a @tech{command handler} applied to @var[args] returns
  @racket[unhandled].

}

@defproc[(process? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{process}, @racket[#f] otherwise.

}

@defproc[(process [thunk (-> any)]) process?]{

  Calls @var[thunk] with no arguments in a new @tech{process}. Returns
  immediately with a @deftech{process descriptor} value.

}

@defproc[(process-tx [π process?]) transmitter?]{

  Returns the transmitting @tech{exchanger} of @var[π].

}

@defproc[(process-rx [π process?]) transmitter?]{

  Returns the receiving @tech{exchanger} of @var[π].

}

@defform[
  (start π-expr hooks-and-handlers ...)
  #:grammar
  [(hooks-and-handlers
    (code:line #:on-stop on-stop)
    (code:line #:on-dead on-dead)
    (code:line #:command handler))]
]{

  Installs @var[hooks-and-handlers] into all processes created in the
  lexical scope of @var[π-expr].

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π
      (start
       (process deadlock)
       #:on-stop (λ () (displayln 'STOP1))
       #:on-dead (λ () (displayln 'DEAD1))
       #:on-stop (λ () (displayln 'STOP2))
       #:on-dead (λ () (displayln 'DEAD2))
       #:command add1))
    (π 1)
    (stop π)
  ]
}

@defproc[(command [π process?] [v any/c] ...) any]{

  Applies the @tech{command handler} of @var[π] to @var[v]s and returns the
  result. Does not raise @racket[unhandled-command] if the result is
  @racket[unhandled].

}

@defproc[(stop [π process?]) void?]{

  Gracefully terminates the execution of @var[π] if it is running. Blocks
  until @var[π] is dead. If @var[π] is already dead, @racket[stop] has no
  effect.

}

@defproc[(kill [π process?]) void?]{

  Immediately terminates the execution of @var[π] if it is running. Blocks
  until @var[π] is dead. If @var[π] is already dead, @racket[kill] has no
  effect.

}

@defproc[(wait [π process?]) void? #:value (void (sync π))]{

  Blocks until @var[π] is @rtech{ready for synchronization}. 

}

@defproc[(dead? [π process?]) boolean?]{

  Returns @racket[#t] if @var[π] has terminated, @racket[#f] otherwise.

}

@defproc[(alive? [π process?]) boolean?]{

  Returns @racket[#t] if @var[π] is not dead, @racket[#f] otherwise.

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

@section{Inter-Process Communication}

@defproc[(give [π process?] [v any/c (void)]) boolean?]{

  Blocks until @var[π] is ready to accept @var[v] on its receiving
  @tech{exchanger}, or until @var[π] is dead. Returns @racket[#t] if @var[π]
  accepted @var[v], @racket[#f] otherwise.

}

@defproc[(take) any/c]{

  Blocks until a sender is ready to provide a value on the receiving
  @tech{exchanger} of the current process. Returns the provided value.

}

@defproc[(recv [π process?]) any/c]{

  Blocks until @var[π] is ready to provide a value through its transmitting
  @tech{exchanger}, or until @var[π] is dead. Returns the provided value, or
  @racket[eof] if @var[π] died.

}

@defproc[(emit [v any/c (void)]) void?]{

  Blocks until a receiver is ready to accept the value @var[v] through the
  transmitting @tech{exchanger} of the current process.

}

@defproc[(call [π process?] [v any/c (void)]) any/c]{

  Gives @var[v] to @var[π] and then immediately @racket[recv]s from @var[π].
  Returns the received value.

}

@defproc[(forward-to [π process?]) void?]{

  Takes a value and gives it to @var[π].

}

@defproc[(forward-from [π process?]) void?]{

  Emits a value received from @var[π].

}

@defproc[(couple [π1 process?] [π2 process?]) void?]{

  Receives a value from @var[π1] and gives it to @var[π2].

}

@defproc[(give-evt [π process?] [v any/c (void)]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when @var[π] is ready to accept the value @var[v] on its
  receiving @tech{exchanger}, or until @var[π] is dead. The
  @rtech{synchronization result} is @racket[#t] if @var[π] accepted @var[v],
  @racket[#f] otherwise.

}

@defproc[(take-evt) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a sender is ready to provide a value on the
  receiving @tech{exchanger} of the current process. The
  @rtech{synchronization result} is the provided value.

}

@defproc[(recv-evt [π process?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @var[π] is ready to provide a value through its
  transmitting @tech{exchanger}, or until @var[π] is dead. The
  @rtech{synchronization result} is the provided value or @racket[eof].

}

@defproc[(emit-evt [v any/c (void)]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when a receiver is ready to accept the value @var[v]
  through the transmitting @tech{exchanger} of the current process.

}

@defproc[(forward-to-evt [π process?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a value has been taken and then given to
  @var[π].

}

@defproc[(forward-from-evt [π process?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a value has been received from @var[π] and then
  emitted.

}

@defproc[(couple-evt [π1 process?] [π2 process?]) void?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a value has been received from @var[π1] and then
  given to @var[π2].

}

@section{Process Control Flow}

@defproc[(server [proc (-> any/c any/c)]) process?]{

  Applies @var[proc] to each value taken and then emits the result.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (server add1))
    (call π 1)
    (call π -1)
  ]
}

@defproc[(proxy [π process?]) process?]{

  Forwards values to and from @var[π]. Stops @var[π] when it stops. Dies when
  @var[π] dies.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (call (proxy (server (curry * 3))) 2)
  ]
}

@defproc[(proxy-to [π process?]) process?]{

  Gives all values taken to @var[π]. Stops @var[π] when it stops. Dies when
  @var[π] dies.

}

@defproc[(proxy-from [π process?]) process?]{

  Emits all values emitted by @var[π]. Stops @var[π] when it stops. Dies when
  @var[π] dies.

}

@defproc[(sink [proc (-> any/c any)]) process?]{

  Applies @var[proc] to each value taken and ignores the result.

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

  Calls @var[proc] repeatedly and emits each result.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (source random))
    (recv π)
    (recv π)
  ]
}

@defproc[(stream [snk process?] [src process?]) process?]{

  Forwards to @var[snk] and from @var[src]. Stops @var[snk] and @var[src] when
  it stops. Dies when both @var[snk] and @var[src] die.

  Commands:
  @itemlist[
    @item{@racket['sink] -- returns @var[snk]}
    @item{@racket['source] -- returns @var[src]}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π-out (server add1))
    (define π-in (sink (λ (x) (give π-out (* x 2)))))
    (call (stream π-in π-out) 3)
  ]
}

@defproc[
  (service [key-proc (-> process? any/c)]
           [#:on-drop on-drop (-> any/c process? any) void])
  process?
]{

  Associates processes to keys generated by @var[key-proc]. When given
  @racket[(list #,(var key) #,(var v))], forwards @var[v] to the
  @tech{process} associated with @var[key]. Emits @racket[(list #,(var key)
  #,(var v))] when the @tech{process} associated with @var[key] emits @var[v].
  Applies @var[on-drop] to each key--@tech{process} pair it drops. Drops each
  @tech{process} that dies. Drops every @tech{process} when it stops.

  Commands:

  @itemlist[
    @item{@racket['peers] -- returns an alist of active peers}
    @item{@racket['add] @var[π] -- adds @tech{process} @var[π] to the set of
      active peers; returns the key associated with @var[π]}
    @item{@racket['get] @var[key] -- returns the @tech{process} associated
      with @var[key], or @racket[#f] if no such @tech{process} exists}
    @item{@racket['drop] @var[key] -- drops the @tech{process} associated with
      @var[key]; returns @racket[#t] if @var[key] was in use, @racket[#f]
      otherwise.}
  ]

  @; @examples[
  @;   #:eval neuron-evaluator
  @;   #:label "Example:"
  @;   (define times
  @;     (let ([N -1])
  @;       (service
  @;        (λ _ (set! N (add1 N)) N)
  @;        #:on-drop (λ (k _) (displayln `(STOP ,k))))))
  @;   (for ([i 10])
  @;     (times `(add ,(server (curry * i)))))
  @;   (for/list ([i 10])
  @;     (call times (list i 3)))
  @;   (for ([i 5] #:when (even? i))
  @;     (times `(drop ,i)))
  @;   (for/list ([i 10])
  @;     (call times (list i 4)))
  @;   (stop times)
  @; ]
}

@defproc[(simulator [proc (-> real? any)] [#:rate rate real? 10]) process?]{

  Repeatedly calls @var[proc] at a frequency of up to @var[rate] times per
  second. Applies @var[proc] to the period corresponding to @var[rate] in
  milliseconds.

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

  Calls @var[π]s in series, implicitly starting with @racket[take] and ending
  with @racket[emit]. Stops all @var[π]s when it stops. Dies when any @var[π]
  dies.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π
      (pipe
       (server add1)
       (server (curry * 3))
       (server sub1)))
    (call π 2)
  ]
}

@defproc[(bridge [π1 process?] [π2 process?]) process?]{

  Forwards from @var[π1] to @var[π2], and vice versa. Stops @var[π1] and
  @var[π2] when it stops. Dies when @var[π1] or @var[π2] die.

  A bridge will attempt to forward unrecognized commands---first to
  @var[π1], then to @var[π2]---before raising
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

@defproc[(managed [π process?]) process?]{

  Forwards non-@racket[eof] values to and from @var[π]. Stops @var[π] when it
  stops. Dies when @var[π] dies.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (managed (server add1)))
    (call π 1)
    (shutdown π)
    (dead? π)
  ]
}

@defproc[(shutdown [π process?]) void?]{

  Gives @racket[eof] to @var[π] and blocks until it dies.

}

@defproc[(shutdown-evt [π process?]) evt?]{

  Gives @racket[eof] to @var[π] and returns a @rtech{synchronizable event}
  that becomes @rtech{ready for synchronization} when @var[π] dies. The
  @rtech{synchronization result} is @var[π].

}

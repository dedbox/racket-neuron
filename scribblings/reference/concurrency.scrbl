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

@; @margin-note{@secref{The Neuron Technical Report} gives a detailed explanation
@; of how and why exchangers differ from ordinary @rtech{channels}.}

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

@defproc[(ex-offer [ex1 exchanger?] [#:to ex2 exchanger?]) void?]{

  Blocks until @racket[ex2] is ready to accept @racket[ex1].

}

@defproc[(ex-accept [#:from ex exchanger?]) exchanger?]{

  Blocks until an exchanger is offered to @racket[ex].

}

@defproc[(ex-put [v any/c] [#:into ex exchanger?]) void?]{

  Blocks until an exchanger is ready to get @racket[v] from @racket[ex].

}

@defproc[(ex-get [#:from ex exchanger?]) any/c]{

  Blocks until an exchanger puts a value into @racket[ex].

}

@subsection{Process Exchangers}

@defproc[(giver [tx exchanger?] [rx exchanger?] [v any/c]) void?]{

  Offers @racket[tx] to @racket[rx], then puts @racket[v] into @racket[tx].

}

@defproc[(taker [rx exchanger?]) any/c]{

  Gets a value from an exchanger accepted from @racket[rx].

}

@defproc[(emitter [tx exchanger?] [v any/c]) void?]{

  Puts @racket[v] into an exchanger accepted from @racket[tx].

}

@defproc[(receiver [rx exchanger?] [tx exchanger?]) any/c]{

  Offers @racket[rx] to @racket[tx], then gets a value from @racket[rx].

}

@defproc[(forwarder [tx exchanger?] [rx exchanger?]) void?]{

  Offers an exchanger accepted from @racket[tx] to @racket[rx].

}

@defproc[
  (coupler [rx exchanger?] [tx exchanger?] [ex exchanger? (make-exchanger)])
  void?
]{

  Offers @racket[ex] to @racket[rx] and @racket[tx].

}

@defproc[(giver-evt [tx exchanger?] [rx exchanger?] [v any/c]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when @racket[(giver tx rx v)] would not block.

}

@defproc[(taker-evt [rx exchanger?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(taker rx)] would not block, and the
  @rtech{synchronization result} is the value taken through @racket[rx].

}

@defproc[(emitter-evt [tx exchanger?] [v any/c]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when @racket[(emitter tx v)] would not block.

}

@defproc[(receiver-evt [rx exchanger?] [tx exchanger?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(receiver rx tx)] would not block, and the
  @rtech{synchronization result} is the value received through @racket[rx].

}

@defproc[(forwarder-evt [tx exchanger?] [rx exchanger?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(forwarder tx rx)] would not block.

}

@defproc[
  (coupler-evt [rx exchanger?] [tx exchanger?] [ex exchanger? (make-exchanger)])
  evt?
]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[(coupler rx tx ex)] would not block.

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

@defproc[(process-tx [π process?]) transmitter?]{

  Returns the transmitting @tech{exchanger} of @racket[π].

}

@defproc[(process-rx [π process?]) transmitter?]{

  Returns the receiving @tech{exchanger} of @racket[π].

}

@defform[
  (start π-expr hooks-and-handlers ...)
  #:grammar
  [(hooks-and-handlers
    (code:line #:on-stop on-stop)
    (code:line #:on-dead on-dead)
    (code:line #:command handler))]
]{

  Installs @racket[hooks-and-handlers] into all processes created in the
  lexical scope of @racket[π-expr].

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

  Applies the @tech{command handler} of @racket[π] to @racket[v]s and returns
  the result. Does not raise @racket[unhandled-command].

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

  Blocks until @racket[π] is @rtech{ready for synchronization}. 

}

@defproc[(dead? [π process?]) boolean?]{

  Returns @racket[#t] if @racket[π] has terminated, @racket[#f] otherwise.

}

@defproc[(alive? [π process?]) boolean?]{

  Returns @racket[#t] if @racket[π] is not dead, @racket[#f] otherwise.

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

  Blocks until @racket[π] is ready to accept @racket[v] on its receiving
  @tech{exchanger}, or until @racket[π] is dead. Returns @racket[#t] if
  @racket[π] accepted @racket[v], @racket[#f] otherwise.

}

@defproc[(take) any/c]{

  Blocks until a sender is ready to provide a value on the receiving
  @tech{exchanger} of the current process. Returns the provided value.

}

@; @defproc[(try-take) any/c]{
@;   Returns the value, if any, provided on the @tech{input channel} of the
@;   current process, or @racket[#f] if no value is available.
@; }

@defproc[(emit [v any/c (void)]) void?]{

  Blocks until a receiver is ready to accept the value @racket[v] through the
  transmitting @tech{exchanger} of the current process.

}

@defproc[(recv [π process?]) any/c]{

  Blocks until @racket[π] is ready to provide a value through its transmitting
  @tech{exchanger}, or until @racket[π] is dead. Returns the provided value,
  or @racket[eof] if @racket[π] died.

}

@; @defproc[(try-recv [π process?]) any/c]{
@;   Returns the value, if any, provided on the @tech{output channel} of
@;   @racket[π], or @racket[#f] if no value is available.
@; }

@defproc[(call [π process?] [v any/c (void)]) any/c]{

  Gives @racket[v] to @racket[π] and then immediately @racket[recv]s from
  @racket[π]. Returns the received value.

}

@defproc[(forward-to [π process?]) void?]{

  Takes a value and gives it to @racket[π].

}

@defproc[(forward-from [π process?]) void?]{

  Emits a value received from @racket[π].

}

@defproc[(couple [π1 process?] [π2 process?]) void?]{

  Receives a value from @racket[π1] and gives it to @racket[π2].

}

@defproc[(give-evt [π process?] [v any/c (void)]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when @racket[π] is ready to accept the value @racket[v] on
  its receiving @tech{exchanger}, or until @racket[π] is dead. The
  @rtech{synchronization result} is @racket[#t] if @racket[π] accepted
  @racket[v], @racket[#f] otherwise.

}

@defproc[(take-evt) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a sender is ready to provide a value on the
  receiving @tech{exchanger} of the current process. The
  @rtech{synchronization result} is the provided value.

}

@defproc[(emit-evt [v any/c (void)]) evt?]{

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when a receiver is ready to accept the value @racket[v]
  through the transmitting @tech{exchanger} of the current process.

}

@defproc[(recv-evt [π process?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when @racket[π] is ready to provide a value through its
  transmitting @tech{exchanger}, or until @racket[π] is dead. The
  @rtech{synchronization result} is the provided value or @racket[eof].

}

@defproc[(forward-to-evt [π process?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a value has been taken and then given to
  @racket[π].

}

@defproc[(forward-from-evt [π process?]) evt?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a value has been received from @racket[π] and
  then emitted.

}

@defproc[(couple-evt [π1 process?] [π2 process?]) void?]{

  Returns a constant @rtech{synchronizable event} that becomes @rtech{ready
  for synchronization} when a value has been received from @racket[π1] and
  then given to @racket[π2].

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

  Returns a fresh @rtech{synchronizable event} that becomes @rtech{ready for
  synchronization} when all @racket[evt]s are @rtech{ready for
  synchronization}. The @rtech{synchronization result} is a list of the
  @rtech{synchronization results} of @racket[evt]s in the order specified.

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
  synchronization} when all events generated by @racket[make-evt]s are
  @rtech{ready for synchronization}. Calls each @racket[make-evt] in the order
  specified and immediately @racket[sync]s the result. Wtaps the last
  @racket[make-evt] in a @racket[handle-evt] that applies the
  @rtech{synchronization result} of the previous event to
  @racket[make-result]. The @rtech{synchronization result} of the sequence is
  the @rtech{synchronization result} of its final event.

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
  synchronization} when all events generated by @racket[make-evt]s have become
  @rtech{ready for synchronization}. Calls each @racket[make-evt] in the order
  specified and immediately @racket[sync]s the result. Applies
  @racket[make-evt] first to @racket[init], then to the @rtech{synchronization
  result} of the previous event. Wraps the last @racket[make-evt] in a
  @racket[handle-evt] that applies the @rtech{synchronization result} of the
  previous event to @racket[make-result]. The @rtech{synchronization result}
  of the series is the @rtech{synchronization result} of its final event.

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
  synchronization}. Repeatedly calls @racket[next-evt] and immediately
  @racket[sync]s the result. Applies @racket[next-evt] first to @racket[init],
  then to the @rtech{synchronization result} of the previous event.

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

@defproc[(server [proc (-> any/c any/c)]) process?]{

  Applies @racket[proc] to each value taken and then emits the result.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (server add1))
    (call π 1)
    (call π -1)
  ]
}

@defproc[(proxy [π process?]) process?]{

  Forwards values to and from @racket[π]. Stops @racket[π] when it stops. Dies
  when @racket[π] dies.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (call (proxy (server (curry * 3))) 2)
  ]
}

@defproc[(proxy-to [π process?]) process?]{

  Gives all values taken to @racket[π]. Stops @racket[π] when it stops. Dies
  when @racket[π] dies.

}

@defproc[(proxy-from [π process?]) process?]{

  Emits all values emitted by @racket[π]. Stops @racket[π] when it stops. Dies
  when @racket[π] dies.

}

@defproc[(sink [proc (-> any/c any)]) process?]{

  Applies @racket[proc] to each value taken and ignores the result.

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

  Calls @racket[proc] repeatedly and emits each result.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define π (source random))
    (recv π)
    (recv π)
  ]
}

@defproc[(stream [snk process?] [src process?]) process?]{

  Forwards to @racket[snk] and from @racket[src]. Stops @racket[snk] and
  @racket[src] when it stops. Dies when both @racket[snk] and @racket[src]
  die.

  Commands:
  @itemlist[
    @item{@racket['sink] -- returns @racket[snk]}
    @item{@racket['source] -- returns @racket[src]}
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

  Associates processes to keys generated by @racket[key-proc]. When given
  @racket[(list key v)], forwards @var[v] to the @tech{process} associated
  with @var[key]. Emits @racket[(list key v)] when the @tech{process}
  associated with @var[key] emits @var[v]. Applies @racket[on-drop] to each
  key--@tech{process} pair it drops. Drops each @tech{process} that dies.
  Drops every @tech{process} when it stops.

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
    (define plus
      (service
       (λ (π) (π))
       #:on-drop (λ (k _) (displayln `(STOP ,k)))))
    ((plus 'add) (start (server (curry + 3)) #:command (λ _ 3)))
    ((plus 'add) (start (server (curry + 2)) #:command (λ _ 2)))
    ((plus 'add) (start (server (curry + 1)) #:command (λ _ 1)))
    (call plus (list 1 5))
    (call plus (list 3 5))
    ((plus 'drop) 2)
    (stop plus)
  ]
}

@defproc[(simulator [proc (-> real? any)] [#:rate rate real? 10]) process?]{

  Repeatedly calls @racket[proc] at a frequency of up to @racket[rate] times
  per second. Applies @racket[proc] to the period corresponding to
  @racket[rate] in milliseconds.

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

  Calls @racket[π]s in series, implicitly starting with @racket[take] and
  ending with @racket[emit]. Stops all @racket[π]s when it stops. Dies when
  any @racket[π] dies.

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

  Forwards from @racket[π1] to @racket[π2], and vice versa. Stops @racket[π1]
  and @racket[π2] when it stops. Dies when @racket[π1] or @racket[π2] die.

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

@defproc[(managed [π process?]) process?]{

  Forwards non-@racket[eof] values to and from @racket[π]. Stops @racket[π]
  when it stops. Dies when @racket[π] dies.

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

  Gives @racket[eof] to @racket[π] and blocks until it dies.

}

@defproc[(shutdown-evt [π process?]) evt?]{

  Gives @racket[eof] to @racket[π] and returns a @rtech{synchronizable event}
  that becomes @rtech{ready for synchronization} when @racket[π] dies. The
  @rtech{synchronization result} is @racket[π].

}

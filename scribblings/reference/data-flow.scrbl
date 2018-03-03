#lang scribble/doc

@(require "../base.rkt")

@title{Data Flow}

@(defmodule neuron/data-flow #:packages ("neuron"))

@section{Serial Communication}

A @deftech{socket} is the local end of a bi-directional serial communications
channel. Each socket holds an @racket-tech{input port} and an
@racket-tech{output port}.

A socket can be used as a @racket-tech{synchronizable event}. A socket is
@racket-tech{ready for synchronization} when the ports it holds have closed.
Sockets do not support half-open connections---when either port closes, the
other port is closed immediately.

@defproc[(socket? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @racket[socket], @racket[#f]
  otherwise.
}

@defproc[(socket [in-port input-port?] [out-port output-port?]) socket?]{
  Returns a @tech{socket}. Serves as @racket[out-port] when used as an
  @racket-tech{output port}, and as @racket[in-port] when used as an
  @racket-tech{input port} or with procedures that take both kinds, such as
  @racket[file-position].
}

@defproc[(close-socket [sock socket?]) void?]{
  Closes the ports held by @racket[sock]. If the ports are already closed,
  @racket[close-socket] has no effect.
}

@defproc[(socket-closed? [sock socket?]) boolean?]{
  Returns @racket{#t} if the ports held by @racket[sock] are closed,
  @racket[#f] otherwise.
}

@defproc[(null-socket) socket?]{
  Returns a @tech{socket} that ignores all input and always outputs
  @racket[eof].
}

@deftogether[(
  @defproc[(byte-socket [#:in str bytes? #""]
                        [#:out out? boolean? #f]) socket?]
  @defproc[(string-socket [#:in str string? ""]
                          [#:out out? boolean? #f]) socket?]
)]{
  Returns a @tech{socket} that inputs from @racket[str] and, if @racket[out?]
  is @racket[#t], accumulates output in a fresh output @racket-tech{string
  port}.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define sock (string-socket #:in "123" #:out #t))
    (read sock)
    (write 'abc sock)
    (get-output-string sock)
  ]
}

@defproc[(file-socket
          [#:in in-path (or/c path-string? #f) #f]
          [#:in-mode in-mode-flag (or/c 'binary 'text) 'binary]
          [#:out out-path (or/c path-string? #f) #f]
          [#:out-mode out-mode-flag (or/c 'binary 'text) 'binary]
          [#:exists exists-flag
                    (or/c 'error 'append 'update 'can-update
                          'replace 'truncate
                          'must-truncate 'truncate/replace)]) socket?]{
  Returns a @tech{socket} that opens the files specified by @racket[in-path]
  and @racket[out-path] for input and output, respectively. If
  @racket[in-path] is @racket[#f], then all input is ignored. If
  @racket[out-path] is @racket[#f], then only @racket[eof] is output. If both
  are @racket[#f], then the @racket[file-socket] call is equivalent to
  @racket[(null-socket)].

  See @racket[open-input-file] for details on @racket[in-mode-flag], and
  @racket[open-output-file] for details on @racket[out-mode-flag] and
  @racket[exists-flag].
}

@section{Serialization}

@subsection{Codecs}

A @deftech{codec} is a @tech{stream} that uses a @tech{socket} to exchange
serializable values with remote agents. The @tech{sink} is called an
@deftech{encoder}; it uses a @deftech{printer} procedure to serialize values
to a socket. The @tech{source} is called a @deftech{decoder}; it uses a
@deftech{parser} procedure to de-serialize values from a socket.

@defthing[parser/c contract? #:value (-> socket? any/c)]{
  Use this @racket-tech{function contract} to indicate that a function is a
  @tech{parser}.
}

@defthing[printer/c contract? #:value (-> any/c socket? any)]{
  Use this @racket-tech{function contract} to indicate that a function is a
  @tech{printer}.
}

@defthing[codec/c contract? #:value (-> socket? process?)]{
  Use this @racket-tech{function contract} to indicate that a function makes
  @tech{decoders}, @tech{encoders}, or @tech{codecs}.
}

@defproc[(flushed [prn printer/c]) printer/c]{
  Returns a @tech{printer} that applies @racket[prn] to a @tech{socket} and
  then flushes its @racket-tech{output port}.
}

@defproc[(decoder [prs parser/c]) codec/c]{
  Returns a procedure that makes @tech{decoders} based on @racket[prs]:

  Parses and emits values from a @tech{socket}. Stops when @racket[prs]
  returns @racket[eof]. Closes the @tech{socket} when it stops. Dies when the
  @tech{socket} closes.

  Commands:

  @itemlist[
    @item{@racket['parser] -- returns @racket[prs]}
    @item{@racket['socket] -- returns a @tech{socket}}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define dec ((decoder read) (string-socket #:in "123 abc")))
    (recv dec)
    (recv dec)
    (recv dec)
  ]
}

@defproc[(encoder [prn printer/c]) codec/c]{
  Returns a procedure that makes @tech{encoders} based on @racket[prn]:

  Takes and prints values to a @tech{socket}. Stops when it takes
  @racket[eof]. Closes the @tech{socket} when it stops. Dies when the
  @tech{socket} closes.

  Commands:

  @itemlist[
    @item{@racket['printer] -- returns @racket[prn]}
    @item{@racket['socket] -- returns a @tech{socket}}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define enc ((encoder writeln) (string-socket #:out #t)))
    (for-each (curry give enc) (list 123 'abc eof))
    (get-output-string (enc 'socket))
  ]
}

@defproc[(codec [prs parser/c] [prn printer/c]) codec/c]{
  Returns a procedure that makes @tech{codecs} based on @racket[prs] and
  @racket[prn]:

  Takes and prints values to a @tech{socket}. Reads and emits values from a
  @tech{socket}. Stops when given @racket[eof] or @racket[prs] returns
  @racket[eof]. Closes the @tech{socket} when it stops. Dies when the
  @tech{socket} closes.

  Commands:

  @itemlist[
    @item{@racket['decoder] -- returns a @tech{decoder} based on @racket[prs]}
    @item{@racket['encoder] -- returns an @tech{encoder} based on @racket[prn]}
    @item{@racket['socket] -- returns a @tech{socket}}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define cdc ((codec read writeln)
                 (string-socket #:in "123 abc" #:out #t)))
    (for-each (curry give cdc) (list 987 'zyx eof))
    (recv cdc)
    (recv cdc)
    (recv cdc)
    (get-output-string (cdc 'socket))
  ]
}

@subsection{Codec Types}

A @deftech{codec type} is a set of uniformly-named procedures for making
codecs and codec parts. A complete codec type named @var[name] is defined by
the following procedures:

@itemlist[
  @item{@var[name]@racket[-parser] : @racket[parser/c]}
  @item{@var[name]@racket[-printer] : @racket[printer/c]}
  @item{@var[name]@racket[-decoder] : @racket[codec/c]}
  @item{@var[name]@racket[-encoder] : @racket[codec/c]}
  @item{@var[name]@racket[-codec] : @racket[codec/c]}
]

@defproc[(make-codec-type [name symbol?]
                          [prs parser/c]
                          [prn printer/c])
           (values codec/c
                   codec/c
                   codec/c)]{
  Creates a new @tech{codec type}. The @racket[name] argument is used as the
  type name.

  The result of @racket[make-codec-type] is three values:

  @itemlist[
    @item{a procedure that makes @tech{decoders} based on @racket[prs],}
    @item{a procedure that makes @tech{encoders} based on @racket[prn],}
    @item{a procedure that makes @tech{codecs} based on @racket[prs] and
      @racket[prn].}
  ]
}

@defform[(define-codec name prs prn)]{
  Creates a new @tech{codec type} and binds variables related to the
  @tech{codec type}.

  A @racket[define-codec] form defines 5 names:

  @itemlist[
    @item{@racket[name]@racketidfont{-parser}, an alias for @tech{parser}
      @racket[prs].}
    @item{@racket[name]@racketidfont{-printer}, an alias for @tech{printer}
      @racket[prn].}
    @item{@racket[name]@racketidfont{-decoder}, a procedure that makes
      @tech{decoders} based on @racket[prs].}
    @item{@racket[name]@racketidfont{-encoder}, a procedure that makes
      @tech{encoders} based on @racket[prn].}
    @item{@racket[name]@racketidfont{-codec}, a procedure that makes
      @tech{codecs} based on @racket[prs] and @racket[prn].}
  ]
}

@deftogether[(
  @defthing[#:kind "procedure" line-parser parser/c]
  @defthing[#:kind "procedure" line-printer printer/c]
  @defthing[#:kind "procedure" line-decoder codec/c]
  @defthing[#:kind "procedure" line-encoder codec/c]
  @defthing[#:kind "procedure" line-codec codec/c]
)]{
  Line @tech{codec type}.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define cdc
      (line-codec
        (string-socket #:in "123 abc\n" #:out #t)))
    (give cdc "987 zyx")
    (recv cdc)
    (get-output-string (cdc 'socket))
  ]
}

@deftogether[(
  @defthing[#:kind "procedure" sexp-parser parser/c]
  @defthing[#:kind "procedure" sexp-printer printer/c]
  @defthing[#:kind "procedure" sexp-decoder codec/c]
  @defthing[#:kind "procedure" sexp-encoder codec/c]
  @defthing[#:kind "procedure" sexp-codec codec/c]
)]{
  S-expression @tech{codec type}.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define cdc
      (sexp-codec
        (string-socket #:in "(#hasheq((ab . 12)) 34)" #:out #t)))
    (give cdc (list 987 'zyx))
    (recv cdc)
    (get-output-string (cdc 'socket))
  ]
}

@deftogether[(
  @defthing[#:kind "procedure" json-parser parser/c]
  @defthing[#:kind "procedure" json-printer printer/c]
  @defthing[#:kind "procedure" json-decoder codec/c]
  @defthing[#:kind "procedure" json-encoder codec/c]
  @defthing[#:kind "procedure" json-codec codec/c]
)]{
  @other-doc['(lib "json/json.scrbl")] @tech{codec type}.

  To change how null is represented, set the @racket[json-null] parameter in
  the @other-doc['(lib "json/json.scrbl")] library.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (require json)
    (define cdc
      (parameterize ([json-null 'NULL])
        (json-codec
          (string-socket #:in "[{\"ab\":12},34,null]" #:out #t))))
    (give cdc '(98 #hasheq([zy . 76]) NULL))
    (recv cdc)
    (get-output-string (cdc 'socket))
  ]
}

@section{Networking}

A @deftech{TCP socket} is a @tech{socket} with a TCP address.

@defproc[(tcp-socket? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{TCP socket}, @racket[#f]
  otherwise.
}

@defproc[(tcp-socket [in-port input-port?]
                     [out-port output-port?]) tcp-socket?]{
  Returns a @tech{TCP socket}.
}

@defproc[(tcp-socket-address [sock tcp-socket?]) (list/c string? port-number?
                                                         string? port-number?)]{
  Returns the address of @tech{TCP socket} @racket[sock].
}

@defproc[(tcp-client [hostname string?]
                     [port-no port-number?]
                     [local-hostname (or/c string? #f) #f]
                     [local-port-no (or/c port-number? #f) #f]) socket?]{
  Establishes a TCP connection to @var[hostname]:@var[port-no]. Returns a
  @tech{TCP socket}.

  See @racket[tcp-connect] for argument details.
}

@defproc[(tcp-server [port-no listen-port-number?]
                     [max-allow-wait exact-nonnegative-integer? 4]
                     [reuse? any/c #f]
                     [hostname (or/c string? #f) #f]) process?]{
  Creates a ``listening'' TCP server on @var[hostname]:@var[port-no]. Returns
  a @racket[source] that emits a @tech{TCP socket} for each connection
  accepted.

  See @racket[tcp-listen] for argument details.

  Commands:

  @itemlist[
    @item{@racket['address] -- returns @racket[(list hostname port-no)]}
  ]
}

@defproc[(tcp-service [make-codec codec/c]
                      [srv process?]
                      [#:on-accept on-accept (-> any/c process? any) void]
                      [#:on-drop on-drop (-> any/c process? any) void])
         process?]{
  Returns a @racket[service] keyed by @tech{TCP socket} address. Applies
  @racket[make-codec] to each @tech{TCP socket} emitted by @racket[srv] and
  adds the resulting @tech{codec} to the service. When given @racket[(list
  addr v)], forwards @var[v] to @var[addr]. Emits @racket[(list addr v)] when
  @var[addr] emits @var[v]. Applies @racket[on-accept] to the @tech{codecs}
  made by @racket[make-codec] and their addresses. Applies @racket[on-drop] to
  each address--@tech{codec} pair it drops. Drops each @tech{codec} that dies.
  Drops every @tech{codec} when it stops.

  Commands:

  @itemlist[
    @item{@racket['peers] -- returns a list of addresses}
    @item{@racket['get] @var[addr] -- returns the @tech{codec} associated with
      @var[addr], or @racket[#f] is no such @tech{codec} exists}
    @item{@racket['drop] @var[addr] -- disconnects the @tech{TCP socket}
      associated with @var[addr]; returns @racket[#t] if @var[addr] was in
      use, @racket[#f] otherwise}
  ]
}

@; @defproc[(udp-datagram-source [sock udp?]) process?]{
@;   Emits each datagram received from @racket[sock] as a byte string.
@; }

@; @defproc[(udp-datagram-sink [sock udp?]) process?]{
@;   Writes each given byte string to @racket[sock] as a datagram. Bytes strings
@;   of length exceeding 65,527 bytes, the maximum size of a UDP datagram
@;   payload, are truncated silently.
@; }

@; @defproc[(udp-datagram-stream [sock udp?]) process?]{
@;   Returns a @racket[stream] process that combines a
@;   @racket[udp-datagram-source] and @racket[udp-datagram-sink].
@; }

@; @defproc[(udp-source [prs parser/c]) process?]{
@;   Listens for incoming UDP datagrams. Returns a @racket[source] process that
@;   applies @racket[prs] to each UDP datagram received and emits the result.
@; }

@; @defproc[(udp-sink [prn printer/c]) process?]{
@;   Applies @racket[prn] to each value received and transmits the result as a
@;   UDP datagram.
@; }

@; @defproc[(udp-decoder [make-dec decoder/c]) process?]{
  
@; }

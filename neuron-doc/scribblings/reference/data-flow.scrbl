#lang scribble/doc

@(require "../base.rkt")

@title{Data Flow}

@section{Socket}

@(defmodule neuron/socket)

A @deftech{socket} is the local end of a bi-directional serial communications
channel. Each socket holds an @rtech{input port} and an @rtech{output port}.
Half-open connections are not supported---when either port closes, the other
port is closed immediately.

A socket can be used as a @rtech{synchronizable event}. A socket is
@rtech{ready for synchronization} when the ports it holds have closed.

@defproc[(socket? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @racket[socket], @racket[#f] otherwise.

}

@defproc[(socket [in-port input-port?] [out-port output-port?]) socket?]{

  Returns a @tech{socket}. Serves as @var[out-port] when used as an
  @rtech{output port}, and as @var[in-port] when used as an @rtech{input port}
  or with procedures that take both kinds, such as @racket[file-position].

}

@defproc[(close-socket [sock socket?]) void?]{

  Closes the ports held by @var[sock]. If the ports are already closed,
  @racket[close-socket] has no effect.

}

@defproc[(socket-closed? [sock socket?]) boolean?]{

  Returns @racket{#t} if the ports held by @var[sock] are closed, @racket[#f]
  otherwise.

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

  Returns a @tech{socket} that inputs from @var[str] and, if @var[out?] is
  @racket[#t], accumulates output in a fresh output @rtech{string port}.

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define sock (string-socket #:in "123" #:out #t))
    (read sock)
    (write 'abc sock)
    (get-output-string sock)
  ]
}

@defproc[
  (file-socket
   [#:in in-path (or/c path-string? #f) #f]
   [#:in-mode in-mode-flag (or/c 'binary 'text) 'binary]
   [#:out out-path (or/c path-string? #f) #f]
   [#:out-mode out-mode-flag (or/c 'binary 'text) 'binary]
   [#:exists exists-flag
             (or/c 'error 'append 'update 'can-update
                   'replace 'truncate
                   'must-truncate 'truncate/replace)])
  socket?
]{

  Returns a @tech{socket} that opens the files specified by @var[in-path] and
  @var[out-path] for input and output, respectively. If @var[in-path] is
  @racket[#f], then all input is ignored. If @var[out-path] is @racket[#f],
  then only @racket[eof] is output. If both are @racket[#f], then the
  @racket[file-socket] call is equivalent to @racket[(null-socket)].

  See @racket[open-input-file] for details on @racket[in-mode-flag], and
  @racket[open-output-file] for @racket[out-mode-flag] and
  @racket[exists-flag].

}

@section{Codec}

@(defmodule neuron/codec)

A @deftech{codec} is a @racket[stream] that uses a @tech{socket} to exchange
serializable values with remote agents. The @racket[sink] is called an
@deftech{encoder}; it uses a @deftech{printer} procedure to serialize values
to a socket. The @racket[source] is called a @deftech{decoder}; it uses a
@deftech{parser} procedure to de-serialize values from a socket.

@defthing[parser/c contract? #:value (-> socket? any/c)]{

  Use this @rtech{function contract} to indicate that a function is a
  @tech{parser}.

}

@defthing[printer/c contract? #:value (-> any/c socket? any)]{

  Use this @rtech{function contract} to indicate that a function is a
  @tech{printer}.

}

@defthing[codec/c contract? #:value (-> socket? process?)]{

  Use this @rtech{function contract} to indicate that a function makes
  @tech{decoders}, @tech{encoders}, or @tech{codecs}.

}

@defproc[(flushed [prn printer/c]) printer/c]{

  Returns a @tech{printer} that applies @var[prn] to a @tech{socket} and then
  flushes its @rtech{output port}.

}

@defproc[(decoder [prs parser/c]) codec/c]{

  Returns a procedure that makes @tech{decoders} based on @var[prs]:

  Parses and emits values from a @tech{socket}. Stops when @var[prs] returns
  @racket[eof]. Closes the @tech{socket} when it stops. Dies when the
  @tech{socket} closes.

  Commands:

  @itemlist[
    @item{@racket['parser] -- returns @var[prs]}
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

  Returns a procedure that makes @tech{encoders} based on @var[prn]:

  Takes and prints values to a @tech{socket}. Stops when it takes
  @racket[eof]. Closes the @tech{socket} when it stops. Dies when the
  @tech{socket} closes.

  Commands:

  @itemlist[
    @item{@racket['printer] -- returns @var[prn]}
    @item{@racket['socket] -- returns a @tech{socket}}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define enc ((encoder writeln) (string-socket #:out #t)))
    (for-each (curry give enc) (list 123 'abc eof))
    (displayln (get-output-string (enc 'socket)))
  ]
}

@defproc[(codec [prs parser/c] [prn printer/c]) codec/c]{

  Returns a procedure that makes @tech{codecs} based on @var[prs] and
  @var[prn]:

  Takes and prints values to a @tech{socket}. Reads and emits values from a
  @tech{socket}. Stops when given @racket[eof] or @var[prs] returns
  @racket[eof]. Closes the @tech{socket} when it stops. Dies when the
  @tech{socket} closes.

  Commands:

  @itemlist[
    @item{@racket['decoder] -- returns a @tech{decoder} based on @var[prs]}
    @item{@racket['encoder] -- returns an @tech{encoder} based on @var[prn]}
    @item{@racket['socket] -- returns a @tech{socket}}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define cdc
      ((codec read writeln)
       (string-socket #:in "123 abc" #:out #t)))
    (for-each (curry give cdc) (list 987 'zyx eof))
    (recv cdc)
    (recv cdc)
    (recv cdc)
    (displayln (get-output-string (cdc 'socket)))
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

@defproc[
  (make-codec-type [name symbol?] [prs parser/c] [prn printer/c])
  (values codec/c codec/c codec/c)
]{

  Creates a new @tech{codec type}. The @var[name] argument is used as the type
  name.

  The result of @racket[make-codec-type] is three values:

  @itemlist[
    @item{a procedure that makes @tech{decoders} based on @var[prs],}
    @item{a procedure that makes @tech{encoders} based on @var[prn],}
    @item{a procedure that makes @tech{codecs} based on @var[prs] and
      @var[prn].}
  ]
}

@defform[(define-codec name prs prn)]{

  Creates a new @tech{codec type} and binds variables related to the
  @tech{codec type}.

  A @racket[define-codec] form defines 5 names:

  @itemlist[
    @item{@racket[name]@racketidfont{-parser}, an alias for @tech{parser}
      @var[prs].}
    @item{@racket[name]@racketidfont{-printer}, an alias for @tech{printer}
      @var[prn].}
    @item{@racket[name]@racketidfont{-decoder}, a procedure that makes
      @tech{decoders} based on @var[prs].}
    @item{@racket[name]@racketidfont{-encoder}, a procedure that makes
      @tech{encoders} based on @var[prn].}
    @item{@racket[name]@racketidfont{-codec}, a procedure that makes
      @tech{codecs} based on @var[prs] and @var[prn].}
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
      (line-codec (string-socket #:in "1 2 3\na b c\n" #:out #t)))
    (give cdc "987")
    (give cdc "zyx")
    (recv cdc)
    (recv cdc)
    (displayln (get-output-string (cdc 'socket)))
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
        (string-socket #:in "#hasheq((ab . 12)) 34" #:out #t)))
    (give cdc 987)
    (give cdc 'zyx)
    (hash-ref (recv cdc) 'ab)
    (recv cdc)
    (displayln (get-output-string (cdc 'socket)))
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
    (pretty-print (recv cdc))
    (displayln (get-output-string (cdc 'socket)))
  ]
}

@section{Network}

@subsection{TCP}

@(defmodule neuron/network/tcp)

A @deftech{TCP socket} is a @tech{socket} with a TCP address.

@defproc[(tcp-socket? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @tech{TCP socket}, @racket[#f]
  otherwise.

}

@defproc[
  (tcp-socket [in-port input-port?] [out-port output-port?])
  tcp-socket?
]{

  Returns a @tech{TCP socket} on @var[in-port] and @var[out-port].

}

@defproc[
  (tcp-socket-address [sock tcp-socket?])
  (list/c string? port-number? string? port-number?)
]{

  Returns the address of @racket[sock].

}

@defproc[
  (tcp-client
   [hostname string?]
   [port-no port-number?]
   [local-hostname (or/c string? #f) #f]
   [local-port-no (or/c port-number? #f) #f])
  socket?
]{

  Establishes a TCP connection to @var[hostname]:@var[port-no]. Returns a
  @tech{TCP socket}.

  See @racket[tcp-connect] for argument details.
}

@defproc[
  (tcp-server
   [port-no listen-port-number?]
   [max-allow-wait exact-nonnegative-integer? 4]
   [reuse? any/c #f]
   [hostname (or/c string? #f) #f])
  process?
]{

  Creates a ``listening'' TCP server on @var[hostname]:@var[port-no]. Returns
  a @racket[source] that emits a @tech{TCP socket} for each connection
  accepted.

  See @racket[tcp-listen] for argument details.

  Commands:

  @itemlist[
    @item{@racket['address] -- returns @racket[(list #,(var hostname) #,(var port-no))]}
  ]
}

@defproc[
  (tcp-service
   [make-codec codec/c]
   [port-no listen-port-number?]
   [max-allow-wait exact-nonnegative-integer? 4]
   [reuse? any/c #f]
   [hostname (or/c string? #f) #f]
   [#:on-accept on-accept (-> any/c process? any) void]
   [#:on-drop on-drop (-> any/c process? any) void])
  process?
]{

  Wraps a @racket[service] keyed by @tech{TCP socket} address. Applies
  @var[make-codec] to each @tech{TCP socket} emitted by @racket[(server #,(var
  port-no) #,(var max-allow-wait) #,(var reuse?) #,(var hostname))] and adds
  the resulting @tech{codec} to the service. When given @racket[(list #,(var
  addr) #,(var v))], forwards @var[v] to @var[addr]. Emits @racket[(list
  #,(var addr) #,(var v))] when @var[addr] emits @var[v]. Applies
  @racket[on-accept] to the @tech{codecs} made by @racket[make-codec] and
  their addresses. Applies @racket[on-drop] to each address--@tech{codec} pair
  it drops. Drops each @tech{codec} that dies. Drops every @tech{codec} when
  it stops.

  Commands:

  @itemlist[
    @item{@racket['peers] -- returns an alist of addressed peers}
    @item{@racket['get] @var[addr] -- returns the @tech{codec} associated with
      @var[addr], or @racket[#f] if no such @tech{codec} exists}
    @item{@racket['drop] @var[addr] -- disconnects the @tech{TCP socket}
      associated with @var[addr]; returns @racket[#t] if @var[addr] was in
      use, @racket[#f] otherwise}
  ]

  @examples[
    #:eval neuron-evaluator
    #:label "Example:"
    (define svc (tcp-service sexp-codec 0))
    (svc 'address)
    (define svc-port (cadr (svc 'address)))
    (define cli (sexp-codec (tcp-client "localhost" svc-port)))
    (give cli 'x)
    (define-values (peer-addr msg) (recv svc))
    (values peer-addr msg)
    (give svc peer-addr 'y)
    (recv cli)
  ]
}

@subsection{UDP}

@(defmodule neuron/network/udp)

@defproc[
  (udp-source
    [prs parser/c]
    [hostname string? "::"]
    [port-no port-number? 0])
  process?
]{

  Listens for incoming UDP datagrams on @var[hostname]:@var[port-no]. Applies
  @var[prs] repeatedly to each UDP datagram received and emits the result.

}

@defproc[
  (udp-sink [prn printer/c] [hostname string?] [port-no listen-port-number?])
  process?
]{

  Applies @var[prn] to each value taken and transmits the resulting byte
  string to @var[hostname]:@var[port-no] as a UDP datagram. Bytes strings of
  length exceeding 65,527 bytes---the maximum size of a UDP datagram
  payload---are truncated silently.

  Commands:

  @itemlist[
    @item{@racket['address] -- returns a full UDP/IP address.}
  ]

  @; @examples[
  @;   #:eval neuron-evaluator
  @;   #:label "Example:"
  @;   (define src (udp-source read))
  @;   (src 'address)
  @;   (define src-port (cadr (src 'address)))
  @;   (define snk (udp-sink writeln "localhost" src-port))
  @;   (snk 'address)
  @;   (give snk 123)
  @;   (recv src)
  @;   (recv src)
  @; ]
}

@defproc[
  (udp-sink-to
   [prn printer/c]
   [local-hostname string? "::"]
   [local-port-no port-number? 0])
  process?
]{

  Takes a @racketid[hostname], a @racketid[port-no], and one or more values.
  Applies @var[prn] to each value taken and transmits the resulting byte
  string from @var[local-hostname]:@var[local-port-no] to
  @racketid[hostname]:@racketid[port-no] as a UDP datagram. Byte strings of
  length exceeding 65,527 bytes---the maximum size of a UDP datagram
  payload---are truncated silently.

  Commands:

  @itemlist[
    @item{@racket['address] -- returns a full UDP/IP address.}
  ]

}

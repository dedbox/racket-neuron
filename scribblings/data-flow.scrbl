#lang scribble/doc

@(require "base.rkt")

@title{Data Flow}

@(defmodule neuron/data-flow #:packages ("neuron"))

@section{Input and Output}

@defproc[(port-sink [out-port output-port?]) process?]{
  Returns a @tech{sink} that writes byte strings to @racket[out-port]. Stops
  when given @racket[eof]. Closes @racket[out-port] when it stops. Dies when
  @racket[out-port] closes.

  Commands:

  @itemlist[
    @item{@racket['output-port] -- returns @racket[out-port]}
  ]

  @examples[
    #:eval neuron-evaluator
    (define snk (port-sink (open-output-string)))
    (for-each (curry give snk) (list #"123" #"ab" eof))
    (get-output-string (snk 'output-port))
  ]
}

@defproc[(port-source [amt exact-nonnegative-integer?]
                      [in-port input-port?]
                      ) process?]{
  Returns a @tech{source} that reads byte strings from @racket[in-port] of
  length up to @racket[amt] bytes. Stops when @racket[in-port] reaches
  @racket[eof]. Dies when @racket[in-port] closes.

  Commands:

  @itemlist[
    @item{@racket['input-port] -- returns @racket[in-port]}
  ]

  @examples[
    #:eval neuron-evaluator
    (define src (port-source 3 (open-input-bytes #"123ab")))
    (recv src)
    (recv src)
    (recv src)
  ]
}

@defproc[(port-socket [amt exact-nonnegative-integer?]
                      [in-port input-port?]
                      [out-port output-port?]
                      ) process?]{
  Returns a @tech{socket} with source @racket[(port-source amt in-port)] and
  sink @racket[(port-sink out-port)].
}

@defproc[(byte-sink) process?]{
  Returns a @tech{sink} that accumulates byte strings. Emits accumulated bytes
  when it is given @racket[eof] and when it stops.

  @examples[
    #:eval neuron-evaluator
    (define snk (byte-sink))
    (for-each (curry give snk) (list #"abc" #"123" eof))
    (recv snk)
  ]
}

@defproc[(string-sink) process?]{
  Returns a @tech{sink} that accumulates strings. Emits accumulated string
  when it is given @racket[eof] and when it stops.

  @examples[
    #:eval neuron-evaluator
    (define snk (string-sink))
    (for-each (curry give snk) (list "λx" ".x" eof))
    (recv snk)
  ]
}

@defproc[
  (file-sink [path path-string?]
             [#:mode mode-flag (or/c 'binary 'text) 'binary]
             [#:exists exists-flag
               (or/c 'error 'append 'update 'can-update
                     'replace 'truncate
                     'must-truncate 'truncate/replace) 'error]
             ) process?]{
  Returns a @racket[port-sink] for writing to the file specified by
  @racket[path]. The @racket[mode-flag] and @racket[exists-flag] are the same
  as for @racket[open-output-file].
}

@defproc[
  (file-source [path path-string?]
               [amt exact-nonnegative-integer?]
               [#:mode mode-flag (or/c 'binary 'text) 'binary]) process?]{
  Returns a @racket[port-source] for reading from the file specified by
  @racket[path]. The @racket[mode-flag] is the same as for
  @racket[open-input-file].
}

@defproc[(directory-source [path path-string? (current-directory)]) process?]{
  Returns a @racket[source] for the names of all files and directories in the
  directory specified by @racket[path]. Emits names in lexicgraphic order and
  then dies.

  @examples[
    #:eval neuron-evaluator
    (define π (directory-source))
    (while (alive? π) (displayln (recv π)))
  ]
}

@section{Decoding and Encoding}

A @deftech{parser} is ...

A @deftech{printer} is ...

A @deftech{codec type} is ...

@defthing[parser/c contract?]{
  Use this contract to indicate that some function is a @tech{parser}.
}

@defthing[printer/c contract?]{
  Use this contract to indicate that some function is a @tech{printer}.
}

@defproc[(flushed [prn printer/c]) printer/c]{
  Returns a @tech{printer} that applies @racket[prn] and then flushes the
  @racket-tech{output port}.
}

@defproc[(decoder [prs parser/c]
                  [in-port input-port?]
                  ) process?]{
  Returns a @deftech{decoder} @tech{source} process. Calls @racket[(emit (prs
  in-port))]. Stops when @racket[prs] returns @racket[eof]. Closes
  @racket[in-port] when it stops. Dies when @racket[in-port] closes.

  Commands:

  @itemlist[
    @item{@racket['parser] -- returns @racket[prs]}
    @item{@racket['input-port] -- returns @racket[in-port]}
  ]

  @examples[
    #:eval neuron-evaluator
    (define dec (decoder read (open-input-string "123 abc")))
    (recv dec)
    (recv dec)
    (recv dec)
  ]
}

@defproc[(encoder [prn printer/c]
                  [out-port output-port?]
                  ) process?]{
  Returns an @deftech{encoder} @tech{sink} process. Calls @racket[(prn (take)
  out-port)]. Stops when given @racket[eof]. Closes @racket[out-port] when it
  stops. Dies when @racket[out-port] closes.

  Commands

  @itemlist[
    @item{@racket['printer] -- returns @racket[prn]}
    @item{@racket['output-port] -- returns @racket[out-port]}
  ]

  @examples[
    #:eval neuron-evaluator
    (define enc (encoder writeln (open-output-string)))
    (for-each (curry give enc) (list 123 'abc eof))
    (get-output-string (enc 'output-port))
  ]
}

@defproc[(codec [prs parser/c]
                [prn printer/c]
                [in-port input-port?]
                [out-port output-port?]
                ) process?]{
  Returns a @deftech{codec} @tech{socket} process with source @racket[(encoder
  prn out-port)] and sink @racket[(decoder prs in-port)].

  Commands:

  @itemlist[
    @item{@racket['decoder] -- returns the @tech{decoder} built from
      @racket[prs] and @racket[in-port].}
    @item{@racket['encoder] -- returns the @tech{encoder} built from
      @racket[prn] and @racket[out-port].}
  ]

  @examples[
    #:eval neuron-evaluator
    (define cdc
      (codec read writeln
             (open-input-string "123 abc")
             (open-output-string)))
    (for-each (curry give cdc) (list 987 'zyx eof))
    (recv cdc)
    (recv cdc)
    (recv cdc)
    (get-output-string ((cdc 'encoder) 'output-port))
  ]
}

@defproc[(make-codec-type [name symbol?]
                          [prs parser/c]
                          [prn printer/c]
                          ) (values (-> input-port? process?)
                                    (-> output-port? process?)
                                    (-> input-port? output-port? process?))]{
  Creates a new @tech{codec type}. The @racket[name] argument is used as the
  type name.

  The result of @racket[make-codec-type] is three values:

  @itemlist[
    @item{a @tech{decoder} constructor for @tech{parser} @racket[prs],}
    @item{an @tech{encoder} constructor for @tech{printer} @racket[prn],}
    @item{a @tech{codec} constructor for @tech{parser} @racket[prs] and
      @tech{printer} @racket[prn].}
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
    @item{@racket[name]@racketidfont{-decoder}, a @tech{decoder} constructor
      for @tech{parser} @racket[prs].}
    @item{@racket[name]@racketidfont{-encoder}, an @tech{encoder} constructor
      for @tech{printer} @racket[prn].}
    @item{@racket[name]@racketidfont{-codec}, a @tech{codec} constructor for
      @tech{parser} @racket[prs] and @tech{printer} @racket[prn].}
  ]
}

@subsection{Codecs}

@deftogether[(@defproc[(line-parser [in-port input-port?]) any/c]
              @defproc[(line-printer [out-port output-port?]) any/c]
              @defproc[(line-decoder [in-port input-port?]) process?]
              @defproc[(line-encoder [out-port output-port?]) process?]
              @defproc[(line-codec [in-port input-port?]
                                   [out-port output-port?]) process?])]{
  Line @tech{codec type}.

  @examples[
    #:eval neuron-evaluator
    (define cdc (line-codec (open-input-string "123 abc\n")
                            (open-output-string)))
    (recv cdc)
    (give cdc "987 zyx")
    (get-output-string ((cdc 'encoder) 'output-port))
  ]
}

@deftogether[(@defproc[(sexp-parser [in-port input-port?]) any/c]
              @defproc[(sexp-printer [out-port output-port?]) any/c]
              @defproc[(sexp-decoder [in-port input-port?]) process?]
              @defproc[(sexp-encoder [out-port output-port?]) process?]
              @defproc[(sexp-codec [in-port input-port?]
                                   [out-port output-port?]) process?])]{
  S-expression @tech{codec type}.

  @examples[
    #:eval neuron-evaluator
    (define cdc (sexp-codec (open-input-string "(#hasheq((ab . 12)) 34)")
                            (open-output-string)))
    (recv cdc)
    (give cdc (list 987 'zyx))
    (get-output-string ((cdc 'encoder) 'output-port))
  ]
}

@deftogether[(@defproc[(json-parser [in-port input-port?]) any/c]
              @defproc[(json-printer [out-port output-port?]) any/c]
              @defproc[(json-decoder [in-port input-port?]) process?]
              @defproc[(json-encoder [out-port output-port?]) process?]
              @defproc[(json-codec [in-port input-port?]
                                   [out-port output-port?]) process?])]{
  @other-doc['(lib "json/json.scrbl")] @tech{codec type}.

  To change how null is represented, set the @racket[json-null] parameter.

  @examples[
    #:eval neuron-evaluator
    (define cdc (json-codec (open-input-string "[{\"ab\":12},34]")
                            (open-output-string)))
    (recv cdc)
    (give cdc '(98 #hasheq([zy . 76])))
    (get-output-string ((cdc 'encoder) 'output-port))
  ]
}

@section{Networking}

@defproc[(tcp-codec [make-codec (-> input-port? output-port? process?)]
                    [in-port input-port?]
                    [out-port output-port?]
                    ) process?]{
  Returns the @racket[codec] created by applying @racket[make-codec] to
  @racket[in-port] and @racket[out-port].

  Additional commands:

  @itemlist[
    @item{@racket['address] -- returns the full address of the connection}
    @item{@racket['local-address] -- returns the local address}
    @item{@racket['remote-address] -- returns the remote address}
  ]
}

@defproc[(tcp-client [make-codec (-> input-port? output-port? process?)]
                     [hostname string?]
                     [port-no port-number?]
                     [local-hostname (or/c string? #f) #f]
                     [local-port-no (or/c port-number? #f) #f]
                     ) process?]{
  Establishes a TCP connection to ``@racket[hostname]:@racket[port-no]''.
  Returns a @racket[tcp-codec] created by @racket[make-codec].

  See @racket[tcp-connect] for argument details.
}

@defproc[(tcp-source [make-codec (-> input-port? output-port? process?)]
                     [port-no listen-port-number?]
                     [max-allow-wait exact-nonnegative-integer? 4]
                     [reuse? any/c #f]
                     [hostname (or/c string? #f) #f]
                     ) process?]{
  Creates a ``listening'' TCP server on the local machine. Emits a
  @racket[tcp-codec] for each TCP connection accepted.

  See @racket[tcp-listen] for argument details.

  Commands:

  @itemlist[
    @item{@racket['listen-address] -- returns the address of the listener}
  ]
}

@defproc[(tcp-server [proc (-> any/c any/c)]
                     [make-codec (-> input-port? output-port? process?)]
                     [port-no listen-port-number?]
                     [max-allow-wait exact-nonnegative-integer? 4]
                     [reuse? any/c #f]
                     [hostname (or/c string? #f) #f]
                     ) process?]{
  Creates a ``listening'' TCP server on the local machine, and a
  @racket[server] on @racket[proc]. Emits a
  @racket[tcp-codec]--@racket[server] @racket[bridge] for each TCP connection
  accepted.

  See @racket[tcp-listen] for argument details.
}

@defproc[(tcp-service [proc (-> any/c any/c)]
                      [make-codec (-> input-port? output-port? process?)]
                      [port-no listen-port-number?]
                      [max-allow-wait exact-nonnegative-integer? 4]
                      [reuse? any/c #f]
                      [hostname (or/c string? #f) #f]
                      ) process?]{
  Creates a @racket[tcp-server] on @racket[proc] and @racket[make-codec]. Adds
  connections emitted by the @racket[tcp-server] to a @racket[service] keyed
  by full address.

  Commands:

  @itemlist[
    @item{@racket['peers] -- @racket[service] command @racket['keys]}
    @item{@racket[`(drop ,addr)] -- @racket[service] command @racket[`(drop
      ,addr)]}
  ]
}

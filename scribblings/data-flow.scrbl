#lang scribble/doc

@(require "base.rkt")

@title{Data Flow}

@(defmodule neuron/data-flow #:packages ("neuron"))

@section{Input and Output}

A @deftech{parser} is ...

A @deftech{printer} is ...

A @deftech{codec type} is ...

@defproc[(parser? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{parser}, @racket[#f] otherwise.
}

@defproc[(printer? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{printer}, @racket[#f]
  otherwise.
}

@defproc[(decoder [prs parser?]
                  [in-port input-port?]
                  ) process?]{
  Returns a @deftech{decoder} @tech{source}. Calls @racket[(emit (prs
  in-port))] until @racket[prs] returns @racket[eof]. Closes @racket[in-port]
  when it stops. Dies when @racket[in-port] closes.
}

@defproc[(encoder [prn printer?]
                  [out-port output-port?]
                  ) process?]{
  Returns an @deftech{encoder} @tech{sink}. Calls @racket[(prn (take)
  out-port)] until it takes @racket[eof]. Closes @racket[out-port] when it
  stops. Dies when @racket[out-port] closes.
}

@defproc[(codec [prs parser?]
                [prn printer?]
                [in-port input-port?]
                [out-port output-port?]
                ) process?]{
  Returns a @deftech{codec} @tech{socket} with @tech{source} @racket[(encoder
  prn out-port)] and @tech{sink} @racket[(decoder prs in-port)].
}

@defproc[(make-codec-type [name symbol?]
                          [prs parser?]
                          [prn printer?]
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

  @; @examples[
  @;   #:eval neuron-evaluator
  @racketblock[
    (define-codec line read-line displayln)
    (line-parser (open-input-string "123\nabc\n"))
  ]

  @racketblock[
    (define out-port (open-output-string))
    (line-printer 987 out-port)
    (line-printer "zyx" out-port)
    (get-output-string out-port)
  ]

  @racketblock[
    (define dec (line-decoder (open-input-string "456\ndef\n")))
    (recv dec)
    (recv dec)
    (recv dec)
  ]

  @racketblock[
    (define out-port (open-output-string))
    (define enc (line-encoder out-port))
    (give enc 654)
    (give enc "wvu")
    (give enc eof)
    (get-output-string out-port)
  ]
}

@subsection{files}
@subsubsection{stdio}
@subsection{network}
@subsubsection{tcp}
@subsubsection{udp}

@section{Encoders and Decoders}

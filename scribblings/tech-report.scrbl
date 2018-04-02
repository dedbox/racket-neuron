#lang scribble/manual

@(require
  "base.rkt"
  "drawings.rkt"
  pict/convert)

@title[
  #:style '(unnumbered)
  #:tag "The Neuron Technical Report"
]{The Neuron Technical Report}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@; @section{The implicit process}

@section{A calculus of mediated exchange}

The intransitivity of bare channel synchronization complicates the semantics
of forwarding operations. Exchangers preserve synchronization across mediated
exchanges by deferring the synchronizing operation until all sides have
committed to the exchange.

@subsection{Primitive operations}

@(code-pict-def
  @racket[(exchanger) (code:comment "ex")]
  (exchanger))

An exchanger contains a control channel and a data channel.

@(code-pict-def
  @racket[(offer ex1 #:to ex2)]
  (offer "ex1" #:to "ex2"))

Offering an exchanger to another puts the first into the control channel of
the second.

@(code-pict-def
  @racket[(accept #:from ex) (code:comment "ex*")]
  (accept #:from "ex" "ex*"))

Accepting an exchanger from another gets the first from the control channel of
the second.

@(code-pict-def
  @racket[(put v #:into ex)]
  (put "v" #:into "ex"))

Putting a value into an exchanger puts the value into the data channel of the
exchanger.

@(code-pict-def
  @racket[(get #:from ex) (code:comment "v")]
  (get #:from "ex" "v"))

Getting a value from an exchanger gets the value from the data channel of the
exchanger.

@subsection{Process exchangers}

@(code-pict-defs
  [@racket[(giver tx rx v)]
   (seq (offer "tx" #:to "rx") (put "v" #:into "tx"))]
  [@racket[(taker rx)]
   (seq (accept #:from "rx" "tx") (get #:from "tx" "v"))])

In a give-take exchange, a giver offers its transmitting exchanger to the
receiving exchanger of a taker. After the taker commits to the exchange by
accepting the offer, a single value flows through the transmitting exchanger
from giver to taker.

@(code-pict-defs
  [@racket[(receiver rx tx)]
   (seq (offer "rx" #:to "tx") (get #:from "rx" "v"))]
  [@racket[(emitter tx v)]
   (seq (accept #:from "tx" "rx") (put "v" #:into "rx"))])

In a receive-emit exchange, a receiver offers its receiving exchanger to the
transmitting exchanger of an emitter. After the emitter commits to the
exchange by accepting the offer, a single value flows through the receiving
exchanger from emitter to receiver.

@(code-pict-def
  @racket[(forwarder ex1 ex2 [ex (make-exchanger)])]
  (seq (accept #:from "ex1" "ex") (offer "ex" #:to "ex2")))

In a forwarding exchange, a mediator accepts an exchanger from one exchanger
and then offers it to another.

@(code-pict-def
  @racket[(coupler rx tx [ex (make-exchanger)])]
  (seq (offer "ex" #:to "rx") (offer "ex" #:to "tx")))

In a coupling exchange, a mediator offers an exchanger to two others.

@subsection{Synchronization}

@subsubsection{Forwarders}

When forwarding from a giver to a taker, the giver offers its transmitting
exchanger to the forwarder and then blocks to put a value into the exchanger.
The forwarder accepts the exchanger from the giver and then offers it to the
taker. The taker accepts the giver's transmitting exchanger from the forwarder
and then gets the value from it as the giver unblocks.

@(named-seqs
  ["giver" (offer "tx" #:to "ex1") (put "v" #:into "tx")]
  ["forwarder" (accept #:from "ex1" "tx") (offer "tx" #:to "ex2")]
  ["taker" (accept #:from "ex2" "tx") (get #:from "tx" "v")])

When forwarding from an emitter to a receiver, the receiver offers its
receiving exchanger to the forwarder and then blocks to get a value from the
exchanger. The forwarder accepts the exchanger from the receiver and then
offers it to the emitter. The emitter accepts the receiver's receiving
exchanger from the forwarder and then puts a value into it as the receiver
unblocks.

@(named-seqs
  ["receiver" (offer "rx" #:to "ex1") (get #:from "rx" "v")]
  ["forwarder" (accept #:from "ex1" "rx") (offer "rx" #:to "ex2")]
  ["emitter" (accept #:from "ex2" "rx") (put "v" #:into "rx")])

Compare to forwarding over bare channels. The giver blocks to put a value into
the forwarder while the taker blocks to get a value from the forwarder. The
forwarder accepts a value from the giver as the giver unblocks ahead of the
taker.

@(named-seqs
  ["giver" (ch-put "v" #:into "ch1") (punct "Ø")]
  ["forwader" (ch-get #:from "ch1" "v") (ch-put "v" #:into "ch2")]
  ["taker" (ch-get #:from "ch2" "v") (punct "Ø")])

@subsubsection{Couplers}

Couplers are forwarders for emit-take exchanges. The coupler offers an
exchanger to a taker and then an emitter. The emitter and taker both accept
the exchanger from the coupler and then synchronize by exchanging a value
through the shared exchanger.

@(named-seqs
  ["coupler" (offer "ex" #:to "rx") (offer "ex" #:to "tx")]
  ["emitter" (accept #:from "tx" "ex") (put "v" #:into "ex")]
  ["taker" (accept #:from "rx" "ex") (get #:from "ex" "v")])

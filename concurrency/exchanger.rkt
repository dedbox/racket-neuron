#lang racket/base

(require
 neuron/private/events
 racket/contract/base
 racket/function)

(provide
 (contract-out
  [exchanger? predicate/c]
  [make-exchanger (-> exchanger?)]
  [ex-offer (-> exchanger? #:to exchanger? void?)]
  [ex-accept (-> #:from exchanger? exchanger?)]
  [ex-put (-> any/c #:into exchanger? void?)]
  [ex-get (-> #:from exchanger? any/c)]
  [ex-offer-evt (-> exchanger? #:to exchanger? evt?)]
  [ex-accept-evt (-> #:from exchanger? evt?)]
  [ex-put-evt (-> any/c #:into exchanger? evt?)]
  [ex-get-evt (-> #:from exchanger? evt?)]))

(struct exchanger (ctrl-ch data-ch))

(define (make-exchanger)
  (exchanger
   (make-channel)
   (make-channel)))

;; Commands

(define (ex-offer ex1 #:to ex2)
  (sync (ex-offer-evt ex1 #:to ex2)))

(define (ex-accept #:from ex)
  (sync (ex-accept-evt #:from ex)))

(define (ex-put v #:into ex)
  (sync (ex-put-evt v #:to ex)))

(define (ex-get #:from ex)
  (sync (ex-get-evt #:from ex)))

;; Events

(define (ex-offer-evt ex1 #:to ex2)
  (handle-evt (channel-put-evt (exchanger-ctrl-ch ex2) ex1) void))

(define (ex-accept-evt #:from ex)
  (exchanger-ctrl-ch ex))

(define (ex-put-evt v #:into ex)
  (handle-evt (channel-put-evt (exchanger-data-ch ex) v) void))

(define (ex-get-evt #:from ex)
  (exchanger-data-ch ex))

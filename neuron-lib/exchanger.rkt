#lang racket/base

(require
 racket/contract/base)

(provide
 (contract-out
  [exchanger? predicate/c]
  [make-exchanger (->* () (channel? channel?) exchanger?)]
  [exchanger-ctrl-ch (-> exchanger? channel?)]
  [exchanger-data-ch (-> exchanger? channel?)]
  [offer (-> exchanger? #:to exchanger? void?)]
  [accept (-> #:from exchanger? exchanger?)]
  [put (-> any/c #:into exchanger? void?)]
  [get (-> #:from exchanger? any/c)]
  [offer-evt (-> exchanger? #:to exchanger? evt?)]
  [accept-evt (-> #:from exchanger? evt?)]
  [put-evt (-> any/c #:into exchanger? evt?)]
  [get-evt (-> #:from exchanger? evt?)]))

(struct exchanger (ctrl-ch data-ch))

(define (make-exchanger [ctrl-ch (make-channel)] [data-ch (make-channel)])
  (exchanger ctrl-ch data-ch))

;; Commands

(define (offer ex1 #:to ex2)
  (sync (offer-evt ex1 #:to ex2)))

(define (accept #:from ex)
  (sync (accept-evt #:from ex)))

(define (put v #:into ex)
  (sync (put-evt v #:into ex)))

(define (get #:from ex)
  (sync (get-evt #:from ex)))

;; Events

(define (offer-evt ex1 #:to ex2)
  (handle-evt (channel-put-evt (exchanger-ctrl-ch ex2) ex1) void))

(define (accept-evt #:from ex)
  (exchanger-ctrl-ch ex))

(define (put-evt v #:into ex)
  (handle-evt (channel-put-evt (exchanger-data-ch ex) v) void))

(define (get-evt #:from ex)
  (exchanger-data-ch ex))

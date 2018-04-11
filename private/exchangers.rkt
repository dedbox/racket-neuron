#lang racket/base

(require neuron/concurrency/exchanger
         neuron/private/events
         racket/contract/base)

(provide
 (contract-out
  [giver (-> exchanger? exchanger? any/c void?)]
  [taker (-> exchanger? any/c)]
  [receiver (-> exchanger? exchanger? any/c)]
  [emitter (-> exchanger? any/c void?)]
  [forwarder (-> exchanger? exchanger? void?)]
  [coupler
   (->* (exchanger? exchanger?)
        (exchanger?)
        void?)]
  [giver-evt (-> exchanger? exchanger? any/c evt?)]
  [taker-evt (-> exchanger? evt?)]
  [emitter-evt (-> exchanger? any/c evt?)]
  [receiver-evt (-> exchanger? exchanger? evt?)]
  [forwarder-evt (-> exchanger? exchanger? evt?)]
  [coupler-evt
   (->* (exchanger? exchanger?)
        (exchanger?)
        evt?)]))

;; Commands

(define (giver tx rx v)
  (sync (giver-evt tx rx v)))

(define (taker rx)
  (sync (taker-evt rx)))

(define (receiver rx tx)
  (sync (receiver-evt rx tx)))

(define (emitter tx v)
  (sync (emitter-evt tx v)))

(define (forwarder ex1 ex2)
  (sync (forwarder-evt ex1 ex2)))

(define (coupler rx tx [ex (make-exchanger)])
  (sync (coupler-evt rx tx ex)))

;; Events

(define (giver-evt tx rx v)
  (evt-sequence
   (λ () (offer-evt tx #:to rx))
   (λ () (put-evt v #:into tx))
   #:then void))

(define (taker-evt rx)
  (evt-series
   (λ _ (accept-evt #:from rx))
   (λ (tx) (get-evt #:from tx))))

(define (receiver-evt rx tx)
  (evt-sequence
   (λ () (offer-evt rx #:to tx))
   (λ () (get-evt #:from rx))))

(define (emitter-evt tx v)
  (evt-series
   (λ _ (accept-evt #:from tx))
   (λ (rx) (put-evt v #:into rx))
   #:then void))

(define (forwarder-evt ex1 ex2)
  (evt-series
   (λ _ (accept-evt #:from ex1))
   (λ (ex) (offer-evt ex #:to ex2))
   #:then void))

(define (coupler-evt rx tx [ex (make-exchanger)])
  (evt-sequence
   (λ () (offer-evt ex #:to rx))
   (λ () (offer-evt ex #:to tx))
   #:then void))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
    "giver -> taker"
    (define tx (make-exchanger))
    (define rx (make-exchanger))
    (thread (λ () (for ([j 10]) (check = (taker rx) j))))
    (for ([i 10]) (check-pred void? (giver tx rx i))))

  (test-case
    "taker <- giver"
    (define tx (make-exchanger))
    (define rx (make-exchanger))
    (thread (λ () (for ([i 10]) (check-pred void? (giver tx rx i)))))
    (for ([j 10]) (check = (taker rx) j)))

  (test-case
    "emitter -> receiver"
    (define tx (make-exchanger))
    (define rx (make-exchanger))
    (thread (λ () (for ([i 10]) (check-pred void? (emitter tx i)))))
    (for ([j 10]) (check = (receiver rx tx) j)))

  (test-case
    "receiver <- emitter"
    (define tx (make-exchanger))
    (define rx (make-exchanger))
    (thread (λ () (for ([j 10]) (check = (receiver rx tx) j))))
    (for ([i 10]) (check-pred void? (emitter tx i))))

  (test-case
    "giver -> forwarder -> taker"
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([_ 10]) (forwarder rx1 rx2))))
    (thread (λ () (for ([j 10]) (check = (taker rx2) j))))
    (for ([i 10]) (check-pred void? (giver tx1 rx1 i))))

  (test-case
    "taker <- forwarder <- giver"
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([_ 10]) (forwarder rx1 rx2))))
    (thread (λ () (for ([i 10]) (check-pred void? (giver tx1 rx1 i)))))
    (for ([j 10]) (check = (taker rx2) j)))

  (test-case
    "emitter -> forwarder -> receiver" 
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([k 10]) (forwarder tx2 tx1))))
    (thread (λ () (for ([j 10]) (check = (receiver rx2 tx2) j))))
    (for ([i 10]) (check-pred void? (emitter tx1 i))))

  (test-case
    "receiver <- forwarder <- emitter"
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([_ 10]) (forwarder tx2 tx1))))
    (thread (λ () (for ([i 10]) (check-pred void? (emitter tx1 i)))))
    (for ([j 10]) (check = (receiver rx2 tx2) j)))

  (test-case
    "emitter -> coupler -> taker"
    (define rx (make-exchanger))
    (define tx (make-exchanger))
    (thread (λ () (for ([_ 10]) (coupler rx tx))))
    (thread (λ () (for ([j 10]) (check = (taker tx) j))))
    (for ([i 10]) (check-pred void? (emitter rx i))))

  (test-case
    "taker <- coupler <- emitter"
    (define rx (make-exchanger))
    (define tx (make-exchanger))
    (thread (λ () (for ([_ 10]) (coupler rx tx))))
    (thread (λ () (for ([i 10]) (check-pred void? (emitter rx i)))))
    (for ([j 10]) (check = (taker tx) j))))

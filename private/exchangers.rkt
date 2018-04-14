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
  [filterer (-> exchanger? exchanger? #:with (-> any/c any/c) void?)]
  [coupler
   (->* (exchanger? exchanger?)
        (exchanger?)
        void?)]
  [giver-evt (-> exchanger? exchanger? any/c evt?)]
  [taker-evt (-> exchanger? evt?)]
  [emitter-evt (-> exchanger? any/c evt?)]
  [receiver-evt (-> exchanger? exchanger? evt?)]
  [forwarder-evt (-> exchanger? exchanger? evt?)]
  [filterer-evt (-> exchanger? exchanger? #:with (-> any/c any/c) evt?)]
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

(define (filterer ex1 ex2 #:with proc)
  (sync (filterer-evt ex1 ex2 #:with proc)))

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

(define (filterer-evt ex1 ex2 #:with proc)
  (evt-series
   (λ _ (accept-evt #:from ex1))
   (λ (ex)
     (offer-evt
      (make-exchanger
       (exchanger-ctrl-ch ex)
       (impersonate-channel
        (exchanger-data-ch ex)
        (λ (c) (values c proc))
        (λ (c v) (proc v))))
      #:to ex2))))

(define (coupler-evt rx tx [ex (make-exchanger)])
  (evt-sequence
   (λ () (offer-evt ex #:to rx))
   (λ () (offer-evt ex #:to tx))
   #:then void))

;;; Unit Tests

(module+ test
  (require rackunit
           racket/function)

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
    "giver -> filterer -> taker"
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([_ 10]) (filterer rx1 rx2 #:with add1))))
    (thread (λ () (for ([j 10]) (check = (taker rx2) (+ j 1)))))
    (for ([i 10]) (check-pred void? (giver tx1 rx1 i))))

  (test-case
    "taker <- filterer <- giver"
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([_ 10]) (filterer rx1 rx2 #:with sub1))))
    (thread (λ () (for ([i 10]) (check-pred void? (giver tx1 rx1 i)))))
    (for ([j 10]) (check = (taker rx2) (- j 1))))

  (test-case
    "emitter -> filterer -> receiver" 
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([k 10]) (filterer tx2 tx1 #:with (curry * 2)))))
    (thread (λ () (for ([j 10]) (check = (receiver rx2 tx2) (+ j j)))))
    (for ([i 10]) (check-pred void? (emitter tx1 i))))

  (test-case
    "receiver <- filterer <- emitter"
    (define tx1 (make-exchanger))
    (define rx1 (make-exchanger))
    (define tx2 (make-exchanger))
    (define rx2 (make-exchanger))
    (thread (λ () (for ([_ 10]) (filterer tx2 tx1 #:with (curry * 3)))))
    (thread (λ () (for ([i 10]) (check-pred void? (emitter tx1 i)))))
    (for ([j 10]) (check = (receiver rx2 tx2) (+ j j j))))

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

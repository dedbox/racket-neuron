#lang racket/base

(require
 neuron/event
 neuron/exchanger
 neuron/process
 neuron/process/exchanger
 racket/contract/base)

(provide
 (contract-out
  [give (->* (process?) (any/c) boolean?)]
  [take (-> any/c)]
  [recv (-> process? any/c)]
  [emit (->* () (any/c) void?)]
  [call (-> process? any/c any/c)]
  [forward-to (-> process? void?)]
  [forward-from (-> process? void?)]
  [filter-to (-> process? #:with (-> any/c any/c) void?)]
  [filter-from (-> process? #:with (-> any/c any/c) void?)]
  [couple
   (->* (process? process?)
        (exchanger?)
        void?)]
  [give-evt (->* (process?) (any/c) evt?)]
  [take-evt (-> evt?)]
  [emit-evt (->* () (any/c) evt?)]
  [recv-evt (-> process? evt?)]
  [forward-to-evt (-> process? evt?)]
  [forward-from-evt (-> process? evt?)]
  [filter-to-evt (-> process? #:with (-> any/c any/c) evt?)]
  [filter-from-evt (-> process? #:with (-> any/c any/c) evt?)]
  [couple-evt
   (->* (process? process?)
        (exchanger?)
        evt?)]))

;; Commands

(define (give π [v (void)])
  (sync (give-evt π v)))

(define (take)
  (sync (take-evt)))

(define (recv π)
  (sync (recv-evt π)))

(define (emit [v (void)])
  (sync (emit-evt v)))

(define (call π [v (void)])
  (give π v)
  (recv π))

(define (forward-to π)
  (sync (forward-to-evt π)))

(define (forward-from π)
  (sync (forward-from-evt π)))

(define (filter-to π #:with proc)
  (sync (filter-to-evt π #:with proc)))

(define (filter-from π #:with proc)
  (sync (filter-from-evt π #:with proc)))

(define (couple π1 π2 [ex (make-exchanger)])
  (sync (couple-evt π1 π2 ex)))

;; Events

(define (give-evt π [v (void)])
  (define tx (process-tx (current-process)))
  (define rx (process-rx π))
  (choice-evt
   (handle-evt (giver-evt tx rx v) (λ _ #t))
   (handle-evt π (λ _ #f))))

(define (take-evt)
  (taker-evt (process-rx (current-process))))

(define (recv-evt π)
  (define tx (process-tx π))
  (define rx (process-rx (current-process)))
  (choice-evt
   (receiver-evt rx tx)
   (handle-evt π (λ _ eof))))

(define (emit-evt [v (void)])
  (define tx (process-tx (current-process)))
  (handle-evt (emitter-evt tx v) void))

(define (forward-to-evt π)
  (forwarder-evt
   (process-rx (current-process))
   (process-rx π)))

(define (forward-from-evt π)
  (forwarder-evt
   (process-tx (current-process))
   (process-tx π)))

(define (filter-to-evt π #:with proc)
  (filterer-evt
   (process-rx (current-process))
   (process-rx π)
   #:with proc))

(define (filter-from-evt π #:with proc)
  (filterer-evt
   (process-tx (current-process))
   (process-tx π)
   #:with proc))

(define (couple-evt π1 π2 [ex (make-exchanger)])
  (coupler-evt
   (process-rx π1)
   (process-tx π2)
   ex))

(module+ test
  (require rackunit)

  ;; Commands

  (test-case
    "give blocks until π accepts v."
    (check-false (not (give (process (λ () (take)))))))

  (test-case
    "give blocks until π dies."
    (check-false (give (process die))))

  (test-case
    "give returns #t if π accepts v."
    (check-true (give (process (λ () (take))))))

  (test-case
    "give returns #f if π dies before accepting v."
    (check-false (give (process die))))

  (test-case
    "take blocks until a value is provided to π."
    (define π (process (λ () (check-false (not (take))))))
    (give π)
    (void (sync π)))

  (test-case
    "take returns the provided value."
    (define π (process (λ () (check = (take) 7))))
    (give π 7)
    (void (sync π)))

  (test-case
    "recv blocks until a value is accepted from π."
    (check-false (not (recv (process (λ () (emit)))))))

  (test-case
    "recv blocks until π dies."
    (check-false (not (recv (process die)))))

  (test-case
    "recv returns the value accepted from π."
    (check = (recv (process (λ () (emit 13)))) 13))

  (test-case
    "recv returns eof when π dies."
    (check-pred eof-object? (recv (process die))))

  (test-case
    "emit blocks until a process accepts v."
    (define π (process (λ () (check-false (not (emit))))))
    (recv π)
    (void (sync π)))

  (test-case
    "emit returns void."
    (define π (process (λ () (check-pred void? (emit)))))
    (recv π)
    (void (sync π)))

  (test-case
    "call gives v to π and then recvs from π."
    (define π (process (λ () (emit (add1 (take))))))
    (check = (call π 47) 48))

  ;; Events

  (test-case
    "A give-evt is ready when π accepts v."
    (define π (process (λ () (emit) (take))))
    (define evt (give-evt π))
    (check-false (sync/timeout 0 evt))
    (recv π)
    (check-false (not (sync evt))))

  (test-case
    "A give-evt syncs to #t if π accepts v."
    (check-true (sync (give-evt (process (λ () (take)))))))

  (test-case
    "A give-evt syncs to #f if π dies before accepting v."
    (define π (process deadlock))
    (define evt (give-evt π))
    (kill π)
    (check-false (sync evt)))

  (test-case
    "A take-evt is ready when a process provides a value."
    (define π (process (λ () (check-false (not (sync (take-evt)))))))
    (give π)
    (void (sync π)))

  (test-case
    "A take-evt syncs to the provided value."
    (define π (process (λ () (check eq? (sync (take-evt)) 3))))
    (give π 3)
    (void (sync π)))

  (test-case
    "A recv-evt is ready when a value is accepted from π."
    (define π (process (λ () (take) (emit))))
    (define evt (recv-evt π))
    (check-false (sync/timeout 0 evt))
    (give π)
    (check-false (not (sync evt))))

  (test-case
    "A recv-evt syncs to the value accepted from π."
    (check = (sync (recv-evt (process (λ () (emit 5))))) 5))

  (test-case
    "A recv-evt syncs to eof if π dies."
    (define π (process deadlock))
    (define evt (recv-evt π))
    (kill π)
    (check-pred eof-object? (sync evt)))

  (test-case
    "An emit-evt is ready when a process accepts v."
    (define π (process (λ () (check-false (not (sync (emit-evt)))))))
    (recv π)
    (void (sync π)))

  (test-case
    "An emit-evt syncs to void."
    (define π (process (λ () (check-pred void? (sync (emit-evt))))))
    (recv π)
    (void (sync π)))

  (test-case
    "forward-to-evt"
    (define π1 (process (λ () (emit (add1 (take))))))
    (define π2 (process (λ () (sync (forward-to-evt π1)))))
    (check-true (give π2 1))
    (check = (recv π1) 2))

  (test-case
    "forward-from-evt"
    (define π1 (process (λ () (emit 1))))
    (define π2 (process (λ () (sync (forward-from-evt π1)))))
    (check = (recv π2) 1)
    (check-pred eof-object? (recv π2)))

  (test-case
    "filter-to-evt"
    (define π1 (process (λ () (emit (take)))))
    (define π2 (process (λ () (sync (filter-to-evt π1 #:with add1)))))
    (check-true (give π2 1))
    (check = (recv π1) 2))

  (test-case
    "filter-from-evt"
    (define π1 (process (λ () (emit 1))))
    (define π2 (process (λ () (sync (filter-from-evt π1 #:with add1)))))
    (check = (recv π2) 2)
    (check-pred eof-object? (recv π2)))

  (test-case
    "couple-evt"
    (define π1 (process (λ () (emit 1) (check = (take) 2))))
    (define π2 (process (λ () (check = (take) 1) (emit 2))))
    (void (sync
           (evt-set
            (thread (λ () (couple π1 π2)))
            (thread (λ () (couple π2 π1))))))))

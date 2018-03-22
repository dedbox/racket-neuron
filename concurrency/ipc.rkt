#lang racket/base

(require neuron/concurrency/process
         neuron/private/events
         racket/contract/base)

(provide
 (contract-out
  [give (->* (process?) (any/c) boolean?)]
  [take (-> any/c)]
  [try-take (-> any/c)]
  [emit (->* () (any/c) void?)]
  [recv (-> process? any/c)]
  [try-recv (-> process? any/c)]
  [call (-> process? any/c any/c)]
  [give-evt (->* (process?) (any/c) evt?)]
  [take-evt (-> evt?)]
  [emit-evt (->* () (any/c) evt?)]
  [recv-evt (-> process? evt?)]))

;; Commands

(define (give π [v (void)])
  (sync (give-evt π v)))

(define (take)
  (sync (take-evt)))

(define (try-take)
  (channel-try-get (process-in-ch (current-process))))

(define (emit [v (void)])
  (sync (emit-evt v)))

(define (recv π)
  (sync (recv-evt π)))

(define (try-recv π)
  (channel-try-get (process-out-ch π)))

(define (call π [v (void)])
  (give π v)
  (recv π))

;; Events

(define (give-evt π [v (void)])
  (choice-evt
    (handle-evt (channel-put-evt (process-in-ch π) v) (λ _ #t))
    (handle-evt π (λ _ #f))))

(define (take-evt)
  (process-in-ch (current-process)))

(define (emit-evt [v (void)])
  (handle-evt (channel-put-evt (process-out-ch (current-process)) v) void))

(define (recv-evt π)
  (choice-evt
    (process-out-ch π)
    (handle-evt π (λ _ eof))))

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
    "try-take returns the provided value if any."
    (define π1 (process (λ () (emit) (check = (try-take) 11))))
    (define π2 (process (λ () (give π1 11))))
    (sync/timeout 0.1 π2)
    (recv π1)
    (sync π2)
    (void (sync π1)))

  (test-case
    "try-take returns #f if no value is provided."
    (void (sync (process (λ () (check-false (try-take)))))))

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
    "try-recv returns the value accepted from π if any."
    (define π (process (λ () (emit 17))))
    (sync/timeout 0.1 π)
    (check = (try-recv π) 17))

  (test-case
    "try-recv returns #f if no value is accepted."
    (check-false (try-recv (process deadlock))))

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
    (check-pred eof-object? (sync evt))))

#lang racket/base

(require
 cmx
 event
 neuron/event
 neuron/process
 racket/contract/base)

(provide
 (contract-out
  [give (-> process? any/c ... boolean?)]
  [take (-> any)]
  [recv (-> process? any)]
  [emit (-> any/c ... void?)]
  [call (-> process? any/c ... any)]
  [forward-to (-> process? void?)]
  [forward-from (-> process? void?)]
  [filter-to (-> process? procedure? void?)]
  [filter-from (-> process? procedure? void?)]
  [splice (-> process? process? void?)]
  [deliver (-> (hash/c any/c process?) void?)]
  [give-evt (-> process? any/c ... evt?)]
  [take-evt (-> evt?)]
  [recv-evt (-> process? evt?)]
  [emit-evt (-> any/c ... evt?)]
  [call-evt (-> process? any/c ... evt?)]
  [forward-to-evt (-> process? evt?)]
  [forward-from-evt (-> process? evt?)]
  [filter-to-evt (-> process? procedure? evt?)]
  [filter-from-evt (-> process? procedure? evt?)]
  [splice-evt (-> process? process? evt?)]
  [deliver-evt (-> (hash/c any/c process?) evt?)]))

;; Commands

(define (give π . vs)
  (sync (apply give-evt π vs)))

(define (take)
  (sync (take-evt)))

(define (recv π)
  (sync (recv-evt π)))

(define (emit . vs)
  (sync (apply emit-evt vs)))

(define (call π . vs)
  (sync (apply call-evt π vs)))

(define (forward-to π)
  (sync (forward-to-evt π)))

(define (forward-from π)
  (sync (forward-from-evt π)))

(define (filter-to π f)
  (sync (filter-to-evt π f)))

(define (filter-from π f)
  (sync (filter-from-evt π f)))

(define (splice π1 π2)
  (sync (splice-evt π1 π2)))

(define (deliver πs)
  (sync (deliver-evt πs)))

;; Events

(define (give-evt π . vs)
  (bind
   (choice-evt π (say* (process-in π) vs))
   (compose return not process?)))

(define (take-evt)
  (hear (process-in (current-process))))

(define (recv-evt π)
  (choice-evt
   (ask (process-out π))
   (handle-evt π (λ _ eof))))

(define (emit-evt . vs)
  (tell* (process-out (current-process)) vs))

(define (call-evt π . vs)
  (seq (apply give-evt π vs) (recv-evt π)))

(define (forward-to-evt π)
  (forward
   (process-in (current-process))
   (process-in π)))

(define (forward-from-evt π)
  (forward
   (process-out (current-process))
   (process-out π)))

(define (filter-to-evt π f)
  (filter (process-in (current-process)) (process-in π) #:put f))

(define (filter-from-evt π f)
  (filter (process-out (current-process)) (process-out π) #:get f))

(define (splice-evt π1 π2)
  (couple (process-out π1) (process-in π2)))

(define (deliver-evt πs)
  (dispatch
   (process-in (current-process))
   (for/hash ([(key π) πs]) (values key (process-in π)))))

(module+ test
  (require rackunit
           racket/function
           (only-in racket/list shuffle))

  ;; Commands

  (test-case
    "give blocks until π accepts v."
    (check-false (not (give (process take)))))

  (test-case
    "give blocks until π dies."
    (check-false (give (process die))))

  (test-case
    "give returns #t if π accepts v."
    (check-true (give (process take))))

  (test-case
    "give returns #f if π dies before accepting v."
    (check-false (give (process die))))

  (test-case
    "take blocks until a value is provided to π."
    (define π (process take))
    (give π)
    (void (sync π)))

  (test-case
    "take returns the provided value."
    (define π (process (λ () (check = (take) 7))))
    (give π 7)
    (void (sync π)))

  (test-case
    "recv blocks until a value is accepted from π."
    (recv (process emit)))

  (test-case
    "recv blocks until π dies."
    (void (recv (process die))))

  (test-case
    "recv returns the value accepted from π."
    (check = (recv (process (λ () (emit 13)))) 13))

  (test-case
    "recv returns eof when π dies."
    (check-pred eof-object? (recv (process die))))

  (test-case
    "emit blocks until a process accepts v."
    (define π (process emit))
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
    (check-true (sync (give-evt (process take)))))

  (test-case
    "A give-evt syncs to #f if π dies before accepting v."
    (define π (process deadlock))
    (define evt (give-evt π))
    (kill π)
    (check-false (sync evt)))

  (test-case
    "A take-evt is ready when a process provides a value."
    (define π (process (λ () (sync (take-evt)))))
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
    (sync evt))

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
    (define π (process (λ () (sync (emit-evt)))))
    (recv π)
    (void (sync π)))

  (test-case
    "forward-to-evt"
    (define π1 (process (λ () (emit (add1 (take))))))
    (define π2 (process (λ () (sync (seq0 (forward-to-evt π1) π1)))))
    (check-true (give π2 1))
    (check = (recv π1) 2))

  (test-case
    "forward-from-evt"
    (define π1 (process (λ () (emit 1))))
    (define π2 (process (λ () (sync (seq0 (forward-from-evt π1) π1)))))
    (check = (recv π2) 1)
    (check-pred eof-object? (recv π2)))

  (test-case
    "filter-to-evt"
    (define π1 (process (λ () (emit (take)))))
    (define π2 (process (λ () (sync (seq0 (filter-to-evt π1 add1) π1)))))
    (check-true (give π2 1))
    (check = (recv π1) 2))

  (test-case
    "filter-from-evt"
    (define π1 (process (λ () (emit 1))))
    (define π2 (process (λ () (sync (seq0 (filter-from-evt π1 add1) π1)))))
    (check = (recv π2) 2)
    (check-pred eof-object? (recv π2)))

  (test-case "splice-evt"
    (define π1 (process (λ () (emit 1) (check = (take) 2))))
    (define π2 (process (λ () (check = (take) 1) (emit 2))))
    (check-pred void? (splice π1 π2))
    (check-pred void? (splice π2 π1)))

  (test-case "deliver-evt"
    (define L null)
    (define (push v) (set! L (cons v L)))
    (define is (build-list 10 values))
    (define js (shuffle is))
    (define πs
      (for/hash ([i is])
        (values i (process (λ () (push (cons i (take))))))))
    (define π
      (process
       (λ ()
         (for ([_ is]) (deliver πs))
         (for-each wait (hash-values πs)))))
    (for ([i is] [j js]) (give π j i))
    (wait π)
    (for ([i is] [j js] [result (reverse L)])
      (check equal? result (cons j i)))))

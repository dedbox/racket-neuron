#lang racket/base

(require
 event
 neuron/evaluation
 neuron/process
 neuron/process/messaging
 neuron/syntax
 racket/contract/base
 racket/dict
 racket/function
 (only-in racket/list flatten make-list last)
 racket/match)

(provide
 (contract-out
  [server (-> (-> any/c any/c) process?)]
  [proxy (->* (process?) ((or/c procedure? #f) (or/c procedure? #f)) process?)]
  [proxy-to (->* (process?) ((or/c (-> any/c any/c) #f)) process?)]
  [proxy-from (->* (process?) ((or/c (-> any/c any/c) #f)) process?)]
  [sink (-> procedure? process?)]
  [source (-> (-> any) process?)]
  [stream (-> process? process? process?)]
  [simulator (->* ((-> real? any)) (#:rate real?) process?)]
  [pipe (-> process? ... process?)]
  [bridge (-> process? process? process?)]
  [service (->* () (#:on-drop (-> any/c process? any)) process?)]
  [managed (->* (process?)
                (#:on-eof-in (-> process? any)
                 #:on-eof-out (-> process? any))
                process?)]
  [shutdown (-> process? void?)]
  [shutdown-evt (-> process? evt?)]))

;; Processes

(define (server proc)
  (process (λ () (forever (emit (proc (take)))))))

(define (proxy π [f-to #f] [f-from #f])
  (start
   (process
    (λ ()
      (define evt
        (choice-evt
         (if f-to (filter-to-evt π f-to) (forward-to-evt π))
         (if f-from (filter-from-evt π f-from) (forward-from-evt π))))
      (sync (loop (λ _ evt)) (handle-evt π die))))
   #:on-stop (λ () (stop π))))

(define (proxy-to π [f #f])
  (start
   (process
    (λ ()
      (define evt (if f (filter-to-evt π f) (forward-to-evt π)))
      (sync (choice-evt (loop (λ _ evt)) (handle-evt π die)))))
   #:on-stop (λ () (stop π))))

(define (proxy-from π [f #f])
  (start
   (process
    (λ ()
      (define evt (if f (filter-from-evt π f) (forward-from-evt π)))
      (sync (loop (λ _ evt)) (handle-evt π die))))
   #:on-stop (λ () (stop π))))

(define (sink proc)
  (process (λ () (forever (proc (take))))))

(define (source proc)
  (process (λ () (forever (emit (proc))))))

(define (stream snk src)
  (start
   (process
    (λ ()
      (define evt (choice-evt (forward-to-evt snk) (forward-from-evt src)))
      (sync
       (loop (λ _ evt))
       (handle-evt (async-set snk src) die))))
   #:on-stop (λ () (stop snk) (stop src))
   #:command (bindings [(sink) snk] [(source) src] #:else unhandled)))

(define (simulator proc #:rate [rate 10])
  (process
   (λ ()
     (define period (/ 1000.0 rate))
     (define timestamp (current-inexact-milliseconds))
     (forever
       (set! timestamp (+ timestamp period))
       (sync (alarm-evt timestamp))
       (proc period)))))

(define (pipe . πs)
  (start
   (process
    (λ ()
      (sync
       (thread (λ () (forever (emit (foldl call (take) πs)))))
       (apply choice-evt πs))))
   #:on-stop (λ () (for-each stop πs))))

(define (bridge π1 π2)
  (start
   (process
    (λ ()
      (sync
       (loop (λ _ (splice-evt π1 π2)))
       (loop (λ _ (splice-evt π2 π1)))
       (bind (choice-evt π1 π2) die))))
   #:on-stop (λ () (stop π1) (stop π2))
   #:command (bindings [(1) π1] [(2) π2] #:else unhandled)
   #:command π1
   #:command π2))

(define (service #:on-drop [on-drop void])
  (define peers (make-hash))
  (define in-latch (make-semaphore))
  (define out-latch (make-semaphore))
  (define (add-peer key π)
    (hash-set! peers key π)
    (semaphore-post in-latch)
    (semaphore-post out-latch))
  (define (drop-peer key)
    (and
     (hash-has-key? peers key)
     (let ([π (hash-ref peers key)])
       (hash-remove! peers key)
       (on-drop key π)
       (semaphore-post in-latch)
       (semaphore-post out-latch))
     #t))
  (define (peer-emit-evt key π)
    (bind (recv-evt π) (λ vs (apply emit-evt (cons key vs)))))
  (define (peer-emit-evts)
    (apply choice-evt (dict-map (hash->list peers) peer-emit-evt)))
  (start
   (process
    (λ ()
      (sync
       (loop (λ _ (choice-evt in-latch (deliver-evt peers))))
       (loop (λ _ (choice-evt out-latch (peer-emit-evts)))))))
   #:on-stop (λ () (for-each drop-peer (dict-keys (hash->list peers))))
   #:command (bindings
              [(peers) (hash->list peers)]
              [(add ,key ,π) (add-peer key π)]
              [(get ,key) (hash-ref peers key #f)]
              [(drop ,key) (drop-peer key)]
              #:else unhandled)))

(define (managed π
                 #:on-eof-in [on-eof-in stop]
                 #:on-eof-out [on-eof-out stop])
  (define (eof-msg? vs)
    (and (not (null? vs)) (eof-object? (car vs)) (null? (cdr vs))))
  (start
   (process
    (λ ()
      (define in-evt
        (bind (take-evt)
              (λ vs
                (when (eof-msg? vs) (on-eof-in π))
                (apply give-evt π vs))))
      (define out-evt
        (bind (recv-evt π)
              (λ vs
                (when (eof-msg? vs) (on-eof-out π))
                (apply emit-evt vs))))
      (sync
       (loop (λ _ in-evt))
       (loop (λ _ out-evt))
       (handle-evt π die))))
   #:on-stop (λ () (stop π))
   #:command π))

(define (shutdown π)
  (give π eof)
  (wait π))

(define (shutdown-evt π)
  (seq (give-evt π eof) (fmap void π)))

(module+ test
  (require rackunit
           racket/async-channel)

  ;; Syntax

  (test-case "forever evaluates its body repeatedly"
    (define N 0)
    (define π
      (process (λ () (forever (set! N (+ N 1)) (when (> N 100) (die))))))
    (wait π)
    (check > N 100))

  (test-case "while evaluates its body for as long as expr evaluates to #t"
    (define count 0)
    (define π (process (λ () (while (<= count 100) (set! count (add1 count))))))
    (wait π)
    (check > count 100))

  (test-case "until evaluates its body for as long as expr evaluates to #f"
    (define count 0)
    (define π (process (λ () (until (> count 100) (set! count (add1 count))))))
    (wait π)
    (check > count 100))

  ;; Processes

  (test-case "server applies proc and emits the result"
    (define π (server add1))
    (give π 23)
    (check = (recv π) 24))

  (test-case "proxy forwards to π"
    (define π (proxy (server (λ (x) (* x 2)))))
    (give π 37)
    (check = (recv π) 74))

  (test-case "proxy forwards from π"
    (define π (proxy (server (λ (x) (* x 2)))))
    (give π 43)
    (check = (recv π) 86))

  (test-case "proxy stops π when it stops"
    (define π (process deadlock))
    (stop (proxy π))
    (check-true (dead? π)))

  (test-case "proxy dies when π dies"
    (define π (process deadlock))
    (define π* (proxy π))
    (kill π)
    (wait π*)
    (check-true (dead? π*)))

  (test-case "proxy with filter"
    (define π (process (λ () (emit 4) (check = (take) 9))))
    (define π* (proxy π sub1 (curry * 3)))
    (check = (recv π*) 12)
    (check-true (give π* 10)))

  (test-case "proxy-to forwards to π"
    (define π (server add1))
    (define to-π (proxy-to π))
    (for ([i 10])
      (check-true (give to-π i))
      (check = (recv π) (add1 i))))

  (test-case "proxy-to does not forward from π"
    (define π (server add1))
    (define to-π (proxy-to π))
    (for ([i 10])
      (give to-π i)
      (check-false (sync/timeout 0 (recv-evt to-π)))
      (check = (recv π) (add1 i))))

  (test-case "proxy-to stops π when it stops"
    (define π (server add1))
    (define to-π (proxy-to π))
    (check-pred alive? π)
    (check-pred alive? to-π)
    (stop to-π)
    (check-pred dead? to-π)
    (check-pred dead? π))

  (test-case "proxy-to dies when π dies"
    (define π (server add1))
    (define to-π (proxy-to π))
    (check-pred alive? π)
    (check-pred alive? to-π)
    (kill π)
    (check-pred dead? π)
    (wait to-π)
    (check-pred dead? to-π))

  (test-case "proxy-to with filter"
    (define π (process (λ () (check = (take) 6))))
    (check-true (give (proxy-to π (curry * 3)) 2)))

  (test-case "proxy-from does not forward to π"
    (define π (server add1))
    (define from-π (proxy-from π))
    (for ([i 10])
      (check-false (sync/timeout 0 (give-evt from-π i)))))

  (test-case "proxy-from forwards from π"
    (define π (server add1))
    (define from-π (proxy-from π))
    (for ([i 10])
      (check-true (give π i))
      (check = (recv from-π) (add1 i))))

  (test-case "proxy-from stops π when it stops"
    (define π (server add1))
    (define from-π (proxy-from π))
    (check-pred alive? π)
    (check-pred alive? from-π)
    (stop from-π)
    (check-pred dead? from-π)
    (check-pred dead? π))

  (test-case "proxy-from dies when π dies"
    (define π (server add1))
    (define from-π (proxy-from π))
    (check-pred alive? π)
    (check-pred alive? from-π)
    (kill π)
    (check-pred dead? π)
    (wait from-π)
    (check-pred dead? from-π))

  (test-case "proxy-from with filter"
    (define π (process (λ () (emit 3))))
    (check = (recv (proxy-from π (curry * 4))) 12))

  (test-case "sink applies proc to each value taken"
    (define last -1)
    (define π (sink (λ (n) (check = n (+ last 1)) (set! last n))))
    (for ([i 10]) (give π i)))

  (test-case "sink ignores the result of proc"
    (define π (sink add1))
    (give π 31)
    (check-false (sync/timeout 0 (recv-evt π))))

  (test-case "source applies proc repeatedly and emits each result"
    (define N -1)
    (define π (source (λ () (set! N (+ N 1)) N)))
    (for ([i 10]) (check = (recv π) i)))

  (test-case "stream forwards to snk"
    (define result-ch (make-channel))
    (define π (stream (sink (curry channel-put result-ch)) (source void)))
    (for ([i 10])
      (give π i)
      (check = (channel-get result-ch) i)))

  (test-case "stream forwards from src"
    (define π (stream (sink void) (source random)))
    (for ([_ 10])
      (define v (recv π))
      (check >= v 0)
      (check <= v 1)))

  (test-case "stream stops snk and src when it stops"
    (define ch (make-async-channel))
    (define π
      (stream
       (start (sink deadlock) #:on-stop (λ () (async-channel-put ch #t)))
       (start (source deadlock) #:on-stop (λ () (async-channel-put ch #t)))))
    (stop π)
    (check-true (async-channel-get ch))
    (check-true (async-channel-get ch)))

  (test-case "stream dies when snk and src die"
    (define snk (sink deadlock))
    (define src (source deadlock))
    (define sock (stream snk src))
    (kill snk)
    (kill src)
    (wait sock)
    (check-true (dead? sock)))

  (test-case "stream does not die when snk dies if src is alive"
    (define snk (sink deadlock))
    (define src (source deadlock))
    (define sock (stream snk src))
    (kill snk)
    (check-true (alive? src))
    (check-false (dead? sock)))

  (test-case "stream does not die when src dies if snk is alive"
    (define snk (sink deadlock))
    (define src (source deadlock))
    (define sock (stream snk src))
    (kill src)
    (check-true (alive? snk))
    (check-false (dead? sock)))

  (test-case "stream command 'sink returns snk"
    (define snk (sink deadlock))
    (define sock (stream snk (source deadlock)))
    (check eq? (sock 'sink) snk))

  (test-case "stream command 'source returns src"
    (define src (source deadlock))
    (define sock (stream (sink deadlock) src))
    (check eq? (sock 'source) src))

  (test-case "simulator calls proc at rate"
    (define N 0)
    (define t0 (current-inexact-milliseconds))
    (wait (simulator (λ _ (set! N (+ N 1)) (when (= N 10) (die))) #:rate 100))
    (define t10 (current-inexact-milliseconds))
    (check = N 10)
    (check >= (- t10 t0) 100))

  (test-case "pipe calls πs in series"
    (define π (apply pipe (for/list ([_ 10]) (server add1))))
    (give π 49)
    (check = (recv π) 59))

  (test-case "pipe stops all πs when it stops"
    (define πs (for/list ([_ 10]) (process deadlock)))
    (stop (apply pipe πs))
    (for ([π πs]) (check-true (dead? π))))

  (test-case "pipe dies when any π dies"
    (for ([i 3])
      (define πs (for/list ([_ 3]) (process deadlock)))
      (define p (apply pipe πs))
      (kill (list-ref πs i))
      (wait p)
      (check-true (dead? p))))

  (test-case "bridge forwards from π1 to π2 and vice versa"
    (wait
     (bridge
      (process (λ () (emit 51) (check = (take) 53)))
      (process (λ () (check = (take) 51) (emit 53))))))

  (test-case "bridge stops π1 and π2 when it stops"
    (define π1 (process deadlock))
    (define π2 (process deadlock))
    (stop (bridge π1 π2))
    (check-true (dead? π1))
    (check-true (dead? π2)))

  (test-case "bridge dies when π1 dies"
    (define π1 (process deadlock))
    (define π (bridge π1 (process deadlock)))
    (kill π1)
    (wait π))

  (test-case "bridge dies when π2 dies"
    (define π2 (process deadlock))
    (define π (bridge (process deadlock) π2))
    (kill π2)
    (wait π))

  (test-case "bridge command 1 returns π1"
    (define π1 (process deadlock))
    (define π2 (process deadlock))
    (check equal? π1 ((bridge π1 π2) 1)))

  (test-case "bridge command 2 return π2"
    (define π1 (process deadlock))
    (define π2 (process deadlock))
    (check equal? π2 ((bridge π1 π2) 2)))

  (test-case "bridge forwards unhandled commands to π1 first"
    (define π
      (bridge (start (process deadlock) #:command add1)
              (process deadlock)))
    (check-pred process? (π 1))
    (check = 4 (π 3)))

  (test-case "bridge forwards unhandled commands to π2 when π1 fails"
    (define π
      (bridge (process deadlock)
              (start (process deadlock) #:command sub1)))
    (check-pred process? (π 2))
    (check = 2 (π 3)))

  (test-case "bridge raises unhandled-command when π1 and π2 both fail"
    (define π
      (bridge (process deadlock)
              (process deadlock)))
    (check-exn unhandled-command? (λ () (π 3))))

  (test-case "service in"
    (define svc (service))
    (define ch (make-channel))

    (for/list ([i 10])
      (define π (process (λ () (forever (channel-put ch (+ i (take)))))))
      (check-pred void? (svc `(add ,i ,π))))

    (for ([j 10])
      (process (λ () (give svc j j)))
      (check = (sync ch) (* 2 j)))

    (for ([j 10])
      (process (λ () (give svc j 10)))
      (check = (sync ch) (+ j 10)))

    (for ([i 10] #:when (even? i)) (check-true (svc `(drop ,i))))
    (for ([i 10] #:when (even? i)) (check-false (svc `(drop ,i))))

    (for ([j 10] #:when (odd? j))
      (process (λ () (give svc j j)))
      (check = (sync ch) (* 2 j)))

    (for ([j 10] #:when (odd? j))
      (process (λ () (give svc j 10)))
      (check = (sync ch) (+ j 10)))

    (for ([i 10] #:when (odd? i)) (check-true (svc `(drop ,i))))
    (for ([i 10] #:when (odd? i)) (check-false (svc `(drop ,i))))

    (check-pred null? (svc 'peers)))

  (test-case "service out"
    (define svc (service))
    (define ch (make-channel))
    (for ([i 10])
      (define π (process (λ () (forever (emit (channel-get ch))))))
      (svc `(add ,i ,π)))
    (for ([j 10])
      (thread (λ () (channel-put ch j)))
      (check equal? (sync (fmap list (recv-evt svc))) (list j j))))

  (test-case "service stop"
    (define svc (service #:on-drop (λ (_ π) (stop π))))
    (define ch (make-channel))
    (for ([i 10]) (svc `(add ,i ,(process deadlock))))
    (define πs (map cdr (svc 'peers)))
    (check-true (andmap alive? πs))
    (stop svc)
    (sync (async-void* πs)))

  (test-case "managed forwards non-eof values to and from π"
    (check = (call (managed (server add1)) 57) 58))

  (test-case "managed calls on-eof-in before eof is given"
    (define π (managed (server add1)))
    (shutdown π))

  (test-case "managed calls post-emit-eof after π emits eof"
    (define π (managed (process (λ () (emit eof) (deadlock)))))
    (recv π)
    (wait π))

  (test-case "managed stops π when it stops"
    (define stopped #f)
    (stop
     (managed
      (start (process deadlock) #:on-stop (λ () (set! stopped #t)))))
    (check-true stopped))

  (test-case "managed dies when π dies"
    (define π (process deadlock))
    (define π* (managed π))
    (kill π)
    (wait π*))

  (test-case "managed forwards commands to π"
    (define π (start (process deadlock) #:command add1))
    (check = ((managed π) 59) 60))

  (test-case "shutdown gives eof to π and blocks until it dies"
    (define π (process (λ () (check-true (eof-object? (take))))))
    (shutdown π))

  (test-case "shutdown-evt returns a synchronizable event"
    (check-pred evt? (shutdown-evt (process deadlock))))

  (test-case "shutdown-evt gives eof to π and syncs when π dies"
    (define π (process (λ () (check-pred eof-object? (take)))))
    (sync (shutdown-evt π))
    (check-pred dead? π))

  (test-case "shutdown-evt syncs to void"
    (check-pred void? (sync (shutdown-evt (managed (process deadlock)))))))

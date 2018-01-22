#lang racket/base

(require neuron/concurrency/process
         neuron/concurrency/ipc
         racket/contract/base
         racket/dict
         racket/function
         (only-in racket/list flatten make-list last))

(provide
 forever while until
 (contract-out
  [evt-set (-> evt? ... evt?)]
  [evt-sequence (-> (-> evt?) (-> evt?) ... evt?)]
  [evt-series (->* ((-> any/c evt?))
                   (#:init any/c)
                   #:rest (listof (-> any/c evt?))
                   evt?)]
  [evt-loop (->* ((-> any/c evt?)) (#:init any/c) evt?)]
  [server (->* ((-> any/c any/c))
               (#:on-stop (-> any)
                #:on-dead (-> any)
                #:command (or/c procedure? (listof procedure?)))
               process?)]
  [sink (->* ((-> any/c void?))
             (#:on-stop (-> any)
              #:on-dead (-> any)
              #:command (or/c procedure? (listof procedure?)))
             process?)]
  [source (->* ((-> any/c))
               (#:on-stop (-> any)
                #:on-dead (-> any)
                #:command (or/c procedure? (listof procedure?)))
               process?)]
  [socket (->* (process?
                process?)
               (#:on-stop (-> any)
                #:on-dead (-> any)
                #:command (or/c procedure? (listof procedure?)))
               process?)]
  [simulator (->* ((-> real? any))
                  (#:rate real?
                   #:on-stop (-> any)
                   #:on-dead (-> any)
                   #:command (or/c procedure? (listof procedure?)))
                  process?)]
  [proxy (->* (process?)
              (#:on-take (-> any/c any/c)
               #:on-emit (-> any/c any/c)
               #:on-stop (-> any)
               #:on-dead (-> any)
               #:command (or/c procedure? (listof procedure?)))
              process?)]
  [pipe (->* ()
             (#:on-stop (-> any)
              #:on-dead (-> any)
              #:command (or/c procedure? (listof procedure?)))
             #:rest (listof process?)
             process?)]
  [bridge (->* (process?
                process?)
               (#:on-stop (-> any)
                #:on-dead (-> any)
                #:command (or/c procedure? (listof procedure?)))
               process?)]
  [managed (->* (process?)
               (#:on-take-eof (-> any)
                #:on-emit-eof (-> any)
                #:on-stop (-> any)
                #:on-dead (-> any)
                #:command (or/c procedure? (listof procedure?)))
               process?)]
  [shutdown (-> process? void?)]))

;; Commands

(define-syntax-rule (forever body ...)
  (let loop () body ... (loop)))

(define-syntax-rule (while expr body ...)
  (let loop () (when expr body ... (loop))))

(define-syntax-rule (until expr body ...)
  (let loop () (unless expr body ... (loop))))

;; Events

(define (evt-set . evts)
  (define results null)
  (define (handle e)
    (handle-evt e (λ (v) (cons e v))))
  (define (recur es)
    (if (null? es)
        (handle-evt always-evt (λ _ (map (curry dict-ref results) evts)))
        (replace-evt (apply choice-evt (map handle es))
                     (λ (e+v)
                       (set! results (cons e+v results))
                       (recur (remq (car e+v) es))))))
  (recur evts))

(define (evt-sequence make-evt0 . make-evts)
  (define (next-evt make-evt evt)
    (replace-evt evt (λ _ (make-evt))))
  (foldl next-evt (make-evt0) make-evts))

(define (evt-series #:init [init (void)] make-evt0 . make-evts)
  (define (next-evt make-evt evt)
    (replace-evt evt (λ (v) (make-evt v))))
  (foldl next-evt (make-evt0 init) make-evts))

(define (evt-loop #:init [init (void)] next-evt)
  (replace-evt (next-evt init) (λ (v) (evt-loop #:init v next-evt))))

;; Processes

(define (server proc
                #:on-stop [on-stop void]
                #:on-dead [on-dead void]
                #:command [handler null])
  (start (λ () (forever (emit (proc (take)))))
         #:on-stop on-stop
         #:on-dead on-dead
         #:command handler))

(define (sink proc
              #:on-stop [on-stop void]
              #:on-dead [on-dead void]
              #:command [handler null])
  (start (λ () (forever (proc (take))))
         #:on-stop on-stop
         #:on-dead on-dead
         #:command handler))

(define (source proc
              #:on-stop [on-stop void]
              #:on-dead [on-dead void]
              #:command [handler null])
  (start (λ () (forever (emit (proc))))
         #:on-stop on-stop
         #:on-dead on-dead
         #:command handler))

(define (socket snk src
                #:on-stop [on-stop void]
                #:on-dead [on-dead void]
                #:command [handler null])
  (start
   (λ ()
     (sync (thread (λ () (forever (give snk (take)))))
           (thread (λ () (forever (emit (recv src)))))
           (handle-evt (choice-evt snk src) die)))
   #:on-stop (λ () (on-stop) (stop snk) (stop src))
   #:on-dead on-dead
   #:command (flatten (list handler
                            (λ vs
                              (cond [(equal? vs '(sink)) snk]
                                    [(equal? vs '(source)) src]
                                    [else (raise (unhandled-command vs))]))))))

(define (simulator proc
                   #:rate [rate 10]
                   #:on-stop [on-stop void]
                   #:on-dead [on-dead void]
                   #:command [handler null])
  (start
   (λ ()
     (define period (/ 1000.0 rate))
     (define timestamp (current-inexact-milliseconds))
     (forever
       (set! timestamp (+ timestamp period))
       (sync (alarm-evt timestamp))
       (proc period)))
   #:on-stop on-stop
   #:on-dead on-dead
   #:command handler))

(define (proxy π
               #:on-take [on-take values]
               #:on-emit [on-emit values]
               #:on-stop [on-stop void]
               #:on-dead [on-dead void]
               #:command [handler π])
  (start (λ () (sync (thread (λ () (forever (give π (on-take (take))))))
                     (thread (λ () (forever (emit (on-emit (recv π))))))
                     (handle-evt π die)))
         #:on-stop (λ () (on-stop) (stop π))
         #:on-dead on-dead
         #:command handler))

(define (pipe #:on-stop [on-stop void]
              #:on-dead [on-dead void]
              #:command [handler null]
              . πs)
  (start (λ () (sync (thread (λ () (forever (emit (foldl call (take) πs)))))
                     (handle-evt (apply choice-evt πs) die)))
         #:on-stop (λ () (on-stop) (for-each stop πs))
         #:on-dead on-dead
         #:command handler))

(define (bridge π1 π2
                #:on-stop [on-stop void]
                #:on-dead [on-dead void]
                #:command [handler null])
  (start
   (λ ()
     (sync
      (evt-loop (λ _ (evt-series (λ _ (recv-evt π1)) (curry give-evt π2))))
      (evt-loop (λ _ (evt-series (λ _ (recv-evt π2)) (curry give-evt π1))))
      (handle-evt (choice-evt π1 π2) die)))
   #:on-stop (λ () (on-stop) (stop π1) (stop π2))
   #:on-dead on-dead
   #:command handler))

(define (managed π
                 #:on-take-eof [on-take-eof stop]
                 #:on-emit-eof [on-emit-eof stop]
                 #:on-stop [on-stop void]
                 #:on-dead [on-dead void]
                 #:command [handler null])
  (define (loop v-in hook v-out)
    (define v (v-in))
    (if (eof-object? v) (hook π) (begin (v-out v) (loop v-in hook v-out))))
  (start (λ () (sync (thread (λ () (loop take on-take-eof (λ (v) (give π v)))))
                     (thread (λ () (loop (λ () (recv π)) on-emit-eof emit)))
                     (handle-evt π die)))
         #:on-stop (λ () (on-stop) (stop π))
         #:on-dead on-dead
         #:command handler))

(define (shutdown π)
  (give π eof)
  (wait π))

(module+ test
  (require rackunit
           racket/async-channel)

  ;; Commands

  (test-case
    "forever evaluates its body repeatedly."
    (define N 0)
    (define π (start (λ () (forever (set! N (+ N 1)) (when (> N 100) (die))))))
    (wait π)
    (check > N 100))

  (test-case
    "while evaluates its body for as long as expr evaluates to #t."
    (define count 0)
    (define π (start (λ () (while (<= count 100) (set! count (add1 count))))))
    (wait π)
    (check > count 100))

  (test-case
    "until evaluates its body for as long as expr evaluates to #f."
    (define count 0)
    (define π (start (λ () (until (> count 100) (set! count (add1 count))))))
    (wait π)
    (check > count 100))

  ;; Events

  (test-case
    "An evt-set is ready when every evt is ready."
    (define πs (for/list ([_ 10]) (start (λ () (take)))))
    (define evt (apply evt-set πs))
    (for-each give πs)
    (check-false (not (sync evt))))

  (test-case
    "An evt-set is not ready until every evt is ready."
    (define πs (for/list ([_ 10]) (start (λ () (take)))))
    (define evt (apply evt-set πs))
    (for ([π πs])
      (check-false (ormap (λ (π) (sync/timeout 0 π)) πs))
      (check-false (sync/timeout 0 evt)))
    (for-each give πs)
    (for ([π πs])
      (check-false (not (sync π))))
    (check-false (not (sync evt))))

  (test-case
    "An evt-set syncs to the list of results of evts."
    (define πs (for/list ([i 10]) (start (λ () (emit i)))))
    (define evt (apply evt-set (map recv-evt πs)))
    (check equal? (sync evt) '(0 1 2 3 4 5 6 7 8 9)))

  (test-case
    "An evt-sequence is ready when all generated events are ready."
    (check-false
     (not (sync (apply evt-sequence (make-list 10 (λ () (start void))))))))

  (test-case
    "An evt-sequence is not ready until all generated events are ready."
    (define πs (for/list ([_ 10]) (start emit)))
    (define evt (apply evt-sequence (map (λ (π) (λ () π)) πs)))
    (for ([π πs])
      (check-false (sync/timeout 0 π))
      (check-false (sync/timeout 0 evt))
      (recv π))
    (check-false (not (sync evt))))

  (test-case
    "An evt-sequence syncs on the results of make-evts in order."
    (define result null)
    (define (make-π i)
      (λ () (start (λ () (set! result (cons i result))))))
    (sync (apply evt-sequence (for/list ([i 10]) (make-π i))))
    (check equal? result '(9 8 7 6 5 4 3 2 1 0)))

  (test-case
    "An evt-sequence syncs to the same result as the last event generated."
    (define πs (for/list ([_ 10]) (start void)))
    (check eq? (sync (apply evt-sequence (map (λ (π) (λ () π)) πs))) (last πs)))

  (test-case
    "An evt-series is ready when all generated events are ready."
    (check-false
     (not (sync (apply evt-series (make-list 10 (λ _ (start void))))))))

  (test-case
    "An evt-series is not ready until all generated events are ready."
    (define πs (for/list ([_ 10]) (start emit)))
    (define evt (apply evt-series (map (λ (π) (λ _ π)) πs)))
    (for ([π πs])
      (check-false (sync/timeout 0 π))
      (check-false (sync/timeout 0 evt))
      (recv π))
    (check-false (not (sync evt))))

  (test-case
    "An evt-series syncs on the results of make-evts in order."
    (define result null)
    (define (make-π i)
      (λ _ (start (λ () (set! result (cons i result))))))
    (sync (apply evt-series (for/list ([i 10]) (make-π i))))
    (check equal? result '(9 8 7 6 5 4 3 2 1 0)))

  (test-case
    "An evt-series syncs to the same result as the last event generated."
    (define πs (for/list ([_ 10]) (start void)))
    (check eq? (sync (apply evt-series (map (λ (π) (λ _ π)) πs))) (last πs)))

  (test-case
    "An evt-series calls make-evt on the result of the previous event."
    (define (make-evt)
      (λ (i) (handle-evt always-evt (λ _ (+ i 1)))))
    (check = (sync (apply evt-series #:init 0 (for/list ([i 10]) (make-evt))))
           10))

  (test-case
    "An evt-loop repeatedly syncs on the result of next-evt."
    (define (next-evt i)
      (handle-evt always-evt (λ _ (if (<= i 100) (+ i 1) (raise 19)))))
    (check = (with-handlers ([number? (λ (v) v)])
               (sync (evt-loop #:init 0 next-evt)))
           19))

  ;; Processes

  (test-case
    "A server applies proc and emits the result."
    (define π (server add1))
    (give π 23)
    (check = (recv π) 24))

  (test-case
    "A sink applies proc to each value taken."
    (define last -1)
    (define π (sink (λ (n) (check = n (+ last 1)) (set! last n))))
    (for ([i 10]) (give π i)))

  (test-case
    "A sink ignores the result of proc."
    (define π (sink add1))
    (give π 31)
    (check-false (sync/timeout 0 (recv-evt π))))

  (test-case
    "A source applies proc repeatedly and emits each result."
    (define N -1)
    (define π (source (λ () (set! N (+ N 1)) N)))
    (for ([i 10]) (check = (recv π) i)))

  (test-case
    "A socket forwards to snk."
    (define result-ch (make-channel))
    (define π (socket (sink (curry channel-put result-ch)) (source void)))
    (for ([i 10])
      (give π i)
      (check = (channel-get result-ch) i)))

  (test-case
    "A socket forwards from src."
    (define π (socket (sink void) (source random)))
    (for ([_ 10])
      (define v (recv π))
      (check >= v 0)
      (check <= v 1)))

  (test-case
    "A socket stops snk and src when it stops."
    (define ch (make-async-channel))
    (define π (socket
               (sink deadlock #:on-stop (λ () (async-channel-put ch #t)))
               (source deadlock #:on-stop (λ () (async-channel-put ch #t)))))
    (stop π)
    (check-true (async-channel-get ch))
    (check-true (async-channel-get ch)))

  (test-case
    "A socket dies when snk dies."
    (define snk (sink deadlock))
    (define sock (socket snk (source deadlock)))
    (kill snk)
    (wait sock)
    (check-true (dead? sock)))

  (test-case
    "A socket dies when src dies."
    (define src (source deadlock))
    (define sock (socket (sink deadlock) src))
    (kill src)
    (wait sock)
    (check-true (dead? sock)))

  (test-case
    "The socket command 'sink returns snk."
    (define snk (sink deadlock))
    (define sock (socket snk (source deadlock)))
    (check eq? (sock 'sink) snk))

  (test-case
    "The socket command 'source returns src."
    (define src (source deadlock))
    (define sock (socket (sink deadlock) src))
    (check eq? (sock 'source) src))

  (test-case
    "A simulator calls proc at a frequency of rate."
    (define N 0)
    (define t0 (current-inexact-milliseconds))
    (wait (simulator (λ _ (set! N (+ N 1)) (when (= N 10) (die))) #:rate 100))
    (define t10 (current-inexact-milliseconds))
    (check = N 10)
    (check >= (- t10 t0) 100))

  (test-case
    "A proxy calls on-take and forwards to π."
    (define π (proxy (server (λ (x) (* x 2))) #:on-take add1))
    (give π 37)
    (check = (recv π) 76))

  (test-case
    "A proxy forwards from π and calls on-emit."
    (define π (proxy (server (λ (x) (* x 2))) #:on-emit sub1))
    (give π 43)
    (check = (recv π) 85))

  (test-case
    "A proxy stops π when it stops."
    (define π (start deadlock))
    (stop (proxy π))
    (check-true (dead? π)))

  (test-case
    "A proxy dies when π dies."
    (define π (start deadlock))
    (define π* (proxy π))
    (kill π)
    (wait π*)
    (check-true (dead? π*)))

  (test-case
    "A pipe calls πs in series."
    (define π (apply pipe (for/list ([_ 10]) (server add1))))
    (give π 49)
    (check = (recv π) 59))

  (test-case
    "A pipe stops all πs when it stops."
    (define πs (for/list ([_ 10]) (start deadlock)))
    (stop (apply pipe πs))
    (for ([π πs]) (check-true (dead? π))))

  (test-case
    "A pipe dies when any π dies."
    (for ([i 3])
      (define πs (for/list ([_ 3]) (start deadlock)))
      (define p (apply pipe πs))
      (kill (list-ref πs i))
      (wait p)
      (check-true (dead? p))))

  (test-case
    "A bridge forwards from π1 to π2, and vice versa."
    (wait (bridge (start (λ () (emit 51) (check = (take) 53)))
                  (start (λ () (emit 53) (check = (take) 51))))))

  (test-case
    "A bridge stops π1 and π2 when it stops."
    (define π1 (start deadlock))
    (define π2 (start deadlock))
    (stop (bridge π1 π2))
    (check-true (dead? π1))
    (check-true (dead? π2)))

  (test-case
    "A bridge dies when π1 dies."
    (define π1 (start deadlock))
    (define π (bridge π1 (start deadlock)))
    (kill π1)
    (wait π)
    (check-true (dead? π)))

  (test-case
    "A bridge dies when π2 dies."
    (define π2 (start deadlock))
    (define π (bridge (start deadlock) π2))
    (kill π2)
    (wait π)
    (check-true (dead? π)))

  (test-case
    "A managed process forwards non-eof values to and from π."
    (define π (managed (server add1)))
    (check = (call π 57) 58))

  (test-case
    "A managed process calls on-take-eof when eof is given."
    (define π (managed (server add1)))
    (give π eof)
    (wait π)
    (check-true (dead? π)))

  (test-case
    "A managed process calls on-emit-eof when π emits eof."
    (define π (managed (start (λ () (emit 59) (emit eof) (deadlock)))))
    (check = (recv π) 59)
    (wait π)
    (check-true (dead? π)))

  (test-case
    "A managed process stops π when it stops."
    (define stopped #f)
    (define π (start deadlock #:on-stop (λ () (set! stopped #t))))
    (stop (managed (start deadlock #:on-stop (λ () (set! stopped #t)))))
    (check-true stopped))

  (test-case
    "A managed process dies when π dies."
    (define π (start deadlock))
    (define π* (managed π))
    (kill π)
    (wait π*)
    (check-pred dead? π*))

  (test-case
    "shutdown gives eof to π and blocks until it dies."
    (define π (start (λ () (check-true (eof-object? (take))))))
    (shutdown π)
    (check-true (dead? π))))

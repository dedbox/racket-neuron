#lang racket/base

(require neuron/concurrency
         racket/contract/base
         racket/function
         (prefix-in list: racket/list)
         racket/tcp)

(provide
 (contract-out 
  [tcp-codec (-> (-> input-port? output-port? process?) input-port? output-port?
                 process?)]
  [tcp-client (->* ((-> input-port? output-port? process?) string? port-number?)
                   ((or/c string? #f)
                    (or/c port-number? #f))
                   process?)]
  [tcp-source (->* ((-> input-port? output-port? process?) listen-port-number?)
                   (exact-nonnegative-integer?
                    any/c
                    (or/c string? #f))
                   process?)]
  [tcp-server (->* ((-> any/c any/c)
                    (-> input-port? output-port? process?)
                    listen-port-number?)
                   (exact-nonnegative-integer?
                    any/c
                    (or/c string? #f))
                   process?)]
  [tcp-service (->* ((-> any/c any/c)
                     (-> input-port? output-port? process?)
                     listen-port-number?)
                    (exact-nonnegative-integer?
                     any/c
                     (or/c string? #f))
                    process?)]))

(define (tcp-codec make-codec in-port out-port)
  (define addr (apply-values list (tcp-addresses in-port #t)))
  (define cdc (make-codec in-port out-port))
  (start (process (λ ()
                    (sync (thread (λ () (forever (give cdc (take)))))
                          (thread (λ () (forever (emit (recv cdc)))))
                          (cdc 'decoder)
                          (cdc 'encoder))))
         #:on-stop (λ () (stop cdc))
         #:on-dead (λ () (kill cdc))
         #:command (λ vs
                     (cond [(equal? vs '(address)) addr]
                           [(equal? vs '(local-address)) (list:take addr 2)]
                           [(equal? vs '(remote-address)) (list:drop addr 2)]
                           [else (apply cdc vs)]))))

(define (tcp-client make-codec hostname port-no
                    [local-hostname #f]
                    [local-port-no #f])
  (apply-values (curry tcp-codec make-codec)
                (tcp-connect hostname port-no local-hostname local-port-no)))

(define (tcp-source make-codec port-no
                    [max-allow-wait 4]
                    [reuse? #f]
                    [hostname #f])
  (define listener (tcp-listen port-no max-allow-wait reuse? hostname))
  (define addr (apply-values list (tcp-addresses listener #t)))
  (start
   (source
    (λ () (apply-values (curry tcp-codec make-codec) (tcp-accept listener))))
   #:on-dead (λ () (tcp-close listener))
   #:command (λ vs
               (cond [(equal? vs '(listen-address)) addr]
                     [else unhandled]))))

(define (tcp-server proc make-codec port-no
                    [max-allow-wait 4]
                    [reuse? #f]
                    [hostname #f])
  (define src (tcp-source make-codec port-no max-allow-wait reuse? hostname))
  (start (source (λ () (emit (bridge (recv src) (server proc)))))
         #:on-stop (λ () (stop src))
         #:on-dead (λ () (kill src))
         #:command (λ vs (apply src vs))))

(define (tcp-service proc make-codec port-no
                     [max-allow-wait 4]
                     [reuse? #f]
                     [hostname #f])
  (define svc (service (λ (π) (π 'address))))
  (define srv
    (tcp-server proc make-codec port-no max-allow-wait reuse? hostname))
  (start (process (λ () (forever (give svc (recv srv)) (recv svc))))
         #:on-dead (λ () (kill srv) (kill svc))
         #:command (λ vs
                     (cond [(equal? vs '(peers)) (svc 'keys)]
                           [(or (null? vs)
                                (null? (cdr vs))
                                (not (null? (cddr vs)))) unhandled]
                           [(equal? (car vs) 'drop) (svc 'drop (cadr vs))]
                           [else unhandled]))))

(module+ test
  (require rackunit
           neuron/concurrency
           neuron/data-flow/codecs)

  (test-case
    "A tcp-codec applies make-codec to in-port and out-port."
    (define π
      (process
       (λ ()
         (define tested? #f)
         (define (check-ports port1 port2)
           (set! tested? #t)
           (check-true (input-port? port1))
           (check-true (output-port? port2)))
         (define listener (tcp-listen 0 4 #t #f))
         (emit (cadr (apply-values list (tcp-addresses listener #t))))
         (define cdc (apply-values (curry tcp-codec check-ports)
                                   (tcp-accept listener)))
         (check-true tested?)
         (emit))))
    (tcp-connect "127.0.0.1" (recv π))
    (recv π))

  (test-case
    "A tcp-codec exchanges values over a TCP connection."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (cadr (apply-values list (tcp-addresses listener #t))))
         (define srv (apply-values (curry tcp-codec line-codec)
                                   (tcp-accept listener)))
         (check equal? (recv srv) "123")
         (give srv 'abc))))
    (define cli (apply-values (curry tcp-codec line-codec)
                              (tcp-connect "127.0.0.1" (recv π))))
    (give cli 123)
    (check equal? (recv cli) "abc"))

  (test-case
    "A tcp-codec dies when either side of the connection closes."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (apply-values (λ vs (cadr vs)) (tcp-addresses listener #t)))
         (define cdc (apply-values (curry tcp-codec sexp-codec)
                                   (tcp-accept listener)))
         (emit (cdc 'address)))))
    (define port-no (recv π))
    (define cli1 (apply-values (curry tcp-codec sexp-codec)
                               (tcp-connect "127.0.0.1" port-no)))
    (close-input-port ((cli1 'decoder) 'input-port))
    (sync cli1)
    (check-pred dead? cli1)
    (define cli2 (apply-values (curry tcp-codec sexp-codec)
                               (tcp-connect "127.0.0.1" port-no)))
    (close-output-port ((cli2 'encoder) 'output-port))
    (sync cli2)
    (check-pred dead? cli2))

  (test-case
    "tcp-codec command 'address returns the full address."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (apply-values (λ vs (cadr vs)) (tcp-addresses listener #t)))
         (define cdc (apply-values (curry tcp-codec sexp-codec)
                                   (tcp-accept listener)))
         (emit (cdc 'address)))))
    (define-values (in-port out-port) (tcp-connect "127.0.0.1" (recv π)))
    (define-values (cli-local-host
                    cli-local-port
                    cli-remote-host
                    cli-remote-port)
      (tcp-addresses in-port #t))
    (check equal? (recv π)
           (list cli-remote-host
                 cli-remote-port
                 cli-local-host
                 cli-local-port)))

  (test-case
    "tcp-codec command 'local-address returns the local address."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (cadr (apply-values list (tcp-addresses listener #t))))
         (define cdc (apply-values (curry tcp-codec sexp-codec)
                                   (tcp-accept listener)))
         (emit (cdc 'local-address)))))
    (define-values (in-port out-port) (tcp-connect "127.0.0.1" (recv π)))
    (check equal? (recv π)
           (apply-values (λ vs (list:drop vs 2)) (tcp-addresses in-port #t))))

  (test-case
    "tcp-codec command 'remote-address returns the remote address."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (cadr (apply-values list (tcp-addresses listener #t))))
         (define cdc (apply-values (curry tcp-codec sexp-codec)
                                   (tcp-accept listener)))
         (emit (cdc 'remote-address)))))
    (define-values (in-port out-port) (tcp-connect "127.0.0.1" (recv π)))
    (check equal? (recv π)
           (apply-values (λ vs (list:take vs 2)) (tcp-addresses in-port #t))))

  (test-case
    "A tcp-client connects to ``hostname:port-no''."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (cadr (apply-values list (tcp-addresses listener #t))))
         (define-values (in-port out-port) (tcp-accept listener))
         (check equal? (read-line in-port) "123")
         (displayln 'abc out-port)
         (flush-output out-port))))
    (define cli (tcp-client line-codec "localhost" (recv π)))
    (give cli 123)
    (check equal? (recv cli) "abc"))

  (test-case
    "A tcp-client calls make-codec."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (cadr (apply-values list (tcp-addresses listener #t))))
         (define-values (in-port out-port) (tcp-accept listener))
         (emit))))
    (define tested? #f)
    (define cli (tcp-client (λ (in-port out-port)
                              (set! tested? #t)
                              (check-true (input-port? in-port))
                              (check-true (output-port? out-port)))
                            "localhost"
                            (recv π)))
    (recv π)
    (check-true tested?))

  (test-case
    "A tcp-client returns a tcp-codec."
    (define π
      (process
       (λ ()
         (define listener (tcp-listen 0 4 #t #f))
         (emit (cadr (apply-values list (tcp-addresses listener #t))))
         (define-values (in-port out-port) (tcp-accept listener))
         (check equal? (read-line in-port) "123")
         (displayln 'abc out-port)
         (flush-output out-port))))
    (define cli (tcp-client sexp-codec "localhost" (recv π)))
    (give cli 123)
    (check equal? (recv cli) 'abc))

  (test-case
    "A tcp-source emits a tcp-codec for each accepted connection."
    (define src (tcp-source sexp-codec 0 4 #t #f))
    (define cli
      (tcp-client sexp-codec "localhost" (cadr (src 'listen-address))))
    (define π (recv src))
    (check-true (process? π))
    (check-true (process? (π 'decoder)))
    (check-true (process? (π 'encoder))))

  (test-case
    "tcp-source command 'listen-address returns the address of the listener."
    (define src (tcp-source sexp-codec 3600 4 #t #f))
    (check equal? (cadr (src 'listen-address)) 3600)
    (kill src))

  (test-case
    "A tcp-server emits a tcp-codec--server bridge."
    (define srv (tcp-server add1 sexp-codec 0 4 #t #f))
    (define cli
      (tcp-client sexp-codec "localhost" (cadr (srv 'listen-address))))
    (define π (recv srv))
    (check-true (process? π))
    (check-true (process? (π 'decoder)))
    (check-true (process? (π 'encoder)))
    (check equal? (π 'local-address) (cli 'remote-address))
    (check equal? (π 'remote-address) (cli 'local-address))
    (for ([i 10]) (check = (call cli i) (+ i 1))))

  )

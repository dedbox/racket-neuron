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
  (define addr (call-with-values (λ () (tcp-addresses in-port #t)) list))
  (start (make-codec in-port out-port)
         #:command (λ vs
                     (cond [(equal? vs '(address)) addr]
                           [(equal? vs '(local-address)) (list:take addr 2)]
                           [(equal? vs '(remote-address)) (list:drop addr 2)]
                           [else unhandled]))))

(define (tcp-client make-codec hostname port-no
                    [local-hostname #f]
                    [local-port-no #f])
  (call-with-values
      (λ () (tcp-connect hostname port-no local-hostname local-port-no))
    (curry tcp-codec make-codec)))

(define (tcp-source make-codec port-no
                    [max-allow-wait 4]
                    [reuse? #f]
                    [hostname #f])
  (define listener (tcp-listen port-no max-allow-wait reuse? hostname))
  (start
   (source (λ () (call-with-values (λ () (tcp-accept listener)) make-codec)))
   #:on-dead (λ () (tcp-close listener))))

(define (tcp-server proc make-codec port-no
                    [max-allow-wait 4]
                    [reuse? #f]
                    [hostname #f])
  (define src (tcp-source make-codec port-no max-allow-wait reuse? hostname))
  (start (source (λ () (emit (bridge (recv src) (server proc)))))
         #:on-dead (λ () (kill src))))

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
    (define port-no (recv π))
    (define-values (in-port out-port) (tcp-connect "127.0.0.1" port-no))
    (recv π))

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
    (define port-no (recv π))
    (define-values (in-port out-port) (tcp-connect "127.0.0.1" port-no))
    (define srv-addr (recv π))
    (define-values (cli-local-host
                    cli-local-port
                    cli-remote-host
                    cli-remote-port)
      (tcp-addresses in-port #t))
    (check equal? srv-addr
           (list cli-remote-host cli-remote-port cli-local-host cli-local-port)))

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
    (define port-no (recv π))
    (define-values (in-port out-port) (tcp-connect "127.0.0.1" port-no))
    (check equal? (recv π) (apply-values (compose (curryr list:drop 2) list)
                                         (tcp-addresses in-port #t))))

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
    (define port-no (recv π))
    (define-values (in-port out-port) (tcp-connect "127.0.0.1" port-no))
    (check equal? (recv π) (apply-values (compose (curryr list:take 2) list)
                                         (tcp-addresses in-port #t)))))

#lang racket/base

(require
 event
 neuron/codec
 neuron/evaluation
 neuron/process
 neuron/process/control
 neuron/process/messaging
 neuron/socket
 neuron/syntax
 racket/contract/base
 racket/function
 racket/tcp)

(provide
 (contract-out
  [tcp-socket? predicate/c]
  [tcp-socket (-> input-port? output-port? tcp-socket?)]
  [tcp-socket-address (-> tcp-socket?
                          (list/c string? port-number?
                                  string? port-number?))]
  [tcp-client (->* (string? port-number?)
                   ((or/c string? #f) (or/c port-number? #f))
                   socket?)]
  [tcp-server (->* (listen-port-number?)
                   (exact-nonnegative-integer? any/c (or/c string? #f))
                   process?)]
  [tcp-service (->* (codec/c listen-port-number?)
                    (exact-nonnegative-integer?
                     any/c
                     (or/c string? #f)
                     #:on-accept (-> any/c process? any)
                     #:on-drop (-> any/c process? any))
                    process?)]))

(struct tcp-socket socket (address)
        #:omit-define-syntaxes
        #:constructor-name make-tcp-socket)

(define (tcp-socket in-port out-port)
  (make-tcp-socket in-port out-port
                   (apply-values list (tcp-addresses in-port #t))))

(define (tcp-client hostname port-no [local-hostname #f] [local-port-no #f])
  (apply-values tcp-socket (tcp-connect hostname port-no
                                        local-hostname local-port-no)))

(define (tcp-server port-no [max-allow-wait 4] [reuse? #f] [hostname #f])
  (define listener (tcp-listen port-no max-allow-wait reuse? hostname))
  (define addr (apply-values list (tcp-addresses listener #t)))
  (start (source (λ () (apply-values tcp-socket (tcp-accept listener))))
         #:on-dead (λ () (tcp-close listener))
         #:command (bindings [(address) addr] #:else unhandled)))

(define (tcp-service make-codec port-no
                     [max-allow-wait 4] [reuse? #f] [hostname #f]
                     #:on-accept [on-accept void]
                     #:on-drop [on-drop void])
  (define srv (tcp-server port-no max-allow-wait reuse? hostname))
  (define svc (service #:on-drop on-drop))
  (start
   (process
    (λ ()
      (define peer-connect-evt
        (handle-evt
         (recv-evt srv)
         (λ (sock)
           (define π (make-codec sock))
           (define addr (tcp-socket-address sock))
           (wait (process (λ () (svc 'add addr π))))
           (on-accept addr π))))
      (sync
       (loop (λ _ peer-connect-evt))
       (loop (λ _ (forward-to-evt svc)))
       (loop (λ _ (forward-from-evt svc))))))
   #:on-stop (λ () (stop svc) (stop srv))
   #:on-dead (λ () (kill svc) (kill srv))
   #:command (list (bindings [(server) srv] [(service) svc] #:else unhandled)
                   (curry command srv)
                   (curry command svc))))

(module+ test
  (require rackunit
           (prefix-in list: racket/list))

  (test-case "A tcp-socket is a socket with a TCP address."
    (define listener (tcp-listen 0 4 #t #f))
    (define port-no (cadr (apply-values list (tcp-addresses listener #t))))
    (define sock (apply-values tcp-socket (tcp-connect "localhost" port-no)))
    (check-pred socket? sock)
    (check-pred (list/c string? port-number?
                        string? port-number?)
                (tcp-socket-address sock)))

  (test-case "A tcp-client returns a TCP socket connected to hostname:port-no."
    (define listener (tcp-listen 0 4 #t #f))
    (define port-no (cadr (apply-values list (tcp-addresses listener #t))))
    (define sock (tcp-client "localhost" port-no))
    (check-pred tcp-socket? sock)
    (define peer (apply-values tcp-socket (tcp-accept listener)))
    (define sock-addr (tcp-socket-address sock))
    (define peer-addr (tcp-socket-address peer))
    (check equal? (list:take sock-addr 2) (list:drop peer-addr 2))
    (check equal? (list:drop sock-addr 2) (list:take peer-addr 2)))

  (test-case "A tcp-server emits TCP sockets."
    (define srv (tcp-server 0 4 #t #f))
    (define port-no (cadr (srv 'address)))
    (define sock (tcp-client "localhost" port-no))
    (check-pred tcp-socket? (recv srv)))

  (test-case "A tcp-server listens for TCP connections on hostname:port-no."
    (define srv (tcp-server 0 4 #t #f))
    (define port-no (cadr (srv 'address)))
    (define sock (tcp-client "localhost" port-no))
    (define peer (recv srv))
    (define sock-addr (tcp-socket-address sock))
    (define peer-addr (tcp-socket-address peer))
    (check equal? (list:take sock-addr 2) (list:drop peer-addr 2))
    (check equal? (list:drop sock-addr 2) (list:take peer-addr 2)))

  (test-case "tcp-server command 'address returns a TCP address."
    (define srv (tcp-server 0 4 #t #f))
    (check-pred (list/c string? port-number?
                        string? listen-port-number?)
                (srv 'address)))

  (test-case "tcp-service"
    (define svc (tcp-service sexp-codec 0 4 #t #f))
    (define svc-port (cadr (svc 'address)))
    (define cli (sexp-codec (tcp-client "localhost" svc-port)))
    (define cli-port (cadr (tcp-socket-address (cli 'socket))))
    (check-true (give cli 'x))
    (define-values (peer-addr msg) (recv svc))
    (check-pred (list/c string? port-number?
                        string? listen-port-number?)
                peer-addr)
    (check = (cadr peer-addr) svc-port)
    (check = (cadddr peer-addr) cli-port)
    (check-eq? msg 'x)
    (check-true (give svc peer-addr 'y))
    (check-eq? (recv cli) 'y)
    (stop svc)
    (stop cli)))

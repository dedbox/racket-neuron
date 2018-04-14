#lang racket/base

(require
 neuron/data-flow/codec
 neuron/data-flow/socket
 neuron/evaluation
 neuron/event
 neuron/process
 neuron/process/control
 neuron/process/messaging
 neuron/syntax
 racket/contract/base
 racket/function
 racket/tcp)

(provide
 (contract-out
  [tcp-socket? predicate/c]
  [tcp-socket (-> input-port? output-port? tcp-socket?)]
  [tcp-socket-address
   (-> tcp-socket?
       (list/c string? port-number?
               string? port-number?))]
  [tcp-client
   (->* (string? port-number?)
        ((or/c string? #f)
         (or/c port-number? #f))
        socket?)]
  [tcp-server
   (->* (listen-port-number?)
        (exact-nonnegative-integer? any/c (or/c string? #f))
        process?)]
  [tcp-service
   (->* (codec/c process?)
        (#:on-accept (-> any/c process? any)
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
         #:command (bind ([address addr])
                         #:else unhandled)))

(define (tcp-service make-codec srv
                     #:on-accept [on-accept void]
                     #:on-drop [on-drop void])
  (define svc
    (service (λ (π) (tcp-socket-address (π 'socket))) #:on-drop on-drop))
  (start
   (process
    (λ ()
      (define peer-connect-evt
        (handle-evt
         (recv-evt srv)
         (λ (sock)
           (define π (make-codec sock))
           (define addr (svc 'add π))
           (on-accept addr π))))
      (define peer-take-evt
        (replace-evt (take-evt) (curry give-evt svc)))
      (define peer-emit-evt
        (replace-evt (recv-evt svc) emit-evt))
      (sync
       (evt-loop (λ _ peer-connect-evt))
       (evt-loop (λ _ peer-take-evt))
       (evt-loop (λ _ peer-emit-evt)))))
   #:on-stop (λ () (stop svc))
   #:on-dead (λ () (kill svc))
   #:command (bind ([peers (svc 'peers)]
                    [(get ,addr) ((svc 'get) addr)]
                    [(drop ,addr) ((svc 'drop) addr)])
                   #:else unhandled)))

(module+ test
  (require rackunit
           (prefix-in list: racket/list))

  (test-case
    "A tcp-socket is a socket with a TCP address."
    (define listener (tcp-listen 0 4 #t #f))
    (define port-no (cadr (apply-values list (tcp-addresses listener #t))))
    (define sock (apply-values tcp-socket (tcp-connect "localhost" port-no)))
    (check-pred socket? sock)
    (check-pred (list/c string? port-number?
                        string? port-number?)
                (tcp-socket-address sock)))

  (test-case
    "A tcp-client returns a TCP socket connected to hostname:port-no."
    (define listener (tcp-listen 0 4 #t #f))
    (define port-no (cadr (apply-values list (tcp-addresses listener #t))))
    (define sock (tcp-client "localhost" port-no))
    (check-pred tcp-socket? sock)
    (define peer (apply-values tcp-socket (tcp-accept listener)))
    (define sock-addr (tcp-socket-address sock))
    (define peer-addr (tcp-socket-address peer))
    (check equal? (list:take sock-addr 2) (list:drop peer-addr 2))
    (check equal? (list:drop sock-addr 2) (list:take peer-addr 2)))

  (test-case
    "A tcp-server emits TCP sockets."
    (define srv (tcp-server 0 4 #t #f))
    (define port-no (cadr (srv 'address)))
    (define sock (tcp-client "localhost" port-no))
    (check-pred tcp-socket? (recv srv)))

  (test-case
    "A tcp-server listens for TCP connections on hostname:port-no."
    (define srv (tcp-server 0 4 #t #f))
    (define port-no (cadr (srv 'address)))
    (define sock (tcp-client "localhost" port-no))
    (define peer (recv srv))
    (define sock-addr (tcp-socket-address sock))
    (define peer-addr (tcp-socket-address peer))
    (check equal? (list:take sock-addr 2) (list:drop peer-addr 2))
    (check equal? (list:drop sock-addr 2) (list:take peer-addr 2)))

  (test-case
    "tcp-server command 'address returns a TCP address."
    (define srv (tcp-server 0 4 #t #f))
    (check-pred (list/c string? port-number?
                        string? listen-port-number?)
                (srv 'address))))

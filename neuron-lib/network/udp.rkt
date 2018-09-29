#lang racket/base

(require
 neuron/codec
 neuron/evaluation
 neuron/process
 neuron/process/messaging
 neuron/socket
 neuron/syntax
 racket/contract/base
 racket/function
 racket/tcp
 racket/udp)

(provide
 (contract-out
  [udp-source (->* (parser/c) (string? port-number?) process?)]
  [udp-sink (-> printer/c string? listen-port-number? process?)]
  [udp-sink-to (->* (printer/c) (string? port-number?) process?)]))

(define (udp-source prs [hostname "::"] [port-no 0])
  (define udp-sock (udp-open-socket hostname (if (= port-no 0) #f port-no)))
  (udp-bind! udp-sock hostname port-no)
  (define addr (apply-values list (udp-addresses udp-sock #t)))
  (start
   (process
    (λ ()
      (define buf (make-bytes #xFFFF))
      (forever
       (define-values (len host port) (udp-receive! udp-sock buf))
       (define in (string-socket #:in (bytes->string/utf-8 buf #f 0 len)))
       (let loop ()
         (define v (prs in))
         (unless (eof-object? v) (emit host port v) (loop)))
       (close-input-port in))))
   #:on-dead (λ () (udp-close udp-sock))
   #:command (bindings [(address) addr] #:else unhandled)))

(define (udp-sink prn hostname port-no)
  (define udp-sock (udp-open-socket hostname port-no))
  (udp-connect! udp-sock hostname port-no)
  (define addr (apply-values list (udp-addresses udp-sock #t)))
  (start
   (process
    (λ ()
      (forever
       (define vs (apply-values list (take)))
       (define out (string-socket #:out #t))
       (for ([v vs]) (prn v out))
       (udp-send* udp-sock (get-output-bytes out #t)))))
   #:on-dead (λ () (udp-close udp-sock))
   #:command (bindings [(address) addr] #:else unhandled)))

(define (udp-sink-to prn [local-hostname "::"] [local-port-no 0])
  (define udp-sock
    (udp-open-socket local-hostname (if (= local-port-no 0) #f local-port-no)))
  (udp-bind! udp-sock local-hostname local-port-no)
  (define addr (apply-values list (udp-addresses udp-sock #t)))
  (start
   (process
    (λ ()
      (define out (open-output-bytes))
      (forever
       (define vs (apply-values list (take)))
       (for-each (curryr prn out) (cddr vs))
       (udp-send-to udp-sock (car vs) (cadr vs) (get-output-bytes out #t)))))
   #:on-dead (λ () (udp-close udp-sock))
   #:command (bindings [(address) addr] #:else unhandled)))

(module+ test
  (require rackunit)

  (test-case "udp-source + udp-sink (ipv6)"
    (define src (udp-source read))
    (define src-port (cadr (src 'address)))
    (define snk (udp-sink writeln "localhost" src-port))
    (define snk-host (car (snk 'address)))
    (define snk-port (cadr (snk 'address)))
    (check-true (give snk 123 'abc 987 'zyx))
    (check-equal? (cdr (apply-values list (recv src))) (list snk-port 123))
    (check-equal? (cdr (apply-values list (recv src))) (list snk-port 'abc))
    (check-equal? (cdr (apply-values list (recv src))) (list snk-port 987))
    (check-equal? (cdr (apply-values list (recv src))) (list snk-port 'zyx)))

  (test-case "udp-source + udp-sink (ipv4)"
    (define src (udp-source read "0.0.0.0"))
    (define src-port (cadr (src 'address)))
    (define snk (udp-sink writeln "127.0.0.1" src-port))
    (define snk-port (cadr (snk 'address)))
    (check-true (give snk 123 'abc 987 'zyx))
    (check-equal? (apply-values list (recv src)) `("127.0.0.1" ,snk-port 123))
    (check-equal? (apply-values list (recv src)) `("127.0.0.1" ,snk-port abc))
    (check-equal? (apply-values list (recv src)) `("127.0.0.1" ,snk-port 987))
    (check-equal? (apply-values list (recv src)) `("127.0.0.1" ,snk-port zyx)))

  (test-case "udp-source + udp-sink-to (ipv6)"
    (define srcs
      (for/hash ([_ 10])
        (define src (udp-source read))
        (values (cadr (src 'address)) src)))
    (define snk (udp-sink-to writeln))
    (define snk-port (cadr (snk 'address)))
    (define src-index
      (for/hash ([i 10]
                 [(src-port _) (in-hash srcs)])
        (check-true (give snk "localhost" src-port i i))
        (values i src-port)))
    (for ([j 10])
      (define src-port (hash-ref src-index j))
      (define src (hash-ref srcs src-port))
      (check-equal? (cdr (apply-values list (recv src))) (list snk-port j))
      (check-equal? (cdr (apply-values list (recv src))) (list snk-port j))))

  (test-case "udp-source + udp-sink-to (ipv4)"
    (define srcs
      (for/hash ([_ 10])
        (define src (udp-source read "0.0.0.0"))
        (values (cadr (src 'address)) src)))
    (define snk (udp-sink-to writeln))
    (define snk-port (cadr (snk 'address)))
    (define src-index
      (for/hash ([i 10]
                 [(src-port _) (in-hash srcs)])
        (check-true (give snk "127.0.0.1" src-port i i))
        (values i src-port)))
    (for ([j 10])
      (define src-port (hash-ref src-index j))
      (define src (hash-ref srcs src-port))
      (check-equal? (apply-values list (recv src)) `("127.0.0.1" ,snk-port ,j))
      (check-equal?
       (apply-values list (recv src)) `("127.0.0.1" ,snk-port ,j)))))

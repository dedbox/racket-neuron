#lang racket/base

(require
 neuron/concurrency
 neuron/data-flow/socket
 json
 racket/contract/base
 racket/splicing)

(require
 (for-syntax racket/base
             racket/syntax))

(provide
 define-codec
 (contract-out
  [parser/c contract?]
  [printer/c contract?]
  [codec/c contract?]
  [flushed (-> printer/c printer/c)]
  [decoder (-> parser/c (-> socket? process?))]
  [encoder (-> printer/c (-> socket? process?))]
  [codec (-> parser/c printer/c (-> socket? process?))]
  [make-codec-type
   (-> symbol? parser/c printer/c
       (values (-> input-port? process?)
               (-> output-port? process?)
               (-> input-port? output-port? process?)))]
  [line-parser parser/c]
  [line-printer printer/c]
  [line-decoder codec/c]
  [line-encoder codec/c]
  [line-codec codec/c]
  [sexp-parser parser/c]
  [sexp-printer printer/c]
  [sexp-decoder codec/c]
  [sexp-encoder codec/c]
  [sexp-codec codec/c]
  [json-parser parser/c]
  [json-printer printer/c]
  [json-decoder codec/c]
  [json-encoder codec/c]
  [json-codec codec/c]))

(define parser/c (-> socket? any/c))
(define printer/c (-> any/c socket? any))
(define codec/c (-> socket? process?))

(define (flushed prn)
  (λ (v sock)
    (prn v sock)
    (flush-output sock)))

(define (decoder prs)
  (λ (sock)
    (start
     (managed (process
               (λ ()
                 (sync
                  (thread (λ ()
                            (with-handlers ([exn:fail? void])
                              (forever (emit (prs sock))))))
                  (handle-evt sock (λ _ (emit eof)))))))
     #:on-stop (λ () (close-socket sock))
     #:command (λ vs
                 (cond [(equal? vs '(parser)) prs]
                       [(equal? vs '(socket)) sock]
                       [else unhandled])))))

(define (encoder prn)
  (λ (sock)
    (start
     (managed (process
               (λ ()
                 (sync
                  (thread (λ ()
                            (with-handlers ([exn:fail? void])
                              (forever (prn (take) sock)))))
                  (handle-evt sock die)))))
     #:on-stop (λ () (close-socket sock))
     #:command (λ vs
                 (cond [(equal? vs '(printer)) prn]
                       [(equal? vs '(socket)) sock]
                       [else unhandled])))))

(define (codec prs prn)
  (define make-decoder (decoder prs))
  (define make-encoder (encoder prn))
  (λ (sock)
    (define dec (make-decoder sock))
    (define enc (make-encoder sock))
    (start
     (stream enc dec)
     #:on-stop (λ () (stop enc) (stop dec))
     #:command (λ vs
                 (cond [(equal? vs '(decoder)) dec]
                       [(equal? vs '(encoder)) enc]
                       [(equal? vs '(socket)) sock]
                       [else unhandled])))))

(define (make-codec-type name prs prn)
  (values
   (decoder prs)
   (encoder prn)
   (codec prs prn)))

(define-syntax (define-codec stx)
  (syntax-case stx ()
    [(_ name prs prn)
     (with-syntax ([name-parser (format-id stx "~a-parser" #'name)]
                   [name-printer (format-id stx "~a-printer" #'name)]
                   [name-decoder (format-id stx "~a-decoder" #'name)]
                   [name-encoder (format-id stx "~a-encoder" #'name)]
                   [name-codec (format-id stx "~a-codec" #'name)])
       #'(splicing-letrec-values
             ([(prs*) prs]
              [(prn*) prn]
              [(make-dec make-enc make-cdc) (make-codec-type 'name prs* prn*)])
           (define name-parser prs*)
           (define name-printer prn*)
           (define name-decoder make-dec)
           (define name-encoder make-enc)
           (define name-codec make-cdc)))]))

(define-codec line read-line (flushed displayln))
(define-codec sexp read (flushed writeln))
(define-codec json
  read-json
  (flushed (λ (v out) (write-json v out) (newline out))))

(module+ test
  (require rackunit)

  (test-case
    "A decoder applies prs to sock and emits the result."
    (check = (recv ((decoder read) (string-socket #:in "123"))) 123))

  (test-case
    "A decoder stops when prs returns eof."
    (define dec ((decoder read) (string-socket #:in "123")))
    (check = (recv dec) 123)
    (check-pred eof-object? (recv dec))
    (sync dec)
    (check-pred dead? dec))

  (test-case
    "A decoder closes sock when it stops."
    (define dec ((decoder read) (null-socket)))
    (stop dec)
    (check-pred socket-closed? (dec 'socket)))

  (test-case
    "A decoder dies when sock closes."
    (define dec ((decoder read) (null-socket)))
    (check-pred eof-object? (recv dec))
    (sync dec)
    (check-pred socket-closed? (dec 'socket))
    (check-pred dead? dec))

  (test-case
    "decoder command 'parser returns a parser."
    (check eq? (((decoder read) (null-socket)) 'parser) read))

  (test-case
    "decoder command 'socket returns a socket."
    (define sock (null-socket))
    (check eq? (((decoder read) sock) 'socket) sock))

  (test-case
    "An encoder takes a value and prints it to sock."
    (define done (make-semaphore 0))
    (define enc ((encoder (λ (v sock) (write v sock) (semaphore-post done)))
                 (string-socket #:out #t)))
    (give enc 123)
    (semaphore-wait done)
    (check equal? (get-output-string (enc 'socket)) "123"))

  (test-case
    "An encoder stops when given eof."
    (define enc ((encoder write) (null-socket)))
    (give enc eof)
    (sync enc)
    (check-pred dead? enc))

  (test-case
    "An encoder closes sock when it stops."
    (define enc ((encoder write) (null-socket)))
    (stop enc)
    (check-pred socket-closed? (enc 'socket)))

  (test-case
    "An encoder dies when sock closes."
    (define enc ((encoder write) (null-socket)))
    (close-socket (enc 'socket))
    (sync enc)
    (check-pred dead? enc))

  (test-case
    "encoder command 'printer returns prn."
    (check eq? (((encoder write) (null-socket)) 'printer) write))

  (test-case
    "encoder command 'socket returns sock."
    (define sock (null-socket))
    (check eq? (((encoder write) sock) 'socket) sock))

  (test-case
    "A codec is a stream."
    (define cdc ((codec read write) (null-socket)))
    (check-pred process? cdc)
    (check-pred process? (cdc 'sink))
    (check-pred process? (cdc 'source)))

  (test-case
    "A codec's source is a decoder on prs and sock."
    (define sock (null-socket))
    (define cdc ((codec read write) sock))
    (check-pred process? (cdc 'source))
    (check eq? ((cdc 'source) 'parser) read)
    (check eq? ((cdc 'source) 'socket) sock))

  (test-case
    "A codec's sink is an encoder on prn and sock."
    (define sock (null-socket))
    (define cdc ((codec read write) sock))
    (check-pred process? (cdc 'sink))
    (check eq? ((cdc 'sink) 'printer) write)
    (check eq? ((cdc 'sink) 'socket) sock))

  (test-case
    "codec command 'decoder returns a decoder."
    (define sock (null-socket))
    (define dec (((codec read write) sock) 'decoder))
    (check eq? (dec 'parser) read)
    (check eq? (dec 'socket) sock))

  (test-case
    "codec command 'encoder returns an encoder."
    (define sock (null-socket))
    (define dec (((codec read write) sock) 'encoder))
    (check eq? (dec 'printer) write)
    (check eq? (dec 'socket) sock)))

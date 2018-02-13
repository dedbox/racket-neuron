#lang racket/base

(require neuron/concurrency
         json
         racket/contract/base
         racket/splicing)

(require (for-syntax racket/base
                     racket/syntax))

(provide
 define-codec
 (contract-out
  [parser/c contract?]
  [printer/c contract?]
  [decoder/c contract?]
  [encoder/c contract?]
  [codec/c contract?]
  [flushed (-> printer/c printer/c)]
  [decoder (-> parser/c input-port? process?)]
  [encoder (-> printer/c output-port? process?)]
  [codec (-> parser/c printer/c input-port? output-port? process?)]
  [make-codec-type (-> symbol? parser/c printer/c
                       (values (-> input-port? process?)
                               (-> output-port? process?)
                               (-> input-port? output-port? process?)))]
  [line-parser parser/c]
  [line-printer printer/c]
  [line-decoder (-> input-port? process?)]
  [line-encoder (-> output-port? process?)]
  [line-codec (-> input-port? output-port? process?)]
  [sexp-parser parser/c]
  [sexp-printer printer/c]
  [sexp-decoder (-> input-port? process?)]
  [sexp-encoder (-> output-port? process?)]
  [sexp-codec (-> input-port? output-port? process?)]
  [json-parser parser/c]
  [json-printer printer/c]
  [json-decoder (-> input-port? process?)]
  [json-encoder (-> output-port? process?)]
  [json-codec (-> input-port? output-port? process?)]))

(define parser/c (-> input-port? any/c))
(define printer/c (-> any/c output-port? any))
(define decoder/c (-> parser/c input-port? process?))
(define encoder/c (-> printer/c output-port? process?))
(define codec/c (-> parser/c printer/c input-port? output-port? process?))

(define (flushed prn)
  (λ (v out-port)
    (prn v out-port)
    (flush-output out-port)))

(define (decoder prs in-port)
  (start
   (managed (process (λ () (sync (thread (λ ()
                                           (with-handlers ([exn:fail? void])
                                             (forever (emit (prs in-port))))))
                                 (handle-evt (port-closed-evt in-port) die)))))
   #:on-stop (λ () (close-input-port in-port))
   #:command (λ vs
               (cond [(equal? vs '(parser)) prs]
                     [(equal? vs '(input-port)) in-port]
                     [else unhandled]))))

(define (encoder prn out-port)
  (start
   (managed (process (λ () (sync (thread (λ ()
                                           (with-handlers ([exn:fail? void])
                                             (forever (prn (take) out-port)))))
                                 (handle-evt (port-closed-evt out-port) die)))))
   #:on-stop (λ () (close-output-port out-port))
   #:command (λ vs
               (cond [(equal? vs '(printer)) prn]
                     [(equal? vs '(output-port)) out-port]
                     [else unhandled]))))

(define (codec prs prn in-port out-port)
  (define dec (decoder prs in-port))
  (define enc (encoder prn out-port))
  (start (stream enc dec)
         #:on-stop (λ () (stop enc) (stop dec))
         #:command (λ vs
                     (cond [(equal? vs '(decoder)) dec]
                           [(equal? vs '(encoder)) enc]
                           [else unhandled]))))

(define (make-codec-type name prs prn)
  (values (λ (in-port) (decoder prs in-port))
          (λ (out-port) (encoder prn out-port))
          (λ (in-port out-port) (codec prs prn in-port out-port))))

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
  (λ (v out) (write-json v out) (newline out) (flush-output out)))

(module+ test
  (require rackunit)

  (test-case
    "A decoder applies prs to in-port and emits the result."
    (check = (recv (decoder read (open-input-string "123"))) 123))

  (test-case
    "A decoder stops when prs returns eof."
    (define dec (decoder read (open-input-string "123")))
    (check = (recv dec) 123)
    (check-pred eof-object? (recv dec))
    (check-pred dead? dec))

  (test-case
    "A decoder closes in-port when it stops."
    (define dec (decoder read (open-input-string "1 2 3")))
    (stop dec)
    (check-pred port-closed? (dec 'input-port)))

  (test-case
    "A decoder dies when in-port closes."
    (define dec (decoder read (open-input-string "")))
    (sync dec)
    (check-pred port-closed? (dec 'input-port))
    (check-pred dead? dec))

  (test-case
    "decoder command 'parser returns prs."
    (check equal? ((decoder read (open-input-string "")) 'parser) read))

  (test-case
    "decoder command 'input-port returns in-port."
    (define in-port (open-input-string ""))
    (check equal? ((decoder read in-port) 'input-port) in-port))

  (test-case
    "An encoder takes a value and applies prn to it and out-port."
    (define done (make-semaphore 0))
    (define enc (encoder (λ (v out) (write v out) (semaphore-post done))
                         (open-output-string)))
    (give enc 123)
    (semaphore-wait done)
    (check equal? (get-output-string (enc 'output-port)) "123"))

  (test-case
    "An encoder stops when given eof."
    (define enc (encoder write (open-output-string)))
    (give enc eof)
    (sync enc)
    (check-pred dead? enc))

  (test-case
    "An encoder closes out-port when it stops."
    (define enc (encoder write (open-output-string)))
    (stop enc)
    (check-pred port-closed? (enc 'output-port)))

  (test-case
    "An encoder dies when out-port closes."
    (define enc (encoder write (open-output-string)))
    (close-output-port (enc 'output-port))
    (sync enc)
    (check-pred dead? enc))

  (test-case
    "encoder command 'printer returns prn."
    (check equal? ((encoder write (open-output-string)) 'printer) write))

  (test-case
    "encoder command 'output-port returns out-port."
    (define out-port (open-output-string))
    (check equal? ((encoder write out-port) 'output-port) out-port))

  (test-case
    "A codec is a stream."
    (define cdc (codec read write (open-input-string "") (open-output-string)))
    (check-pred process? cdc)
    (check-pred process? (cdc 'sink))
    (check-pred process? (cdc 'source)))

  (test-case
    "A codec source is a decoder on prs and in-port."
    (define cdc (codec read write (open-input-string "") (open-output-string)))
    (check-pred process? (cdc 'source))
    (check-pred procedure? ((cdc 'source) 'parser))
    (check-pred input-port? ((cdc 'source) 'input-port)))

  (test-case
    "A codec sink is an encoder on prn and out-port."
    (define cdc (codec read write (open-input-string "") (open-output-string)))
    (check-pred process? (cdc 'sink))
    (check-pred procedure? ((cdc 'sink) 'printer))
    (check-pred output-port? ((cdc 'sink) 'output-port)))

  (test-case
    "codec command 'decoder returns a decoder."
    (define cdc (codec read write (open-input-string "") (open-output-string)))
    (check equal? (cdc 'decoder) (cdc 'source)))

  (test-case
    "codec command 'encoder returns an encoder."
    (define cdc (codec read write (open-input-string "") (open-output-string)))
    (check equal? (cdc 'encoder) (cdc 'sink))))

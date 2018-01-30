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

(define parser/c (-> input-port? any))
(define printer/c (-> any/c output-port? any))

(define (flushed prn)
  (λ (v out-port)
    (prn v out-port)
    (flush-output out-port)))

(define (decoder prs in-port)
  (start
   (managed (source (λ () (with-handlers ([exn:fail? die]) (prs in-port)))))
   #:on-stop (λ () (close-input-port in-port))
   #:command (λ vs
               (cond [(equal? vs '(parser)) prs]
                     [(equal? vs '(input-port)) in-port]
                     [else unhandled]))))

(define (encoder prn out-port)
  (start
   (managed (sink (λ (v) (with-handlers ([exn:fail? die]) (prn v out-port)))))
   #:on-stop (λ () (close-output-port out-port))
   #:command (λ vs
               (cond [(equal? vs '(printer)) prn]
                     [(equal? vs '(output-port)) out-port]
                     [else unhandled]))))

(define (codec prs prn in-port out-port)
  (define dec (decoder prs in-port))
  (define enc (encoder prn out-port))
  (start (socket enc dec)
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

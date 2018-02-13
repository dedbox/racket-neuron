#lang racket/base

(require neuron/concurrency
         racket/contract/base
         racket/function)

(provide
 (contract-out
  [port-sink (-> output-port? process?)]
  [port-source (-> exact-nonnegative-integer? input-port? process?)]
  [port-stream (-> exact-nonnegative-integer? input-port? output-port?
                   process?)]
  [byte-sink (-> process?)]
  [string-sink (-> process?)]
  [file-sink (->* (path-string?)
                  (#:mode (or/c 'binary 'text)
                   #:exists (or/c 'error 'append 'update 'can-update
                                  'replace 'truncate
                                  'must-truncate 'truncate/replace))
                  process?)]
  [file-source (->* (path-string? exact-nonnegative-integer?)
                    (#:mode (or/c 'binary 'text))
                    process?)]
  [directory-source (->* () (path-string?) process?)]))

(define (port-sink out-port)
  (start (managed (sink (λ (bs)
                          (with-handlers ([exn:fail? die])
                            (write-bytes bs out-port)))))
         #:on-stop (λ () (close-output-port out-port))
         #:command
         (λ vs (if (equal? vs '(output-port)) out-port unhandled-command))))

(define (port-source amt in-port)
  (start (managed (source (λ ()
                            (with-handlers ([exn:fail? die])
                              (read-bytes amt in-port)))))
         #:on-stop (λ () (close-input-port in-port))
         #:command (λ vs
                     (if (equal? vs '(input-port)) in-port unhandled-command))))

(define (port-stream amt in-port out-port)
  (stream (port-sink out-port) (port-source amt in-port)))

(define (byte-sink)
  (define out-port (open-output-bytes))
  (define (emit-next-bytes . _)
    (emit (get-output-bytes out-port #t)))
  (start
   (managed (sink (curryr display out-port)) #:on-take-eof emit-next-bytes)
   #:on-stop emit-next-bytes))

(define (string-sink)
  (define out-port (open-output-bytes))
  (define (emit-next-string . _)
    (emit (bytes->string/utf-8 (get-output-bytes out-port #t) #\?)))
  (start
   (managed (sink (curryr display out-port)) #:on-take-eof emit-next-string)
   #:on-stop emit-next-string))

(define (file-sink path
                   #:mode [mode-flag 'binary]
                   #:exists [exists-flag 'error])
  (port-sink (open-output-file path #:mode mode-flag #:exists exists-flag)))

(define (file-source path amt
                     #:mode [mode-flag 'binary])
  (port-source amt (open-input-file path #:mode mode-flag)))

(define (directory-source [path (current-directory)])
  (process (λ () (for-each emit (directory-list path)))))

#lang racket/base

(require racket/contract/base
         racket/port)

(provide
 (contract-out
  [struct socket
    ([in-port input-port?]
     [out-port output-port?])]
  [close-socket (-> socket? void?)]
  [socket-closed? (-> socket? boolean?)]
  [null-socket (-> socket?)]
  [byte-socket (->* () (#:in bytes? #:out boolean?) socket?)]
  [string-socket (->* () (#:in string? #:out boolean?) socket?)]
  [file-socket
   (->* ()
        (#:in (or/c path-string? #f)
         #:in-mode (or/c 'binary 'text)
         #:out (or/c path-string? #f)
         #:out-mode (or/c 'binary 'text)
         #:exists (or/c 'error 'append 'update 'can-update
                        'replace 'truncate
                        'must-truncate 'truncate/replace))
        socket?)]))

(struct socket
  (in-port out-port)
  #:property prop:input-port (struct-field-index in-port)
  #:property prop:output-port (struct-field-index out-port)
  #:property prop:evt
  (λ (sock)
    (choice-evt
      (handle-evt (port-closed-evt (socket-in-port sock))
                  (λ _ (close-output-port sock)))
      (handle-evt (port-closed-evt (socket-out-port sock))
                  (λ _ (close-input-port sock))))))

(define (close-socket sock)
  (close-input-port sock)
  (close-output-port sock)
  (sync sock))

(define (socket-closed? sock)
  (and
   (port-closed? (socket-in-port sock))
   (port-closed? (socket-out-port sock))))

(define (null-socket)
  (socket
   (open-input-string "")
   (open-output-nowhere)))

(define (byte-socket #:in [bstr #""]
                     #:out [out? #f])
  (socket
   (open-input-bytes bstr)
   (if out?
       (open-output-bytes)
       (open-output-nowhere))))

(define (string-socket #:in [str ""]
                       #:out [out? #f])
  (socket
   (open-input-string str)
   (if out?
       (open-output-string)
       (open-output-nowhere))))

(define (file-socket #:in [in-path #f]
                     #:in-mode [in-mode-flag 'binary]
                     #:out [out-path #f]
                     #:out-mode [out-mode-flag 'binary]
                     #:exists [exists-flag 'error])
  (socket
   (if in-path
       (open-input-file in-path #:mode in-mode-flag)
       (open-input-bytes #""))
   (if out-path
       (open-output-file out-path #:mode out-mode-flag #:exists exists-flag)
       (open-output-nowhere))))

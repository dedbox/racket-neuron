#lang racket/base

(require racket/contract/base
         (only-in racket/list flatten))

(provide
 start
 (contract-out
  [unhandled symbol?]
  [struct unhandled-command
    ([process process?]
     [args (listof any/c)])]
  [process? predicate/c]
  [process (-> (-> any) process?)]
  [process-in-ch (-> process? channel?)]
  [process-out-ch (-> process? channel?)]
  [command (-> process? (listof any/c) any)]
  [stop (-> process? void?)]
  [kill (-> process? void?)]
  [wait (-> process? void?)]
  [dead? (-> process? boolean?)]
  [alive? (-> process? boolean?)]
  [current-process (-> process?)]
  [quit (->* () #:rest (listof any/c) void?)]
  [die (->* () #:rest (listof any/c) void?)]
  [deadlock (->* () #:rest (listof any/c) void?)]))

(define unhandled
  (string->unreadable-symbol "unhandled"))

(struct unhandled-command (process args) #:transparent)

(struct process
  (thread dead-cont stop-cont command raised in-ch out-ch)
  #:constructor-name make-process
  #:omit-define-syntaxes
  #:property prop:evt (λ (π) (wait-evt π))
  #:property prop:procedure (λ (π . vs)
                              (define result (command π vs))
                              (if (eq? result unhandled)
                                  (raise (unhandled-command π vs))
                                  result)))

(define (wait-evt π)
  (handle-evt
   (process-thread π)
   (λ _
     (define raised (unbox (process-raised π)))
     (when (pair? raised)
       (raise (car raised)))
     π)))

(define (command π vs)
  (let loop ([handlers (process-command π)])
    (if (null? handlers)
        unhandled
        (let ([result (apply (car handlers) vs)])
          (if (equal? result unhandled)
              (loop (cdr handlers))
              result)))))

(define current-process (make-parameter #f))

(define (quit . _)
  ((process-stop-cont (current-process))))

(define (die . _)
  ((process-dead-cont (current-process))))

(define (deadlock . _)
  (sync never-evt))

(define current-on-stop (make-parameter null))
(define current-on-dead (make-parameter null))
(define current-command (make-parameter null))

(define (process thunk)
  (define on-stop-hook (flatten (current-on-stop)))
  (define on-dead-hook (flatten (current-on-dead)))
  (define command-hook (flatten (current-command)))
  (parameterize ([current-on-stop null]
                 [current-on-dead null]
                 [current-command null])
    (define raised (box #f))
    (define ready-ch (make-channel))
    (define (process)
      (let/ec dead-cont
        (let/ec stop-cont
          (parameterize-break #f
            (channel-put ready-ch dead-cont)
            (channel-put ready-ch stop-cont)
            (parameterize ([current-process (channel-get ready-ch)])
              (with-handlers ([exn:break:hang-up? quit]
                              [exn:break:terminate? die]
                              [(λ _ #t) (λ (e) (set-box! raised (list e)) (die))])
                (parameterize-break #t
                  (thunk))))))
        (parameterize-break #f
          (for ([proc on-stop-hook]) (proc))))
      (parameterize-break #f
        (for ([proc on-dead-hook]) (proc))))
    (define π
      (make-process (thread process)
                    (channel-get ready-ch)
                    (channel-get ready-ch)
                    command-hook
                    raised
                    (make-channel)
                    (make-channel)))
    (channel-put ready-ch π)
    π))

(define-syntax start
  (syntax-rules ()
    [(start body ... #:on-stop on-stop)
     (parameterize ([current-on-stop (cons on-stop (current-on-stop))])
       (start body ...))]
    [(start body ... #:on-dead on-dead)
     (parameterize ([current-on-dead (cons on-dead (current-on-dead))])
       (start body ...))]
    [(start body ... #:command command)
     (parameterize ([current-command (cons command (current-command))])
       (start body ...))]
    [(start π) π]))

(define (stop π)
  (break-thread (process-thread π) 'hang-up)
  (wait π))

(define (kill π)
  (break-thread (process-thread π) 'terminate)
  (wait π))

(define (wait π)
  (void (sync π)))

(define (dead? π)
  (thread-dead? (process-thread π)))

(define (alive? π)
  (not (dead? π)))

(module+ test
  (require rackunit)

  ;; starting and stopping

  (test-case
    "A process is alive if it is not dead."
    (define π (process deadlock))
    (check-true (alive? π))
    (check-false (dead? π)))

  (test-case
    "A process is dead if it is not alive."
    (define π (process void))
    (wait π)
    (check-true (dead? π))
    (check-false (alive? π)))

  (test-case
    "A process is alive when it starts."
    (define π (process deadlock))
    (check-true (alive? π)))

  (test-case
    "A process is dead after it ends."
    (define π (process void))
    (wait π)
    (check-true (dead? π)))

  (test-case
    "A process can be stopped before it ends."
    (define π (process deadlock))
    (stop π)
    (check-true (dead? π)))

  (test-case
    "A process is dead after it is stopped."
    (define π (process deadlock))
    (stop π)
    (check-true (dead? π)))

  (test-case
    "A process can be killed before it ends."
    (define π (process deadlock))
    (kill π)
    (check-true (dead? π)))

  (test-case
    "A process is dead after it is killed."
    (define π (process deadlock))
    (stop π)
    (check-true (dead? π)))

  ;; on-stop hook

  (require racket/function)

  (test-case
    "A process calls its on-stop hook when it ends."
    (define stopped #f)
    (wait (start (process void) #:on-stop (λ () (set! stopped #t))))
    (check-true stopped))

  (test-case
    "A process calls its on-stop hook when it stops."
    (define stopped #f)
    (stop (start (process deadlock) #:on-stop (λ () (set! stopped #t))))
    (check-true stopped))

  (test-case
    "A process does not call its on-stop hook when it dies."
    (define stopped #f)
    (wait (start (process die) #:on-stop (λ () (set! stopped #t))))
    (check-false stopped))

  (test-case
    "A process does not call its on-stop hook when it is killed."
    (define stopped #f)
    (kill (start (process deadlock) #:on-stop (λ () (set! stopped #t))))
    (check-false stopped))

  ;; on-dead hook

  (test-case
    "A process calls its on-dead hook when it ends."
    (define dead #f)
    (wait (start (process void) #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it stops."
    (define dead #f)
    (stop (start (process deadlock) #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it dies."
    (define dead #f)
    (wait (start (process die) #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it is killed."
    (define dead #f)
    (kill (start (process deadlock) #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  ;; command handler

  (test-case
    "A process invokes its command handler when applied as a procedure."
    (define handled #f)
    ((start (process deadlock) #:command (λ (v) (set! handled v))) #t)
    (check-true handled))

  ;; synchronizable event

  (test-case
    "A process is ready for synchronization when sync would not block."
    (define π (process deadlock))
    (check-false (sync/timeout 0 π))
    (kill π)
    (check-false (not (sync/timeout 0 π))))

  (test-case
    "The synchronization result of a process is the process itself."
    (define π (process die))
    (check eq? (sync π) π))

  ;; unhandled exceptions

  (test-case
    "Unhandled exceptions are fatal."
    (define π (process (λ () (raise #t))))
    (with-handlers ([(λ _ #t) (λ (e) e)]) (wait π))
    (check-true (dead? π)))

  (test-case
    "wait raises unhandled-exception on unhandled exceptions."
    (check-exn boolean? (λ () (wait (process (λ () (raise #t))))))))

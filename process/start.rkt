#lang racket/base

(require racket/contract/base
         (only-in racket/list flatten))

(provide
 (contract-out
  [current-process (-> process?)]
  [quit (->* () #:rest (listof any/c) void?)]
  [die (->* () #:rest (listof any/c) void?)]
  [deadlock (->* () #:rest (listof any/c) void?)]
  [struct process ([thread thread?]
                   [dead-cont continuation?]
                   [stop-cont continuation?]
                   [handler (listof procedure?)]
                   [raised (box/c (or/c #f (cons/c any/c null)))]
                   [input-ch channel?]
                   [output-ch channel?])]
  [dead? (-> process? boolean?)]
  [alive? (-> process? boolean?)]
  [start (->* ((-> any))
              (#:on-stop (-> any)
               #:on-dead (-> any)
               #:command (or/c procedure? (listof procedure?)))
              process?)]
  [stop (-> process? void?)]
  [kill (-> process? void?)]
  [wait (-> process? void?)]
  [struct unhandled-exception ([value any/c])]
  [struct unhandled-command ([args (listof any/c)])]))

(define current-process (make-parameter #f))
(define quit (λ _ ((process-stop-cont (current-process)))))
(define die (λ _ ((process-dead-cont (current-process)))))
(define deadlock (λ _ (sync never-evt)))

(struct process
  (thread dead-cont stop-cont handler raised input-ch output-ch)
  #:property prop:evt
  (λ (π)
    (handle-evt
     (process-thread π)
     (λ _
       (define raised (unbox (process-raised π)))
       (if (list? raised)
           (raise (unhandled-exception (car raised)))
           π))))
  #:property prop:procedure
  (λ (π . args)
    (let loop ([procs (process-handler π)])
      (if (null? procs)
          (raise (unhandled-command args))
          (with-handlers ([unhandled-command? (λ _ (loop (cdr procs)))])
            (apply (car procs) args))))))

(define (dead? π)
  (thread-dead? (process-thread π)))

(define (alive? π)
  (not (dead? π)))

(define (start thunk
               #:on-stop [on-stop void]
               #:on-dead [on-dead void]
               #:command [handler null])
  (define raised (box #f))
  (define ready-ch (make-channel))
  (define (ready-process)
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
      (on-stop))
    (on-dead))
  (define π (process (thread ready-process)
                     (channel-get ready-ch)
                     (channel-get ready-ch)
                     (flatten handler)
                     raised
                     (make-channel)
                     (make-channel)))
  (channel-put ready-ch π)
  π)

(define (stop π)
  (break-thread (process-thread π) 'hang-up)
  (wait π))

(define (kill π)
  (break-thread (process-thread π) 'terminate)
  (wait π))

(define (wait π)
  (void (sync π)))

(struct unhandled-exception (value) #:transparent)
(struct unhandled-command (args) #:transparent)

(module+ test
  (require rackunit)

  ;; starting and stopping

  (test-case
    "A process is alive if it is not dead."
    (define π (start deadlock))
    (check-true (alive? π))
    (check-false (dead? π)))

  (test-case
    "A process is dead if it is not alive."
    (define π (start void))
    (wait π)
    (check-true (dead? π))
    (check-false (alive? π)))

  (test-case
    "A process is alive when it starts."
    (define π (start deadlock))
    (check-true (alive? π)))

  (test-case
    "A process is dead after it ends."
    (define π (start void))
    (wait π)
    (check-true (dead? π)))

  (test-case
    "A process can be stopped before it ends."
    (define π (start deadlock))
    (stop π)
    (check-true (dead? π)))

  (test-case
    "A process is dead after it is stopped."
    (define π (start deadlock))
    (stop π)
    (check-true (dead? π)))

  (test-case
    "A process can be killed before it ends."
    (define π (start deadlock))
    (kill π)
    (check-true (dead? π)))

  (test-case
    "A process is dead after it is killed."
    (define π (start deadlock))
    (stop π)
    (check-true (dead? π)))

  ;; on-stop hook

  (require racket/function)

  (test-case
    "A process calls its on-stop hook when it ends."
    (define stopped #f)
    (wait (start void #:on-stop (λ () (set! stopped #t))))
    (check-true stopped))

  (test-case
    "A process calls its on-stop hook when it stops."
    (define stopped #f)
    (stop (start deadlock #:on-stop (λ () (set! stopped #t))))
    (check-true stopped))

  (test-case
    "A process does not call its on-stop hook when it dies."
    (define stopped #f)
    (wait (start die #:on-stop (λ () (set! stopped #t))))
    (check-false stopped))

  (test-case
    "A process does not call its on-stop hook when it is killed."
    (define stopped #f)
    (kill (start deadlock #:on-stop (λ () (set! stopped #t))))
    (check-false stopped))

  ;; on-dead hook

  (test-case
    "A process calls its on-dead hook when it ends."
    (define dead #f)
    (wait (start void #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it stops."
    (define dead #f)
    (stop (start deadlock #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it dies."
    (define dead #f)
    (wait (start die #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it is killed."
    (define dead #f)
    (kill (start deadlock #:on-dead (λ () (set! dead #t))))
    (check-true dead))

  ;; command handler

  (test-case
    "A process invokes its command handler when applied as a procedure."
    (define handled #f)
    ((start deadlock #:command (λ (v) (set! handled v))) #t)
    (check-true handled))

  ;; synchronizable event

  (test-case
    "A process is ready for synchronization when sync would not block."
    (define π (start deadlock))
    (check-false (sync/timeout 0 π))
    (kill π)
    (check-false (not (sync/timeout 0 π))))

  (test-case
    "The synchronization result of a process is the process itself."
    (define π (start die))
    (check eq? (sync π) π))

  ;; unhandled exceptions

  (test-case
    "Unhandled exceptions are fatal."
    (define π (start (λ () (raise #t))))
    (with-handlers ([(λ _ #t) (λ (e) e)]) (wait π))
    (check-true (dead? π)))

  (test-case
    "wait raises unhandled-exception on unhandled exceptions."
    (check-exn unhandled-exception? (λ () (wait (start (λ () (raise #t))))))))

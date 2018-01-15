#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [current-process (-> process?)]
  [quit (->* () #:rest (listof any/c) void?)]
  [die (->* () #:rest (listof any/c) void?)]
  [deadlock (->* () #:rest (listof any/c) void?)]
  [process? predicate/c]
  [dead? (-> process? boolean?)]
  [alive? (-> process? boolean?)]
  [start (-> (-> any)
             #:on-stop (-> any)
             #:command procedure?
             process?)]
  [stop (-> process? void?)]
  [kill (-> process? void?)]
  [wait (-> process? void?)]
  )
 (struct-out exn:unhandled))

;;; Starting and Stopping Processes

(define current-process (make-parameter #f))
(define quit (λ _ ((process-stop-cont (current-process)))))
(define die (λ _ ((process-die-cont (current-process)))))
(define deadlock (λ _ (sync never-evt)))

(struct process (thread die-cont stop-cont handler [exn #:auto #:mutable])
        #:auto-value #f
        #:property prop:evt (λ (π) (handle-evt (wait-evt π) (λ _ π)))
        #:property prop:procedure (λ (π . args)
                                    (apply (process-handler π) args)))

(define (dead? π)
  (thread-dead? (process-thread π)))

(define (alive? π)
  (not (dead? π)))

(define (start thunk
               #:on-stop [on-stop void]
               #:on-dead [on-dead void]
               #:command [handler void])
  (define (unhandled e)
    (set-process-exn! (current-process) e)
    (die))
  (define ready-ch (make-channel))
  (define (ready-process)
    (parameterize-break #f
      (let/ec die-cont
        (channel-put ready-ch die-cont)
        (let/ec stop-cont
          (channel-put ready-ch stop-cont)
          (parameterize ([current-process (channel-get ready-ch)])
            (with-handlers ([exn:break:hang-up? quit]
                            [exn:break:terminate? die]
                            [(λ _ #t) unhandled])
              (parameterize-break #t
                (thunk)))))
        (on-stop))
      (on-dead)))
  (define π (process (thread ready-process)
                     (channel-get ready-ch)
                     (channel-get ready-ch)
                     handler))
  (channel-put ready-ch π)
  π)

(define (stop π)
  (parameterize-break #f
    (break-thread (process-thread π) 'hang-up)
    (wait π)))

(define (kill π)
  (parameterize-break #f
    (break-thread (process-thread π) 'terminate)
    (wait π)))

(define (wait π)
  (sync (wait-evt π)))

(define (wait-evt π)
  (handle-evt
   (process-thread π)
   (λ _
     (when (process-exn π)
       (raise (exn:unhandled (process-exn π)))))))

(struct exn:unhandled (value) #:transparent)

(module+ test
  (require rackunit)

  ;; starting and stopping

  (test-case
    "A process is alive when it starts."
    (define π (start deadlock))
    (check-true (alive? π))
    (check-false (dead? π)))

  (test-case
    "A process is dead after it ends."
    (define π (start void))
    (wait π)
    (check-true (dead? π))
    (check-false (alive? π)))

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
    "A process can be stopped before it ends."
    (define π (start deadlock))
    (check-true (alive? π))
    (stop π)
    (check-true (dead? π)))

  (test-case
    "A process is dead after it is stopped."
    (define π (start deadlock))
    (stop π)
    (check-true (dead? π))
    (check-false (alive? π)))

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
    (define latch (make-semaphore 0))
    (define π
      (start (λ () (semaphore-wait latch)) #:on-stop (λ () (set! stopped #t))))
    (check-false stopped)
    (semaphore-post latch)
    (wait π)
    (check-true stopped))

  (test-case
    "A process calls its on-stop hook when it stops."
    (define stopped #f)
    (define π (start deadlock #:on-stop (λ () (set! stopped #t))))
    (check-false stopped)
    (stop π)
    (check-true stopped))

  (test-case
    "A process does not call its on-stop hook when it dies."
    (define stopped #f)
    (define latch (make-semaphore 0))
    (define π
      (start (λ () (semaphore-wait latch) (die)) #:on-stop (λ () (set! stopped #t))))
    (check-false stopped)
    (semaphore-post latch)
    (wait π)
    (check-false stopped))

  (test-case
    "A process does not call its on-stop hook when it is killed."
    (define stopped #f)
    (define π (start deadlock #:on-stop (λ () (set! stopped #t))))
    (check-false stopped)
    (kill π)
    (check-false stopped))

  ;; on-dead hook

  (test-case
    "A process calls its on-dead hook when it ends."
    (define dead #f)
    (define latch (make-semaphore 0))
    (define π
      (start (λ () (semaphore-wait latch)) #:on-dead (λ () (set! dead #t))))
    (check-false dead)
    (semaphore-post latch)
    (wait π)
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it stops."
    (define dead #f)
    (define π (start deadlock #:on-dead (λ () (set! dead #t))))
    (check-false dead)
    (stop π)
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it dies."
    (define dead #f)
    (define latch (make-semaphore 0))
    (define π
      (start (λ () (semaphore-wait latch)) #:on-dead (λ () (set! dead #t))))
    (check-false dead)
    (semaphore-post latch)
    (wait π)
    (check-true dead))

  (test-case
    "A process calls its on-dead hook when it is killed."
    (define dead #f)
    (define π (start deadlock #:on-dead (λ () (set! dead #t))))
    (check-false dead)
    (kill π)
    (check-true dead))

  ;; command handler

  (test-case
    "A process invokes its command handler when applied as a procedure."
    (define handled #f)
    (define π (start deadlock #:command (λ (v) (set! handled v))))
    (check-false handled)
    (π #t)
    (check-true handled))

  ;; synchronizable event

  (test-case
    "A process is ready for synchronization when wait would not block."
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
    "wait raises exn:unhandled on unhandled exceptions."
    (define π (start (λ () (raise #t))))
    (check-exn exn:unhandled? (λ () (wait π)))))

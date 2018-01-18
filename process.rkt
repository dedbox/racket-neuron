#lang racket/base

(require racket/contract/base
         racket/dict
         racket/function
         racket/list
         racket/match)

;;; Starting and Stopping Processes

(provide
 (contract-out
  [current-process (-> process?)]
  [quit (->* () #:rest (listof any/c) void?)]
  [die (->* () #:rest (listof any/c) void?)]
  [deadlock (->* () #:rest (listof any/c) void?)]
  [process? predicate/c]
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
(define die (λ _ ((process-die-cont (current-process)))))
(define deadlock (λ _ (sync never-evt)))

(struct process
  (thread die-cont stop-cont handler input-ch output-ch [exn #:auto #:mutable])
  #:auto-value #f
  #:property prop:evt (λ (π) (handle-evt (wait-evt π) (λ _ π)))
  #:property prop:procedure (λ args (apply command args)))

(define (command π . vs)
  (let loop ([procs (process-handler π)])
    (if (null? procs)
        (raise (unhandled-command vs))
        (with-handlers ([unhandled-command? (λ _ (loop (cdr procs)))])
          (apply (car procs) vs)))))

(define (dead? π)
  (thread-dead? (process-thread π)))

(define (alive? π)
  (not (dead? π)))

(define (start thunk
               #:on-stop [on-stop void]
               #:on-dead [on-dead void]
               #:command [handler null])
  (define (do-unhandled e)
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
                            [(λ _ #t) do-unhandled])
              (parameterize-break #t
                (thunk)))))
        (on-stop))
      (on-dead)))
  (define π (process (thread ready-process)
                     (channel-get ready-ch)
                     (channel-get ready-ch)
                     (flatten handler)
                     (make-channel)
                     (make-channel)))
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
       (raise (unhandled-exception (process-exn π)))))))

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
    "wait raises unhandled-exception on unhandled exceptions."
    (check-exn unhandled-exception? (λ () (wait (start (λ () (raise #t))))))))

;;; Inter-Process Communication

(provide
 (contract-out
  [give (->* (process?) (any/c) boolean?)]
  [take (-> any/c)]
  [try-take (-> any/c)]
  [emit (->* () (any/c) void?)]
  [recv (-> process? any/c)]
  [try-recv (-> process? any/c)]
  [give-evt (->* (process?) (any/c) evt?)]
  [take-evt (-> evt?)]
  [emit-evt (->* () (any/c) evt?)]
  [recv-evt (-> process? evt?)]))

;; Commands

(define (give π [v (void)])
  (sync (give-evt π v)))

(define (take)
  (sync (take-evt)))

(define (try-take)
  (channel-try-get (process-input-ch (current-process))))

(define (emit [v (void)])
  (sync (emit-evt v)))

(define (recv π)
  (sync (recv-evt π)))

(define (try-recv π)
  (channel-try-get (process-output-ch π)))

;; Events

(define (give-evt π [v (void)])
  (choice-evt
    (handle-evt (channel-put-evt (process-input-ch π) v) (λ _ #t))
    (handle-evt π (λ _ #f))))

(define (take-evt)
  (process-input-ch (current-process)))

(define (emit-evt [v (void)])
  (handle-evt (channel-put-evt (process-output-ch (current-process)) v) void))

(define (recv-evt π)
  (choice-evt
    (process-output-ch π)
    (handle-evt π (λ _ eof))))

(module+ test
  (require rackunit)

  ;; Events

  (test-case
    "A give-evt is ready when π accepts v."
    (define π (start (λ () (emit) (take))))
    (define evt (give-evt π))
    (check-false (sync/timeout 0 evt))
    (recv π)
    (check-false (not (sync evt))))

  (test-case
    "A give-evt syncs to #t if π accepts v."
    (check-true (sync (give-evt (start (λ () (take)))))))

  (test-case
    "A give-evt syncs to #f if π dies before accepting v."
    (define π (start deadlock))
    (define evt (give-evt π))
    (kill π)
    (check-false (sync evt)))

  (test-case
    "A take-evt is ready when a process provides a value."
    (define π (start (λ () (check-false (not (sync (take-evt)))))))
    (give π)
    (wait π))

  (test-case
    "A take-evt syncs to the provided value."
    (define π (start (λ () (check eq? (sync (take-evt)) 3))))
    (give π 3)
    (wait π))

  (test-case
    "An emit-evt is ready when a process accepts v."
    (define π (start (λ () (check-false (not (sync (emit-evt)))))))
    (recv π)
    (wait π))

  (test-case
    "An emit-evt syncs to void."
    (define π (start (λ () (check-pred void? (sync (emit-evt))))))
    (recv π)
    (wait π))

  (test-case
    "A recv-evt is ready when a value is accepted from π."
    (define π (start (λ () (take) (emit))))
    (define evt (recv-evt π))
    (check-false (sync/timeout 0 evt))
    (give π)
    (check-false (not (sync evt))))

  (test-case
    "A recv-evt syncs to the value accepted from π."
    (check = (sync (recv-evt (start (λ () (emit 5))))) 5))

  (test-case
    "A recv-evt syncs to eof if π dies."
    (define π (start deadlock))
    (define evt (recv-evt π))
    (kill π)
    (check-pred eof-object? (sync evt)))

  ;; Commands

  (test-case
    "give blocks until π accepts v."
    (check-false (not (give (start (λ () (take)))))))

  (test-case
    "give blocks until π dies."
    (check-false (give (start die))))

  (test-case
    "give returns #t if π accepts v."
    (check-true (give (start (λ () (take))))))

  (test-case
    "give returns #f if π dies before accepting v."
    (check-false (give (start die))))

  (test-case
    "take blocks until a value is provided to π."
    (define π (start (λ () (check-false (not (take))))))
    (give π)
    (wait π))

  (test-case
    "take returns the provided value."
    (define π (start (λ () (check = (take) 7))))
    (give π 7)
    (wait π))

  (test-case
    "try-take returns the provided value if any."
    (define π1 (start (λ () (emit) (check = (try-take) 11))))
    (define π2 (start (λ () (give π1 11))))
    (sync/timeout 0.1 π2)
    (recv π1)
    (wait π2)
    (wait π1))

  (test-case
    "try-take returns #f if no value is provided."
    (wait (start (λ () (check-false (try-take))))))

  (test-case
    "emit blocks until a process accepts v."
    (define π (start (λ () (check-false (not (emit))))))
    (recv π)
    (wait π))

  (test-case
    "emit returns void."
    (define π (start (λ () (check-pred void? (emit)))))
    (recv π)
    (wait π))

  (test-case
    "recv blocks until a value is accepted from π."
    (check-false (not (recv (start (λ () (emit)))))))

  (test-case
    "recv blocks until π dies."
    (check-false (not (recv (start die) ))))

  (test-case
    "recv returns the value accepted from π."
    (check = (recv (start (λ () (emit 13)))) 13))

  (test-case
    "recv returns eof when π dies."
    (check-pred eof-object? (recv  (start die))))

  (test-case
    "try-recv returns the value accepted from π if any."
    (define π (start (λ () (emit 17))))
    (sync/timeout 0.1 π)
    (check = (try-recv π) 17))

  (test-case
    "try-recv returns #f if no value is accepted."
    (check-false (try-recv (start deadlock) ))))

#lang racket/base

(module+ test
  (require rackunit)

  ;; starting and stopping

  (test-case
    "A process is alive when it starts."
    (fail))

  (test-case
    "A process can be stopped or killed before it ends."
    (fail))

  ;; on-stop hook

  (test-case
    "A process calls its on-stop hook when it ends."
    (fail))

  (test-case
    "A process calls its on-stop hook when it stops."
    (fail))

  (test-case
    "A process does not call its on-stop hook when it dies."
    (fail))

  (test-case
    "A process does not call its on-stop hook when it is killed."
    (fail))

  ;; unhandled exceptions

  (test-case
    "Unhandled exceptions are fatal."
    (fail))

  (test-case
    "wait raises exn:fail:unhandled on unhandled exceptions."
    (fail))

  ;; command handler

  (test-case
    "A process invokes its command handler when applied as a procedure."
    (fail))

  ;; synchronizable event

  (test-case
    "A process is ready for synchronization when wait would not block."
    (fail))

  (test-case
    "The synchronization result of a process is the process itself."
    (fail)))

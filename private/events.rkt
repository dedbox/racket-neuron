#lang racket/base

(require racket/contract/base
         racket/dict
         racket/function
         (prefix-in list: racket/list))

(provide
 (contract-out
  [evt-set (-> evt? ... evt?)]
  [evt-sequence
   (->* ((-> evt?))
        (#:then (-> any/c any))
        #:rest (listof (-> evt?))
        evt?)]
  [evt-series (->* ((-> any/c evt?))
                   (#:init any/c
                    #:then (-> any/c any))
                   #:rest (listof (-> any/c evt?))
                   evt?)]
  [evt-loop (->* ((-> any/c evt?)) (#:init any/c) evt?)]))

(define (evt-set . evts)
  (define results null)
  (define (handle e)
    (handle-evt e (λ (v) (cons e v))))
  (define (recur es)
    (if (null? es)
        (handle-evt always-evt (λ _ (map (curry dict-ref results) evts)))
        (replace-evt (apply choice-evt (map handle es))
                     (λ (e+v)
                       (set! results (cons e+v results))
                       (recur (remq (car e+v) es))))))
  (recur evts))

(define (evt-sequence #:then [make-result values] make-evt0 . make-evts)
  (define-values (lhs rhs) (list:split-at-right make-evts 1))
  (set! make-evts
    (append
     lhs
     (list (λ () (handle-evt ((car rhs)) make-result)))))
  (foldl (λ (make-evt evt) (replace-evt evt (λ _ (make-evt))))
         (make-evt0)
         make-evts))

(define (evt-series #:init [init (void)]
                    #:then [make-result values]
                    make-evt0 . make-evts)
  (define-values (lhs rhs) (list:split-at-right make-evts 1))
  (set! make-evts
    (append
     lhs
     (list (λ vs (handle-evt (apply (car rhs) vs) make-result)))))
  (foldl (λ (make-evt evt) (replace-evt evt (λ (v) (make-evt v))))
         (make-evt0 init)
         make-evts))

(define (evt-loop #:init [init (void)] next-evt)
  (replace-evt (next-evt init) (λ (v) (evt-loop #:init v next-evt))))

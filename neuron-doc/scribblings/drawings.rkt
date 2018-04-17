#lang racket/base

(require pict
         pict/convert
         racket/class
         racket/draw
         racket/function
         racket/match
         scribble/base
         (only-in scribble/core make-style)
         (only-in scribble/html-properties make-css-addition)
         (only-in scribble/manual racket))

(provide (all-defined-out))

(define FONT-SIZE 16)
(define BIG-FONT-SIZE 30)
(define MIN-CHANNEL-WIDTH 70)
(define MIN-EDGE-LEN 60)

;; (define full-width-style
;;   (make-style
;;    "FullWidth"
;;    (list (make-css-addition "scribblings/full-width.css"))))

(define blocks-style
  (make-style "Blocks" (list (make-css-addition "scribblings/blocks.css"))))

(define leading-spaces-style
  (make-style
   "LeadingSpaces"
   (list (make-css-addition "scribblings/leading-spaces.css"))))

(define (draw pict)
  (if (pict-convertible? pict) (pict-convert pict) pict))

;; Drawings

(define (pad pict [t #f] [r #f] [b #f] [l #f])
  (define w (pict-width pict))
  (define h (pict-height pict))
  (set! t (or t 5))
  (set! r (or r 5))
  (set! b (or b t 5))
  (set! l (or l r 5))
  (hc-append
   (blank l (+ h t b))
   (vc-append (blank w t) pict (blank w b))
   (blank r (+ h t b))))

(define (label content)
  (text content 'roman FONT-SIZE))

;; Block Diagrams

(define-syntax-rule (block-diagram [content ...] ...)
  (tabular
   #:cell-properties '(((center border)))
   (list (list content ...) ...)))

(define-syntax-rule (blocks [content ...] ...)
  (tabular
   #:style blocks-style
   (list (list content ...) ...)))

(define (block str)
  (pad (label str) 10 25))

;; Semantic Diagrams

(define val (compose pad label))

(struct ~channel
  (label in out)
  #:property prop:pict-convertible
  (match-lambda
    [(~channel lbl ch-in ch-out)
     (define pict (hc-append ch-in lbl ch-out))
     (set! pict (pin-line pict ch-out lt-find ch-in lt-find))
     (set! pict (pin-line pict ch-in lt-find ch-in rc-find))
     (set! pict (pin-line pict ch-in rc-find ch-in lb-find))
     (set! pict (pin-line pict ch-in lb-find ch-out lb-find))
     (set! pict (pin-line pict ch-out lb-find ch-out rc-find))
     (set! pict (pin-line pict ch-out rc-find ch-out lt-find))
     pict]))

(define (node content)
  (define body (pad (label content) 10 10))
  (define w (pict-width body))
  (define h (pict-height body))
  (cc-superimpose
   (filled-rectangle w h #:draw-border? #f #:color "white")
   body
   (rectangle w h)))

(define (channel content)
  (define lbl (val content))
  (when (< (pict-width lbl) MIN-CHANNEL-WIDTH)
    (set! lbl (pad lbl 0 (/ (- MIN-CHANNEL-WIDTH (pict-width lbl)) 2))))
  (define w (pict-width lbl))
  (define h (pict-height lbl))
  (set! lbl (cc-superimpose
             (filled-rectangle w h #:draw-border? #f #:color "white")
             lbl))

  (define (draw-ch-in dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    (define top-fin (new dc-path%))
    (send top-fin move-to 0 0)
    (send top-fin line-to 5 0)
    (send top-fin line-to 5 (/ h 2))
    (send top-fin close)
    (define bottom-fin (new dc-path%))
    (send bottom-fin move-to 0 h)
    (send bottom-fin line-to 5 h)
    (send bottom-fin line-to 5 (/ h 2))
    (send bottom-fin close)
    (send dc set-brush (new brush% [color "white"] [style 'solid]))
    (send dc set-pen (new pen% [width 1] [color "white"]))
    (send dc draw-path top-fin dx dy)
    (send dc draw-path bottom-fin dx dy)
    (send dc set-brush old-brush)
    (send dc set-pen old-pen))
  (define ch-in (dc draw-ch-in 5 h))

  (define (draw-ch-out dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    (define path (new dc-path%))
    (send path move-to 0 0)
    (send path line-to 5 (/ h 2))
    (send path line-to 0 h)
    (send dc set-brush (new brush% [color "white"] [style 'solid]))
    (send dc set-pen (new pen% [width 1] [color "white"]))
    (send dc draw-path path dx dy)
    (send dc set-brush old-brush)
    (send dc set-pen old-pen))
  (define ch-out (dc draw-ch-out 5 h))

  (~channel lbl ch-in ch-out))

(define (big str)
  (text str null BIG-FONT-SIZE))

(define cdots (pad (big "···")))

(define (--> content
             lhs lhs-offset lhs-part lhs-find
             rhs rhs-offset rhs-part rhs-find)
  (define label-pict (pad (or content (blank)) 5 15 2 5))
  (define label-width (pict-width label-pict))
  (define lhs* (apply offset (cons (pict-convert lhs) lhs-offset)))
  (define rhs* (apply offset (cons (pict-convert rhs) rhs-offset)))
  (define edge-len (max (+ label-width 20) MIN-EDGE-LEN))
  (define pict (hc-append edge-len lhs* rhs*))
  (pin-arrow-line
   10 pict
   (if lhs-part (lhs-part lhs) (~offset-target lhs*))
   (or lhs-find rc-find)
   (if rhs-part (rhs-part rhs) (~offset-target rhs*))
   (or rhs-find lc-find)
   #:label label-pict))

(struct ~offset
  (target x y)
  #:property prop:pict-convertible
  (match-lambda
    [(~offset tgt x y)
     (pad tgt (max y 0) (- (min x 0)) (- (min y 0)) (max x 0))]))

(define (offset tgt [x 0] [y 0])
  (~offset (draw tgt) x y))

(define (intersperse y xs)
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (list* (car xs) y (intersperse y (cdr xs)))))

(define (seq . picts)
  (apply (curry hc-append 10)
         (intersperse (big ";") picts)))

(struct ~exchanger
  (name ctrl data)
  #:property prop:pict-convertible
  (match-lambda
    [(~exchanger name ctrl data)
     (define body
       (vc-append (pad name 10 10 5 10) (pad ctrl 0 10) (pad data 0 10 10 10)))
     (cc-superimpose
      (filled-rounded-rectangle
       (pict-width body)
       (pict-height body)
       -0.125 #:color "white")
      body)]))

(define (exchanger [name "ex"] [ctrl "ctrl"] [data "data"])
  (~exchanger
   (label name)
   (draw (channel ctrl))
   (draw (channel data))))

(define (ref name)
  (define body (val name))
  (cc-superimpose
   (filled-rounded-rectangle
    (pict-width body)
    (pict-height body)
    #:color "white")
   body))

;; Document Structure

(define-syntax-rule (named-picts [name pict] ... [last-name last-pict])
  (tabular
   #:style leading-spaces-style
   #:cell-properties '((center))
   (append
    (list
     (list (draw name) pict)
     (list (blank 25) 'cont))
    ...
    (list
     (list (draw last-name) last-pict)))))

(define-syntax-rule (named-seqs [name pict1 picts ...] ...
                                [last-name last-pict1 last-picts ...])
  (tabular
   #:style leading-spaces-style
   #:cell-properties '((center))
   (append
    (list
     (list (draw name) (seq pict1 picts ...))
     (list (blank 25) 'cont))
    ...
    (list
     (list (draw last-name) (seq last-pict1 last-picts ...))))))

(define-syntax-rule (define/picts [code ...] pict ... last-pict)
  (tabular
   #:style 'boxed
   #:cell-properties '((center))
   (append
    (list
     (list (racket code ...))
     (list (blank 10)))
    (list
     (list (draw pict))
     (list (blank 25)))
    ...
    (list
     (list (draw last-pict))))))

(define-syntax-rule (define*/picts
                      ([code0 ...] pict0 ... last-pict0) ...
                      ([code ...] pict ... last-pict))
  (tabular
   #:style 'boxed
   #:cell-properties '((center))
   (append
    (append
     (list
      (list (racket code0 ...))
      (list (blank 10)))
     (list
      (list (draw pict0))
      (list (blank 25)))
     ...
     (list
      (list (draw last-pict0))
      (list (blank 10))))
    ...
    (append
     (list
      (list (blank 20))
      (list (racket code ...))
      (list (blank 10)))
     (list
      (list (draw pict))
      (list (blank 25)))
     ...
     (list
      (list (draw last-pict)))))))

;; Channel Operations

(define (ch-put v ch)
  (--> (label "put")
       (val v) '(0 0) #f #f
       (channel ch) '(0 0) #f #f))

(define (ch-get ch v)
  (--> (label "get")
       (channel ch) '(0 0) #f #f
       (val v) '(0 0) #f #f))

;; Exchanger Operations

(define (offer* ex1 ex2 [ofs '(0 -5)])
  (--> (label "offer")
       ex1 ofs #f #f
       ex2 '(0 0) ~exchanger-ctrl #f))

(define (accept* ex ex* [ofs '(0 -5)])
  (--> (label "accept")
      ex '(0 0) ~exchanger-ctrl #f
      ex* ofs #f #f))

(define (put* v ex [ofs '(0 52)])
  (--> (label "put")
       v ofs #f #f
       ex '(0 0) ~exchanger-data #f))

(define (get* ex v [ofs '(0 52)])
  (--> (label "get")
       ex '(0 0) ~exchanger-data #f
       v ofs #f #f))

(define (offer ex1 #:to ex2)
  (offer* (ref ex1) (exchanger ex2)))

(define (accept #:from ex ex*)
  (accept* (exchanger ex) (ref ex*)))

(define (put v #:into ex)
  (put* (val v) (exchanger ex)))

(define (get #:from ex v)
  (get* (exchanger ex) (val v)))

#lang racket/base

(require pict
         racket/function
         scribble/base
         (only-in scribble/core make-style)
         (only-in scribble/html-properties make-css-addition))

(provide (all-defined-out))

(define full-width-style
  (make-style
   "FullWidth"
   (list (make-css-addition "scribblings/full-width.css"))))

(define leading-spaces-style
  (make-style
   "LeadingSpaces"
   (list (make-css-addition "scribblings/leading-spaces.css"))))

(define (bounded w h pict
                 #:t [t (hline w 0)]
                 #:r [r (vline 0 h)]
                 #:b [b (hline w 0)]
                 #:l [l (vline 0 h)]
                 #:bg [bg "white"])
  (define T (curryr ct-superimpose t))
  (define R (curryr rc-superimpose r))
  (define B (curryr cb-superimpose b))
  (define L (curryr lc-superimpose l))
  (cc-superimpose
   (T (R (B (L (filled-rectangle w h #:color bg #:draw-border? #f)))))
   pict))

(define (layer w h str
               #:t [t (hline w 0)]
               #:r [r (vline 0 h)]
               #:b [b (hline w 0)]
               #:l [l (vline 0 h)]
               #:bg [bg "white"]
               #:fg [fg "black"])
  (bounded w h (colorize (text str 'roman 14) fg) #:t t #:r r #:b b #:l l #:bg bg))

(define (label w h str #:fg [fg "black"])
  (layer w h str #:t (blank) #:r (blank) #:b (blank) #:l (blank) #:fg fg))

(define (channel w h name)
  (define p (blank 10 h))
  (set! p (pin-line p p rt-find p lt-find))
  (set! p (pin-line p p lt-find p cc-find))
  (set! p (pin-line p p cc-find p lb-find))
  (set! p (pin-line p p lb-find p rb-find))
  (define q (blank 10 h))
  (set! q (pin-line q q lt-find q ct-find))
  (set! q (pin-line q q ct-find q rc-find))
  (set! q (pin-line q q rc-find q cb-find))
  (set! q (pin-line q q cb-find q lb-find))
  (hc-append p (layer w h #:r (blank) #:l (blank) name) q))

(define (exchanger [name "ex"] [ctrl #f] [data #f])
  (bounded
   80 85
   (vc-append
    5
    (text name 'roman 14)
    (vc-append (or ctrl (channel 35 25 "ctrl"))
               (or data (channel 35 25 "data")))
    (blank))))

(define (edge pict lhs lhs-find rhs rhs-find
              #:label [label (blank)]
              #:start-angle [start-angle 0]
              #:end-angle [end-angle 0])
  (pin-arrow-line
   8
   pict lhs lhs-find rhs rhs-find
   #:label label
   #:start-angle start-angle
   #:end-angle end-angle))

(define (offer name1 #:to name2)
  (define ex1 (layer 45 35 name1))
  (define ex2-ctrl (channel 35 25 "ctrl"))
  (define ex2 (exchanger name2 ex2-ctrl))
  (edge
   (hc-append 80 (vc-append ex1 (blank 12)) ex2)
   ex1 rc-find
   ex2-ctrl lc-find
   #:label (text "offer" null 14)))

(define (accept #:from name1 name2)
  (define ex-ctrl (channel 35 25 "ctrl"))
  (define ex (exchanger name1 ex-ctrl))
  (define ex* (layer 45 35 name2))
  (edge
   (hc-append 80 ex (vc-append ex* (blank 12)))
   ex-ctrl rc-find
   ex* lc-find
   #:label (text "accept" null 14)))

(define (ch-put name1 #:into name2)
  (define v (cc-superimpose (blank 25 35) (text name1 'roman 14)))
  (define ch (channel 35 25 name2))
  (edge
   (hc-append 80 v ch)
   v rc-find
   ch lc-find
   #:label (text "put" null 14)))

(define (ch-get #:from name1 name2)
  (define ch (channel 35 25 name1))
  (define v (cc-superimpose (blank 25 35) (text name2 'roman 14)))
  (edge
   (hc-append 80 ch v)
   ch rc-find
   v lc-find
   #:label (text "get" null 14)))

(define (punct str)
  (text str null 40))

(define (put name1 #:into name2)
  (define ex-data (channel 35 25 "data"))
  (define v (cc-superimpose (blank 25 35) (text name1 'roman 14)))
  (define ex (exchanger name2 #f ex-data))
  (edge
   (hc-append 70 (vc-append (blank 38) v) ex)
   v rc-find
   ex-data lc-find
   #:label (text "put" null 14)))

(define (get #:from name1 name2)
  (define ex-data (channel 35 25 "data"))
  (define ex (exchanger name1 #f ex-data))
  (define v (cc-superimpose (blank 25 35) (text name2 'roman 14)))
  (edge
   (hc-append 70 ex (vc-append (blank 38) v))
   ex-data rc-find
   v lc-find
   #:label (text "get" null 14)))

(define (intersperse y xs)
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (list* (car xs) y (intersperse y (cdr xs)))))

(define (seq . picts)
  (apply (curry hc-append 20)
         (intersperse (text ";" null 40) picts)))

(define-syntax-rule (named-seqs [name pict1 picts ...] ...
                                [last-name last-pict1 last-picts ...])
  (tabular
   #:style leading-spaces-style
   #:cell-properties '((center))
   (append
    (list
     (list (text name 'roman 14) (seq pict1 picts ...))
     (list (blank 25) 'cont))
    ...
    (list
     (list (text last-name 'roman 14) (seq last-pict1 last-picts ...))))))

(define (code-pict-def code pict)
  (tabular
   #:style 'boxed
   #:cell-properties '((center))
   (list
    (list code)
    (list (blank 10))
    (list pict))))

(define-syntax-rule (code-pict-defs [code pict] ... [last-code last-pict])
  (tabular
   #:style 'boxed
   #:cell-properties '((center))
   (append
    (list (list code)
          (list (blank 10))
          (list pict)
          (list (blank 25)))
    ...
    (list (list last-code)
          (list (blank 10))
          (list last-pict)))))

(define-syntax-rule (picts pict ... last-pict)
  (tabular
   #:style 'RBoxed
   #:cell-properties '((center))
   (append
    (list (list pict)
          (list (blank 25)))
    ...
    (list (list last-pict)))))

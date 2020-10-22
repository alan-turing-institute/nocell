#lang racket/base

(require racket/contract)
(require "builtins.rkt")

#| 

Grid is "assembly language for spreadsheets". 

This module exports structure definitions which define a Grid programme

|#

(provide
 ;; Predicates for simple types that are not structs
 label?
 expression?
 value?
 atomic-value?
 error?
 nothing?)


;; --- Programs, sheets, and cells

(struct program (sheets) #:transparent)
(struct sheet (rows) #:transparent)
(struct cell (xpr) #:transparent)
(struct labelled-cell cell (lbl) #:transparent)

(provide
 (contract-out
  (struct program
    ([sheets (listof sheet?)]))
  (struct sheet
    ([rows (listof (listof cell?))]))
  (struct cell
    ([xpr expression?]))
  (struct labelled-cell
    ([xpr expression?]
     [lbl string?]))))

(define label? string?)


;; -- Expressions and values

(struct application (fn args) #:transparent)
(struct matrix (rows) #:transparent)

(provide
 (contract-out
  (struct application
    ([fn   builtin?]
     [args (listof expression?)]))
  (struct matrix
    ([rows (vectorof (vectorof atomic-value? #:flat? #t) #:flat? #t)]))))

(define (expression? v)
  (or/c
   value?
   application?))

(define (value? v)
  (or/c
   atomic-value? 
   matrix?      
   reference?))

(define (atomic-value? v)
  (or/c
   number?
   string?
   boolean?
   error?
   nothing?))

(define (nothing? v)
  (eq? v 'nothing))

(define (error? v)
  (or/c 'error:arg 'error:undef 'error:val))


;; --- References

(struct reference
  ()
  #:transparent)
(struct cell-reference reference
  (loc)
  #:transparent)
(struct range-reference reference
  (tl br)
  #:transparent)

(struct location
  ()
  #:transparent)
(struct absolute-location location
  (label)
  #:transparent)
(struct relative-location location
  (source target)
  #:transparent)

(provide
 (contract-out
  (struct reference ())
  (struct (cell-reference reference) ([loc location?]))
  (struct (range-reference reference) ([tl location?] [br location?]))
  (struct location ())
  (struct (absolute-location location) ([label string?]))
  (struct (relative-location location) ([source string?] [target string?]))))




;; ---------------------------------------------------------------------------------------------------
;; Utility functions

;; matrix-ref : matrix? exact-nonnegative-integer? exact-nonnegative-integer? -> atomic-value? 
(define (matrix-ref m row col)
  (vector-ref
   (vector-ref (matrix-rows m) row)
   col))



;; ---------------------------------------------------------------------------------------------------
;; Unit tests

(module+ test
  (require rackunit)

  (define avs
    (list 42.0 "foo" #t #f 'error:arg 'error:val 'error:undef 'nothing))
  
  (for ([v avs])
    (check-pred atomic-value? v)
    (check-pred value? v)
    (check-pred expression? v))

  (define m (matrix #(#(0.0 1.0) #(2.0 3.0))))
  (check-pred value? m)
  (check-pred expression? m)
  (check-equal? (matrix-ref m 1 0) 2.0)

  (define rs
    (list (cell-reference  (absolute-location "the-cell"))
          (range-reference (absolute-location "the-cell-tl") (absolute-location "the-cell-br"))
          (cell-reference  (relative-location "the-cell-src" "the-cell-tgt"))
          (range-reference (absolute-location "the-cell")
                           (relative-location "the-cell-src" "the-cell-tgt"))))

  (for ([r rs])
    (check-pred reference? r)
    (check-pred value? r)
    (check-pred expression? r))

  )

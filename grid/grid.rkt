#lang racket/base

(require racket/contract)

(require "builtins.rkt")

#| 

Grid is "assembly language for spreadsheets". 

This module exports structure definitions which define a Grid programme

|#

(provide
 (all-from-out "builtins.rkt"))

;; Predicates for simple types that are not structs
(provide
 label?
 expression?
 value?
 atomic-value?
 error?
 nothing?
 matrix-ref)


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

(define (nothing? v)
  (eq? v 'nothing))

(define error?
  (or/c 'error:arg
        'error:undef
        'error:val))

(define atomic-value?
  (or/c
   number?
   string?
   boolean?
   error?
   nothing?))

(define value?
  (or/c
   atomic-value?
   matrix?
   reference?))

(define expression?
  (or/c
   value?   
   application?))


;; ---------------------------------------------------------------------------------------------------
;; Utility functions

;; matrix-ref : matrix? exact-nonnegative-integer? exact-nonnegative-integer? -> atomic-value? 
(define (matrix-ref m row col)
  (vector-ref
   (vector-ref (matrix-rows m) row)
   col))



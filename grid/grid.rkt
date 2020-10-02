#lang racket/base

#| 

Grid is "assembly language for spreadsheets". 

This module exports structure definitions which define a Grid programme

|#

(provide
 (struct-out programme)
 (struct-out sheet)
 (struct-out cell)
 (struct-out labelled-cell)
 label?
 expression?
 (struct-out application)
 (struct-out reference)
 (struct-out cell-reference)
 (struct-out range-reference)
 (struct-out location)
 (struct-out absolute-location)
 (struct-out relative-location)
 (struct-out matrix)
 atomic-value?
 error?)

;; sheets : list-of sheet?
(struct programme (sheets) #:transparent)

;; rows : list-of list-of cell?
(struct sheet (rows) #:transparent)

;; xpr : expression?
;; lbl : (or/c string? #f)
(struct cell (xpr) #:transparent)
(struct labelled-cell cell
  (lbl)
  #:transparent)

(define label? string?)

(define (expression? v)
  (or
   (value? v)
   (application? v)))

;; fn : symbol?
;; args : list-of expression?
(struct application (fn args) #:transparent)

(define (value? v)
  (or
   (atomic-value? v)
   (matrix?       v)
   (reference?    v)))

;; matrix-rows is a vector-of vector-of atomic-value?
(struct matrix
  (rows)
  #:transparent)

(define (matrix-ref m row col)
  (vector-ref
   (vector-ref (matrix-rows m) row)
   col))

(struct reference () #:transparent)
(struct cell-reference reference
  (loc)
  #:transparent)
(struct range-reference reference
  (tl br)
  #:transparent)

(struct location () #:transparent)
(struct absolute-location location
  (label)
  #:transparent)
(struct relative-location location
  (source target)
  #:transparent)

(define (atomic-value? v)
  (or
   (number?  v)
   (string?  v)
   (boolean? v)
   (error?   v)
   (eq? v 'nothing))
  )

(define (error? v)
  (or
   (eq? v 'error:arg)
   (eq? v 'error:undef)
   (eq? v 'error:val)))

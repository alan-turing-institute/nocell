#lang racket

(require "./grid.rkt")
;grid by hand,
; 5 factorial.

;Example 1
;factorial grid sheet spreadsheet, two columns, A = input, B=factorial output
(define (factorial x [tot 1])
  (if (= x 0) tot (factorial (sub1 x) (* tot x))))

(define (factorial_rows x)
  (for/list ([i (in-range 1 (add1 x))])
        (list (cell i)
              (cell (factorial i)))))

(define (factorial_prog x)
  (program
   (list
    (sheet
     (factorial_rows x)))))

(display (factorial_prog 5))

;Example 2
;factorial grid spreadsheet using references and formulas.


;cell that is an expression, that is a value, that is a numeric.
(define cell1 (cell 1)) ;numeric
(define cell2 (labelled-cell 2 "2nd")) ;labelled numeric
(define cell3 (cell "three")) ;string

;(define cell4 (cell (cell-reference (absolute-location "A1"))))
(define absloc (absolute-location "A1"))
(location? absloc)
(define cr (cell-reference absloc))
(reference? cr)
(value? cr)
(cell cr)
(define cell4 (cell cr)) ;absolute cell reference
(define cell5 (cell (cell-reference (relative-location "A1" "B1")))) ;absolute cell reference


(define sheet1 (sheet (list (list cell1 cell2 cell3 cell4 cell5))))
(define prog1 (program (list sheet1)))

(display prog1)
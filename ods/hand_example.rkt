#lang racket

(require "../grid/grid.rkt")
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

(provide factorial_prog)

;Example 2
;factorial grid spreadsheet using references and formulas.
;TODO
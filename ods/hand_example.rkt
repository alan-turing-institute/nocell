#lang racket

(require "../grid/grid.rkt"
         "./ods.rkt")
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

(define factorial_sheet (factorial_prog 5))

;test flat_ods
(define flat_sheet (prog->fsxml factorial_sheet))
(fsxml->fods flat_sheet)

;test ext_ods
(define extended_sheet (prog->esxml factorial_sheet))
(esxml->eods extended_sheet #:open #t)

;Example 2
;factorial grid spreadsheet using absolute references. Two repeated columns
(define myprog (program (list (sheet
                               (list
                                (list (labelled-cell 1 "A1") (cell (cell-reference (absolute-location "A1"))))
                                (list (labelled-cell 2 "A2") (cell (cell-reference (absolute-location "A2"))))
                                (list (labelled-cell 3 "A3") (cell (cell-reference (absolute-location "A3"))))
                                (list (labelled-cell 4 "A4") (cell (cell-reference (absolute-location "A4")))))))))

(define refprog (prog->fsxml myprog))
(fsxml->fods refprog #:path "ref_ods.xml")

  
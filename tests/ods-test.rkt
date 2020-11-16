#lang racket/base

#|

Tests for the grid-to-ods backend.

TODO:
- range references within formulas.
- support for all builtins.

|#


(require "../ods/ods.rkt")

;; ---------------------------------------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit)

  ;; Atomic values: number? string? boolean? error? nothing?

  ;; number?
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 1)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "float") (office:value "1")))))))
  
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 42.0)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "float") (office:value "42.0")))))))
 

  ;; string?
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell "")))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "string") (office:string-value "")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell "hello")))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "string") (office:string-value "hello")))))))

  
  ;; boolean?
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell #t)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "boolean") (office:boolean-value "true")))))))


  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell #f)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "boolean") (office:boolean-value "false")))))))

   
  ;; nothing?
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 'nothing)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell)))))

  ;; error?
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 'error:arg)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "#VALUE!")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 'error:arg)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "#VALUE!")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 'error:undef)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "#N/A")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 'error:val)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "#N/A")))))))

  ;; application?
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell (application '+ '(10)))))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "+10")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell (application '+ '(10 20)))))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "10+20")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell (application '* '(10 20)))))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "10*20")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell (application '/
                                                     `(10 ,(application '* '(20 30)))))))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "10/(20*30)")))))))

  ;; define a cell-hash to test references within applications.
  (define cell-refs (locate-labelled-cells
                     (sheet
                      (list
                       (list (labelled-cell "1" "cellA1") (labelled-cell "2" "cellB1"))
                       (list (labelled-cell "3" "cellA2") (labelled-cell "4" "cellB2"))))))

  (check-equal? cell-refs
                (hash "cellA1" (indices 0 0)
                      "cellB1" (indices 0 1)
                      "cellA2" (indices 1 0)
                      "cellB2" (indices 1 1)))
    

  ;;references within applications
  ;; in the above cell-hash cell1 is 00 and cell4 is 10, so indices 02 should return 12 (i.e."=C2
  (check-equal?
   (build-openformula
    (application '/
                 (list
                  (cell-reference (absolute-location "cellB1"))
                  (application '*
                               (list
                                (cell-reference (relative-location "cellA2" "cellA1"))
                                30))))
    (indices 2 1) ;B3
    #:cell-hash cell-refs)
   "$B$1/(B2*30)")


  ;;cell-hash test
  ;;use below cell-hash in cell-reference? tests.
  (define cell-hash (locate-labelled-cells
                     (sheet
                      (list
                       (list (labelled-cell "" "cell1") (cell "") (labelled-cell "" "cell3"))
                       (list (labelled-cell "" "cell4") (labelled-cell "" "cell5") (cell ""))))))

  (check-equal? cell-hash
                (hash "cell1" (indices 0 0)
                      "cell3" (indices 0 2)
                      "cell4" (indices 1 0)
                      "cell5" (indices 1 1))) 


  
  ;;cell-reference? absolute-location? test
  (define absolute-cell (cell (cell-reference (absolute-location "cell5"))))

  (check-equal?
   (grid-reference->openformula (cell-xpr absolute-cell) #:cell-hash cell-hash)
   "$B$2")
  (check-equal?
   (grid-reference->openformula (cell-reference (absolute-location "nonexistent"))
                                #:cell-hash cell-hash)
   "#N/A")

  (check-equal? (get-absolute-formula (indices 10 5)) "$F$11")

  ;;cell-reference relative-location? test
  ;; in the above cell-hash cell1 is 00 and cell4 is 10, so indices 02 should return 12 (i.e."=C2")
  (define relative-cell (cell (cell-reference (relative-location "cell1" "cell4"))))
  (check-equal?
   (grid-reference->openformula (cell-xpr relative-cell) (indices 0 2) #:cell-hash cell-hash)
   "C2")

  (check-equal? (get-relative-formula (indices 20 10)) "K21")

  (check-equal? (get-relative-formula (indices 0 -1)) "#N/A")
  (check-equal? (get-relative-formula (indices -1 0)) "#N/A")
  (check-equal? (get-relative-formula (indices -1 -1)) "#N/A")
  (check-equal? (get-relative-formula (indices 0 0)) "A1")

  ;;range-reference?
  (check-equal?
   (grid-reference->openformula
    (range-reference (absolute-location "cell1") (absolute-location "cell3"))
    #:cell-hash cell-hash)
   "$A$1:$C$1")
  
  ;;multiple cells test
  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 1)) (list (cell 2)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "float") (office:value "1"))))
      (table:table-row
       (table:table-cell (@ (office:value-type "float") (office:value "2")))))))
  ) 

#lang racket

;output basic grid sheets to ods.
(require sxml
         "./namespaces.rkt"
         "./hand_example.rkt"
         "../grid/grid.rkt")

;; can save flat directly to xml
(define (save_fods sheet #:path [path "flat_ods.xml"]
                    #:open [open #t])
  (call-with-output-file path #:exists 'replace
    (lambda (out) (srl:sxml->xml sheet out)))
  (if open (system  ("open -a \"LibreOffice\" " path) (void)))


(define PI `(*PI* xml "version=\"1.0\" encoding=\"UTF-8\""))
(define MIME "application/vnd.oasis.opendocument.spreadsheet")
(define TYPE `(@ (office:version "1.2") (office:mimetype ,MIME)))

;;basic one-cell flat ods.
;;how do I want to be able to specify a cell? 
(define (flatsheet prog)
  `(*TOP* ,NS ,PI
                 (office:document ,TYPE
                    (office:body
                     ,@(map sheet-sxml (program-sheets prog))))))

; each cell is a quasiquote entry with unqoute splicing
(define (cell-sxml cell)
 `(table:table-cell (@ (office:value ,(~a (cell-xpr cell))) (office:value-type "float")))) ;the type should return different values depending on cell-xpr

(define (row-sxml row)
  `(table:table-row ,@(map cell-sxml row)))

(define (sheet-sxml sheet)
  `(office:spreadsheet
     (table:table
      ,@(map row-sxml (sheet-rows sheet)))))

(define factorial_sheet (factorial_prog 5))

(define flat_sheet (flatsheet factorial_sheet))
(save_fods flat_sheet)
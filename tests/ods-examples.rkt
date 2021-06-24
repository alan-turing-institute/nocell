#lang racket/base

(require "./sheet-examples.rkt"
         "../sheet/ods/ods.rkt")

(let ([multi-table-sxml (sheet-spreadsheet->sxml multiplication-table)])

  (let ([fods-bytes (sxml->ods multi-table-sxml #:type 'fods)])
    (bytes->file fods-bytes "multi-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods multi-table-sxml #:type 'ods)])    
    (bytes->file ods-bytes "multi-extended-bytes.ods")))


(let ([bubbly-sxml (sheet-spreadsheet->sxml bubbly)])
  
  (let ([fods-bytes (sxml->ods bubbly-sxml #:type 'fods)])
    (bytes->file fods-bytes "bubbly-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods bubbly-sxml #:type 'ods)])
    (bytes->file ods-bytes "bubbly-extended-bytes.ods")))


#|
;; currently this fails because the column-widths function uses map, which assumes a rectangular list.
(let ([budget-sxml (grid-program->sxml budget
                                       #:blank-rows-before '(2 0 0 0 1 0 1)
                                       #:blank-cols-before '(0 0 5))])
   (let ([fods-bytes (sxml->ods budget-sxml #:type 'fods)])
    (bytes->file fods-bytes "budget-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods budget-sxml #:type 'ods)])
    (bytes->file ods-bytes "budget-extended-bytes.ods")))
|#

#lang racket/base

(require "./grid-examples.rkt"
         "../ods/ods.rkt")

(let ([multi-table-sxml (grid-program->sxml multiplication-table)])

  (let ([fods-bytes (sxml->ods multi-table-sxml #:type "fods")])
    (bytes->file fods-bytes "multi-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods multi-table-sxml #:type "ods")])    
    (bytes->file ods-bytes "multi-extended-bytes.ods")))


(let ([bubbly-sxml (grid-program->sxml bubbly)])
  
  (let ([fods-bytes (sxml->ods bubbly-sxml #:type "fods")])
    (bytes->file fods-bytes "bubbly-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods bubbly-sxml #:type "ods")])
    (bytes->file ods-bytes "bubbly-extended-bytes.ods")))


(let ([budget-sxml (grid-program->sxml budget)])
   (let ([fods-bytes (sxml->ods budget-sxml #:type "fods")])
    (bytes->file fods-bytes "budget-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods budget-sxml #:type "ods")])
    (bytes->file ods-bytes "budget-extended-bytes.ods")))


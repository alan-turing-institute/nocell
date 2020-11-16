#lang racket/base

(require "./grid-examples.rkt"
         "../ods/ods.rkt")


(define (bytes->file bstr fn)
  (call-with-output-file fn #:exists 'replace
    (lambda (out) (write-bytes bstr out))))


(let ([multi-table-sxml (grid-program->sxml multiplication-table)])
  ;; save files
  ; old way - save direct. will still work.
  ;  (sxml->ods multi-table-sxml #:filename "multi-flat" #:type "fods")
  ; (sxml->ods multi-table-sxml #:filename "multi-extended" #:type "ods")
  ;;save by returning bytes 
  (let ([fods-bytes (sxml->ods-bytes multi-table-sxml #:type "fods")])
    (display fods-bytes)
    (bytes->file fods-bytes "multi-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods-bytes multi-table-sxml #:type "ods")])
    (display ods-bytes)
    (bytes->file ods-bytes "multi-extended-bytes.xml")))


    
(let ([bubbly-sxml (grid-program->sxml bubbly)])
  
  ;(sxml->ods bubbly-sxml #:filename "bubbly-flat" #:type "flat")
  ;(sxml->ods bubbly-sxml #:filename "bubbly-extended" #:type "extended"))

  (let ([fods-bytes (sxml->ods-bytes bubbly-sxml #:type "fods")])
    (bytes->file fods-bytes "bubbly-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods-bytes bubbly-sxml #:type "ods")])
    (bytes->file ods-bytes "bubbly-extended-bytes.ods")))


(let ([budget-sxml (grid-program->sxml budget)])
  ;(sxml->ods budget-sxml #:filename "budget-flat" #:type "f")
  ;(sxml->ods budget-sxml #:filename "budget-extended" #:type "e"))

   (let ([fods-bytes (sxml->ods-bytes budget-sxml #:type "fods")])
    (bytes->file fods-bytes "budget-flat-bytes.xml"))
  
  (let ([ods-bytes (sxml->ods-bytes budget-sxml #:type "ods")])
    (bytes->file ods-bytes "budget-extended-bytes.ods")))


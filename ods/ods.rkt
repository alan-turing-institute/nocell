#lang racket/base

(require sxml
         "./namespaces.rkt"
         "../grid/grid.rkt"
         "./column-letters.rkt"
         racket/format
         racket/function
         racket/file
         racket/string
         racket/list
         racket/system
         racket/contract)

#| 
A backend for grid which produces Open Document Format spreadsheets. 
See the github wiki page for accompanying notes: 
https://github.com/alan-turing-institute/nocell/wiki/Creating-ODF-files-from-scratch 

Note: the commands sxml->extended-ods, called externally from sxml->ods, require 
that an executable `zip` program is in the user's path. 

1. Interface
2. Conversion of grid program to sxml-program structure with content and styles.
3. Adding necessary headers to sxml-program for flat ods or extended ods. 
4. Serialisation - convert sxml-program to flat or extended ods (includes zipping).
|#

;; ---------------------------------------------------------------------------------------------------
;; Interface

(provide
 (contract-out
  [sxml->ods (->* (sxml-program?)
                  (#:filename string? #:type string?)
                   any)]
  [grid-program->sxml (-> program? sxml-program?)]))                   



;; ---------------------------------------------------------------------------------------------------
;; Conversion of grid program to sxml-program structure with content and styles.

(struct sxml-program (content styles) #:transparent)

(define (grid-program->sxml program)
  (sxml-program `(office:body
                  ,@(map grid-sheet->sxml (program-sheets program)))  
                '(office:styles
                  (style:style))))

(define (grid-sheet->sxml sheet)
  `(office:spreadsheet
    (table:table
     ,@(map (curry grid-row->sxml #:cell-hash (cell-hash sheet))
            (sheet-rows sheet)))))

(define (grid-row->sxml row #:cell-hash cell-hash)
  `(table:table-row
    ,@(map (curry grid-cell->sxml cell-hash) row)))

(define (grid-cell->sxml cell #:cell-hash cell-hash)
  (if (nothing? (cell-xpr cell))
      '(table:table-cell)
      `(table:table-cell , (grid-expression->sxml-attributes #:cell-hash cell-hash (cell-xpr cell))))) 

(define (grid-expression->sxml-attributes xpr #:cell-hash cell-hash)
  (cond
    [(string? xpr)
     `(@ (office:value-type "string") (office:string-value ,(~a xpr)))]

    [(number? xpr)
     `(@ (office:value-type "float") (office:value ,(~a xpr)))]

    [(boolean? xpr)
     `(@ (office:value-type "boolean") (office:boolean-value ,(if xpr "true" "false")))]

    [(error? xpr)
     `(@ (table:formula ,(hash-ref errors xpr)))]

    [(reference? xpr)
     (cell-references xpr #:cell-hash cell-hash)]

    [(application? xpr)
     (error "applications not yet supported")]
    
    [else (error "unrecognised type")]))


(define errors (make-hash (list (cons 'error:arg "of:=#VALUE!")
                                (cons 'error:undef "of:=#N/A")
                                (cons 'error:val "of:=#N/A"))))

(define (cell-references xpr #:cell-hash cell-hash)
  (cond
    [(cell-reference? xpr)
     (let ([loc (cell-reference-loc xpr)])
       (cond
         [(absolute-location? loc)
          ;; placeholder: this should eventually reference a cell-hash
          `(@ (table:formula ,(string-append
                               "="
                               (hash-ref cell-hash (absolute-location-label loc)))))]
            
         [(relative-location? loc) (error "relative location not yet supported")]))]
          
    [(range-reference? xpr) (error "range reference not yet supportedc")]))


;; builds a hash table of the labelled cells and their alphanumeric positions
(define (cell-hash sheet)
  ;; rows go 1,2,3,4 ...; columns go A,B,C,D...
  ;; if cell not a labelled-cell the hash points to itself.
  (define (column-name i j) (string-append (integer->column-letter j) (~a (add1 i))))
  (for*/hash ([(row i) (in-indexed (sheet-rows sheet))]
              [(cell j) (in-indexed row)])
    (if (labelled-cell? cell)
        (values (labelled-cell-lbl cell)
                (column-name i j))
        (values (column-name i j)
                (column-name i j)))))


;; ---------------------------------------------------------------------------------------------------
;; Adding necessary headers to sxml-program for flat ods or extended ods. 
;; Used in sxml->flat-ods and sxml->extended-ods

(define PI '(*PI* xml "version=\"1.0\" encoding=\"UTF-8\""))
(define MIME "application/vnd.oasis.opendocument.spreadsheet")
(define TYPE `(@ (office:version "1.3")
                 (office:mimetype ,MIME)))

(define (flat-sxml sxml-program)
  `(*TOP* ,NS ,PI
          (office:document ,TYPE
                           ,(sxml-program-content sxml-program)
                           ,(sxml-program-styles sxml-program))))

(define (extended-sxml sxml-program)
  (list MIME 
        (xml-content (sxml-program-content sxml-program))
        (xml-styles (sxml-program-styles sxml-program))
        xml-manifest))

(define (xml-content sxml-content)
  `(*TOP* ,NS ,PI
          (office:document-content ,TYPE
                                   ,sxml-content)))
  
(define (xml-styles sxml-styles)
  `(*TOP* ,NS ,PI
          (office:document-styles ,TYPE
                                  ,sxml-styles)))

(define xml-manifest
  `(*TOP* ,NS ,PI
          (manifest:manifest
           (@ (manifest:version "1.3"))
           (manifest:file-entry
            (@ (manifest:full-path "/") (manifest:version "1.3") (manifest:media-type ,MIME)))
           (manifest:file-entry 
            (@ (manifest:full-path "content.xml") (manifest:media-type "text/xml")))
           (manifest:file-entry 
            (@ (manifest:full-path "styles.xml") (manifest:media-type "text/xml"))))))

;; ---------------------------------------------------------------------------------------------------
;; Serialisation - convert sxml-program to flat or extended ods. Uses filesystem commands.
;; WARNING: uses (system zip) because Racket file/zip 
;; doesn't support leaving the first file in the archive uncompressed.
  
(define (sxml->ods sxml-program
                   #:filename [filename "flat_ods"]
                   #:type [type "flat"])
  (cond
    [(ormap (curry = type) '("flat" "fods" "f"))
     (sxml->flat-ods sxml-program filename)]

    [(ormap (curry = type) '("extended" "ods" "e"))
     (sxml->extended-ods sxml-program filename)]
      
    [else (error "unrecognised type")]))



(define (sxml->flat-ods sxml-program filename)
  (call-with-output-file (string-append filename ".xml") #:exists 'replace
    (lambda (out) (srl:sxml->xml (flat-sxml sxml-program) out))))



(define (sxml->extended-ods sxml-program filename)
  ;;save temporary files
  (make-directory* "META-INF") 
  (define filelist (list "mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml"))
  (for ([fn filelist]
        [xml (extended-sxml sxml-program)])
    (call-with-output-file fn #:exists 'replace
      (lambda (out) (srl:sxml->xml xml out))))
  
  ;;zip.
  (define odsfolder (string-append "\"" filename ".ods\""))
  (define cmdmime (string-join (list "zip -0 -X" odsfolder (first filelist)) " "))
  (system cmdmime)
  (for ([fn (rest filelist)])
    (define cmd (string-join (list "zip -r" odsfolder fn) " "))
    (system cmd))

  ;; Remove temporary files
  (map delete-file filelist))
  


;; -------------------------------- TESTS ----------------------

(module+ test
  (require rackunit)

  ;; Atomic values, number? string? boolean? error? nothing?

  ;; number?
  (check-equal? (grid-cell->sxml (cell 1))
                '(table:table-cell (@ (office:value-type "float") (office:value "1"))))
  (check-equal? (grid-cell->sxml (cell 42.0))
                '(table:table-cell (@ (office:value-type "float") (office:value "42.0"))))

  ;; string?
  (check-equal? (grid-cell->sxml (cell ""))
                '(table:table-cell (@ (office:value-type "string") (office:string-value ""))))
  (check-equal? (grid-cell->sxml (cell "hello"))
                '(table:table-cell (@ (office:value-type "string") (office:string-value "hello"))))
  
  ;; boolean?
  (check-equal? (grid-cell->sxml (cell #t))
                '(table:table-cell (@ (office:value-type "boolean") (office:boolean-value "true"))))
  (check-equal? (grid-cell->sxml (cell #f))
                '(table:table-cell (@ (office:value-type "boolean") (office:boolean-value "false"))))
   
  ;; nothing?
  (check-equal? (grid-cell->sxml (cell 'nothing)) '(table:table-cell))

  ;; error?
  (check-equal? (grid-cell->sxml (cell 'error:arg))
                '(table:table-cell (@ (table:formula "of:=#VALUE!")))) 
  (check-equal? (grid-cell->sxml (cell 'error:undef))
                '(table:table-cell (@ (table:formula "of:=#N/A"))))
  (check-equal? (grid-cell->sxml (cell 'error:val))
                '(table:table-cell (@ (table:formula "of:=#N/A")))) 

 

  #|
  ;; application?
  (check-equal? (cell->sxml (cell (+ 1 2))) '(table:table-cell (@ (table:formula "of:=1+2"))))
  |#


  ;;cell-hash test
  (check-equal? (cell-hash
                 (sheet
                  (list
                   (list (labelled-cell "" "cell1") (cell "") (labelled-cell "" "cell3"))
                   (list (labelled-cell "" "cell4") (labelled-cell "" "cell5") (cell "")))))
                #hash(("B1" . "B1")
                      ("C2" . "C2")
                      ("cell1" . "A1")
                      ("cell3" . "C1")
                      ("cell4" . "A2")
                      ("cell5" . "B2"))) ;at the moment the keys return alphabetically
  
  
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

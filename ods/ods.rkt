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
(struct indices (row column) #:transparent)

;; grid-program->sxml : program? -> sxml-program?
(define (grid-program->sxml program)
  (sxml-program `(office:body
                  ,@(map grid-sheet->sxml (program-sheets program)))  
                '(office:styles
                  (style:style))))

;; grid-sheet->sxml : sheet? -> string?
(define (grid-sheet->sxml sheet)

  (define cell-hash (locate-labelled-cells sheet))

  ;; grid-row->sxml : [listof cell?] integer? -> string?
  (define (grid-row->sxml row i)
    `(table:table-row
      ,@(for/list ([(cell j) (in-indexed row)])
          (grid-cell->sxml cell (indices i j)))))

  ;; grid-cell->sxml : cell? indices? -> string?
  (define (grid-cell->sxml cell pos)
    (if (nothing? (cell-xpr cell))
        '(table:table-cell)
        `(table:table-cell ,(grid-expression->sxml-attributes (cell-xpr cell) pos))))

  ;; grid-expression->sxml-attributes : expression? indices? -> string?
  (define (grid-expression->sxml-attributes xpr pos)
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
       `(@ (table:formula, (get-reference-formula xpr pos #:cell-hash cell-hash)))]

      [(application? xpr)
       (error "applications not yet supported")]
    
      [else (error "unrecognised type")]))

  `(office:spreadsheet
    (table:table
     ,@(for/list ([(row i) (in-indexed (sheet-rows sheet))])
         (grid-row->sxml row i)))))
 


(define errors (make-hash (list (cons 'error:arg "of:=#VALUE!")
                                (cons 'error:undef "of:=#N/A")
                                (cons 'error:val "of:=#N/A"))))

;;get-reference-formula : expression? indices? [hash-of label? indices?]
(define (get-reference-formula xpr [pos (indices 0 0)] #:cell-hash cell-hash)
  (cond
    [(cell-reference? xpr)
     (let ([loc (cell-reference-loc xpr)])
       (cond
         [(absolute-location? loc)
          (if (hash-has-key? cell-hash (absolute-location-label loc))
              (get-absolute-formula (hash-ref cell-hash (absolute-location-label loc)))
               "of:=#N/A")]
              
            
         [(relative-location? loc)
          (get-relative-formula (get-referent-indices loc pos #:cell-hash cell-hash))]))]
          
    [(range-reference? xpr) (error "range reference not yet supported")]))

;; get-absolute-formula : indices? -> string?
(define (get-absolute-formula pos)
      (string-append "of:=$"
                     (integer->column-letter (indices-column pos))
                     "$"
                     (~a (add1 (indices-row pos)))))


;; get-relative-formula : indices? -> string?
(define (get-relative-formula pos)
  (if (and (>= (indices-row pos) 0)
           (>= (indices-column pos) 0))
      (string-append "of:="
                     (integer->column-letter (indices-column pos))
                     (~a (add1 (indices-row pos))))
      "of:=#N/A"))
   

;; get-referent-indices : indices? relative-location? hash? -> indices?
;; Apply relative location offset to current-position
(define (get-referent-indices location current-position #:cell-hash cell-hash)
  (define source-indices (hash-ref cell-hash (relative-location-source location)))
  (define target-indices (hash-ref cell-hash (relative-location-target location)))

  (define offset-row (- (indices-row target-indices) (indices-row source-indices)))
  (define offset-column (- (indices-column target-indices) (indices-column source-indices)))

  (define referent-row (+ (indices-row current-position) offset-row))
  (define referent-column (+ (indices-column current-position) offset-column))

  (indices referent-row referent-column))



;; locate-labelled-cells : sheet? -> [hash-of label? indices?]
;; Determine the locations of labelled cells
(define (locate-labelled-cells sheet)
  (for*/hash ([(row i) (in-indexed (sheet-rows sheet))]
              [(cell j) (in-indexed row)]
              #:when (labelled-cell? cell))
    (values (labelled-cell-lbl cell)
            (indices i j))))


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
  (system (string-join (list "zip -0 -X" odsfolder (first filelist)) " "))
  (for ([fn (rest filelist)])
    (system (string-join (list "zip -r" odsfolder fn) " ")))

  ;; Remove temporary files
  (map delete-file filelist))
  


;; -------------------------------- TESTS ----------------------

(module+ test
  (require rackunit)

  ;; Atomic values, number? string? boolean? error? nothing?

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
       (table:table-cell (@ (table:formula "of:=#VALUE!")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 'error:undef)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "of:=#N/A")))))))

  (check-equal?
   (grid-sheet->sxml (sheet
                      (list (list (cell 'error:val)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (table:formula "of:=#N/A")))))))


 

  #|
  ;; application?
  (check-equal? (cell->sxml (cell (+ 1 2))) '(table:table-cell (@ (table:formula "of:=1+2"))))
  |#


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
   (get-reference-formula (cell-xpr absolute-cell) #:cell-hash cell-hash)
   "of:=$B$2")
   (check-equal?
    (get-reference-formula (cell-reference (absolute-location "nonexistent"))
                            #:cell-hash cell-hash)
   "of:=#N/A")

  (check-equal? (get-absolute-formula (indices 10 5)) "of:=$F$11")

  ;;cell-reference relative-location? test
  ;; in the above cell-hash cell1 is 00 and cell4 is 10, so indices 02 should return 12 (i.e."=C2")
  (define relative-cell (cell (cell-reference (relative-location "cell1" "cell4"))))
  (check-equal?
   (get-reference-formula (cell-xpr relative-cell) (indices 0 2) #:cell-hash cell-hash)
   "of:=C2")

  (check-equal? (get-relative-formula (indices 20 10)) "of:=K21")
  (check-equal? (get-relative-formula (indices 1 2)) "of:=C2")

  (check-equal? (get-relative-formula (indices 0 -1)) "of:=#N/A")
  (check-equal? (get-relative-formula (indices -1 0)) "of:=#N/A")
  (check-equal? (get-relative-formula (indices -1 -1)) "of:=#N/A")
  (check-equal? (get-relative-formula (indices 0 0)) "of:=A1")
  
    
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

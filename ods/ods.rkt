#lang racket

(provide
 (contract-out
  [prog->fsxml (-> program? sxmlfile?)]
  [prog->esxml (-> program? esxmlfile?)]
  [fsxml->fods (->* (sxmlfile?)
                   (#:open boolean? #:path string?)
                   any)]
  [esxml->eods (->* (esxmlfile?)
                   (#:open boolean? #:dir string?)
                   any)]
  (struct esxmlfile ([files (listof sxmlfile?)]))))



;output basic grid sheets to ods.
(require sxml
         "./namespaces.rkt"
         "../grid/grid.rkt")

;----- HEADER INFO -----------

(define PI `(*PI* xml "version=\"1.0\" encoding=\"UTF-8\""))
(define MIME "application/vnd.oasis.opendocument.spreadsheet")
(define TYPE `(@ (office:version "1.3") (office:mimetype ,MIME)))


;-------SXML -> FLAT ODS FUNCTIONS -------------

;; can save flat directly to xml
(define (fsxml->fods sheet
                   #:path [path "flat_ods.xml"]
                   #:open [open #t])
  (call-with-output-file path #:exists 'replace
    (lambda (out) (srl:sxml->xml (sxmlfile-sexp sheet) out)))
  (if open (system  (string-append "open -a \"LibreOffice\" " path))
      (void)))

(struct sxmlfile (sexp) #:transparent)
(struct esxmlfile (files) #:transparent)

;;basic one-cell flat ods.
(define (prog->fsxml prog)
  (sxmlfile `(*TOP* ,NS ,PI
                 (office:document ,TYPE
                    (office:body
                     ,@(map sheet->sxml (program-sheets prog)))))))


;---------SXML -> EXTENDED ODS FUNCTIONS----------

;mostly hardcoded for now, but styles and manifest need to adapt to input
(define (contentxml prog)
  (sxmlfile `(*TOP* ,NS ,PI
                 (office:document-content ,TYPE
                    (office:body
                     ,@(map sheet->sxml (program-sheets prog)))))))
  
(define stylesxml
  (sxmlfile `(*TOP* ,NS ,PI
                 (office:document-styles ,TYPE
                    (office:styles
                     (style:style))))))

(define manifestxml
  (sxmlfile `(*TOP* ,NS ,PI
                 (manifest:manifest (@ (manifest:version "1.3"))
                    (manifest:file-entry (@ (manifest:full-path "/") (manifest:version "1.3") (manifest:media-type ,MIME)))
                    (manifest:file-entry (@ (manifest:full-path "content.xml") (manifest:media-type "text/xml")))
                    (manifest:file-entry (@ (manifest:full-path "styles.xml") (manifest:media-type "text/xml")))
                     ))))

(define (prog->esxml prog)
  (esxmlfile (list (sxmlfile MIME) (contentxml prog) stylesxml manifestxml)))

(define (esxml->eods odslist
                  #:dir [dir "ods_example"]
                  #:open [open #f])

  ;;first save all the files
  (make-directory* "META-INF") 
  (define fnlist (list "mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml"))
  (for ([fn fnlist]
        [sh (esxmlfile-files odslist)])
     (call-with-output-file fn #:exists 'replace
    (lambda (out) (srl:sxml->xml (sxmlfile-sexp sh) out))))
  
  ;;then zip them.
  (define odsfolder (string-append "\"" dir ".ods\""))
  (define cmdmime (string-join (list "zip -0 -X" odsfolder (first fnlist)) " "))
  (displayln cmdmime)
  (system cmdmime)
  (for ([fn (rest fnlist)])
    (define cmd (string-join (list "zip -r" odsfolder fn) " "))
    (displayln cmd)
    (system cmd))

  ;;then clear up.
   (for ([fn fnlist])(system (string-append "rm " fn)))
       
  ;; and open
  (if open (let* ([zip1 (string-append "open -a \"Microsoft Excel\" " odsfolder)]
                  [zip2 (string-append "open -a \"LibreOffice\" " odsfolder)])
             (displayln zip1)
             (displayln zip2)
             (system zip1)
             (system zip2))
      (void)))

;---------- GRID -> SXML ------------

; --- functions for different cell inputs

#|
TYPES                         DONE

number?                        x
string?                        x
boolean?     
error?       
nothing?
matrix?
reference?
- cell-reference
  - absolute-location
  - relative-location
- range-reference
  - absolute-location
  - relative-location
application? 
|#


(define (cellattr cell);change attributes depending on cell contents
  (define xpr (cell-xpr cell))
  (cond
    [(string? xpr)
     `(@ (office:value-type "string") (office:string-value ,(~a xpr)))]
    ;;
    [(number? xpr)
     `(@ (office:value-type "float") (office:value ,(~a xpr)))]
    ;;
    [(reference? xpr)
     (cond
       [(cell-reference? xpr)
        (let ([loc (cell-reference-loc xpr)])
          (println "here")
          (cond
            [(absolute-location? loc)
             `(@ (table:formula ,(string-append "=" (absolute-location-label loc))))]
            ;;
            [(relative-location? loc) (error "relative loc todo")]))]
          ;;
       [(range-reference? xpr) (error "range reference to do")])]
    ;;
    [else (error "unrecognised type")]))


;; ---- build to sxml
(define (cell->sxml cell)
  `(table:table-cell ,(cellattr cell))) ;the type should return different values depending on cell-xpr

(define (row->sxml row)
  `(table:table-row
    ,@(map cell->sxml row)))

(define (sheet->sxml sheet)
  `(office:spreadsheet
     (table:table
      ,@(map row->sxml (sheet-rows sheet)))))


(module+ test
  (require rackunit)
  (check-equal? (cellattr (cell "1")) `(@ (office:value-type "string") (office:string-value "1")))
  (check-equal? (cellattr (cell 1)) `(@ (office:value-type "float") (office:value "1")))
  (check-equal?
   (sheet->sxml (sheet
               (list (list (cell 1)) (list (cell 2)))))
   `(office:spreadsheet
     (table:table
      (table:table-row
       (table:table-cell (@ (office:value-type "float") (office:value "1"))))
      (table:table-row
       (table:table-cell (@ (office:value-type "float") (office:value "2")))))))
 ) 
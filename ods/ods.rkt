#lang racket


;output basic grid sheets to ods.
(require sxml
         "./namespaces.rkt"
         "./hand_example.rkt"
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
    (lambda (out) (srl:sxml->xml sheet out)))
  (if open (system  (string-append "open -a \"LibreOffice\" " path))
      (void)))

;;basic one-cell flat ods.
(define (prog->fsxml prog)
  `(*TOP* ,NS ,PI
                 (office:document ,TYPE
                    (office:body
                     ,@(map sheet->sxml (program-sheets prog))))))


;---------SXML -> EXTENDED ODS FUNCTIONS----------

;mostly hardcoded for now, but styles and manifest need to adapt to input
(define (contentxml prog)
  `(*TOP* ,NS ,PI
                 (office:document-content ,TYPE
                    (office:body
                     ,@(map sheet->sxml (program-sheets prog))))))
  
(define stylesxml `(*TOP* ,NS ,PI
                 (office:document-styles ,TYPE
                    (office:styles
                     (style:style)))))

(define manifestxml `(*TOP* ,NS ,PI
                 (manifest:manifest (@ (manifest:version "1.3"))
                    (manifest:file-entry (@ (manifest:full-path "/") (manifest:version "1.3") (manifest:media-type ,MIME)))
                    (manifest:file-entry (@ (manifest:full-path "content.xml") (manifest:media-type "text/xml")))
                    (manifest:file-entry (@ (manifest:full-path "styles.xml") (manifest:media-type "text/xml")))
                     )))

(define (prog->esxml prog)
  (list (contentxml prog) stylesxml manifestxml))

(define (esxml->eods odslist
                  #:name [dir "ods_example"]
                  #:open [open #f])

  ;;first save all the files
  (make-directory* "META-INF") 
  (define fnlist (list "mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml"))
  (define filelist (append (list MIME) odslist))
  (for ([fn fnlist]
        [sh filelist])
     (call-with-output-file fn #:exists 'replace
    (lambda (out) (srl:sxml->xml sh out))))
  
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

; each cell is a quasiquote entry with unqoute splicing

; --- functions for different cell inputs
(define (valtype cell)
  (define xpr (cell-xpr cell))
  (cond [(string? xpr) "string"]
        [(number? xpr) "float"]
        [else (error "unrecognised type")]))

(define (val cell)
  (define xpr (cell-xpr cell))
  (define str-xpr (~a xpr))
  (cond [(string? xpr) `(office:string-value ,str-xpr)]
        [(number? xpr) `(office:value ,str-xpr)]
        [else (error "unrecognised type")]))
 
(define (cell->sxml cell)
  `(table:table-cell (@ (office:value-type ,(valtype cell)) ,(val cell)))) ;the type should return different values depending on cell-xpr

(define (row->sxml row)
  `(table:table-row
    ,@(map cell->sxml row)))

(define (sheet->sxml sheet)
  `(office:spreadsheet
     (table:table
      ,@(map row->sxml (sheet-rows sheet)))))

(define factorial_sheet (factorial_prog 5))

;test flat_ods
(define flat_sheet (prog->fsxml factorial_sheet))
(fsxml->fods flat_sheet)

;test ext_ods
(define extended_sheet (prog->esxml factorial_sheet))
(esxml->eods extended_sheet #:open #t)


(module+ test
  (require rackunit)
  (check-equal? (val (cell "1")) `(office:string-value "1"))
  (check-equal? (val (cell 1)) `(office:value "1"))
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
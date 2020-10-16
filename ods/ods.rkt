#lang racket

;output basic grid sheets to ods.
(require sxml
         "./namespaces.rkt"
         "./hand_example.rkt"
         "../grid/grid.rkt")

;----- HEADER INFO -----------

(define PI `(*PI* xml "version=\"1.0\" encoding=\"UTF-8\""))
(define MIME "application/vnd.oasis.opendocument.spreadsheet")
(define TYPE `(@ (office:version "1.2") (office:mimetype ,MIME)))


;-------SXML -> FLAT ODS FUNCTIONS -------------

;; can save flat directly to xml
(define (save_fods sheet
                   #:path [path "flat_ods.xml"]
                   #:open [open #t])
  (call-with-output-file path #:exists 'replace
    (lambda (out) (srl:sxml->xml sheet out)))
  (if open (system  (string-append "open -a \"LibreOffice\" " path))
      (void)))

;;basic one-cell flat ods.
(define (flatsheet prog)
  `(*TOP* ,NS ,PI
                 (office:document ,TYPE
                    (office:body
                     ,@(map sheet-sxml (program-sheets prog))))))


;---------SXML -> EXTENDED ODS FUNCTIONS----------

;mostly hardcoded for now, but styles and manifest need to adapt to input
(define (contentxml prog)
  `(*TOP* ,NS ,PI
                 (office:document-content ,TYPE
                    (office:body
                     ,@(map sheet-sxml (program-sheets prog))))))
  
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

(define (extendedsheet prog)
  (list (contentxml prog) stylesxml manifestxml))

(define (save_ods odslist
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
(define (cell-sxml cell)
 `(table:table-cell (@ (office:value ,(~a (cell-xpr cell))) (office:value-type "float")))) ;the type should return different values depending on cell-xpr

(define (row-sxml row)
  `(table:table-row
    ,@(map cell-sxml row)))

(define (sheet-sxml sheet)
  `(office:spreadsheet
     (table:table
      ,@(map row-sxml (sheet-rows sheet)))))

(define factorial_sheet (factorial_prog 5))

;test flat_ods
(define flat_sheet (flatsheet factorial_sheet))
(save_fods flat_sheet)

;test ext_ods
(define extended_sheet (extendedsheet factorial_sheet))
(save_ods extended_sheet #:open #t)

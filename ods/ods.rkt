#lang racket/base

(require sxml
         "./namespaces.rkt"
         "../grid/grid.rkt"
         "../grid/builtins.rkt"
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
       `(@ (table:formula ,(grid-reference->openformula xpr pos #:cell-hash cell-hash)))]

      [(application? xpr)
       `(@ (table:formula ,(build-openformula xpr pos #:cell-hash cell-hash)))]
    
      [else (error "unrecognised type")]))

  `(office:spreadsheet
    (table:table
     ,@(for/list ([(row i) (in-indexed (sheet-rows sheet))])
         (grid-row->sxml row i)))))
 


(define errors (make-hash (list (cons 'error:arg "#VALUE!")
                                (cons 'error:undef "#N/A")
                                (cons 'error:val "#N/A"))))


;; ---------------------------------------------------------------------------------------------------
;; References

;;grid-reference->openformula : expression? indices? [hash-of label? indices?] -> string?
(define (grid-reference->openformula xpr [pos (indices 0 0)] #:cell-hash cell-hash)


  ;;resolve-location : location? -> string?
  (define (resolve-location loc)
    (cond
      [(absolute-location? loc)
       (if (hash-has-key? cell-hash (absolute-location-label loc))
           (get-absolute-formula (hash-ref cell-hash (absolute-location-label loc)))
           "#N/A")]
      
      [(relative-location? loc)
       (get-relative-formula (get-referent-indices loc pos #:cell-hash cell-hash))]))

    (cond
      [(cell-reference? xpr) (resolve-location (cell-reference-loc xpr))]
      [(range-reference? xpr)
       (string-append (resolve-location (range-reference-tl xpr))
                      ":"
                      (resolve-location (range-reference-br xpr)))]))

;; get-absolute-formula : indices? -> string?
(define (get-absolute-formula pos)
  (string-append "$"
                 (integer->column-letter (indices-column pos))
                 "$"
                 (~a (add1 (indices-row pos)))))


;; get-relative-formula : indices? -> string?
(define (get-relative-formula pos)
  (if (and (>= (indices-row pos) 0)
           (>= (indices-column pos) 0))
      (string-append (integer->column-letter (indices-column pos))
                     (~a (add1 (indices-row pos))))
      "#N/A"))
   

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
;; Formulas

;;build-openformula : application? indices? [hash-of label? indices?] -> string?
(define (build-openformula xpr pos #:cell-hash cell-hash)

  ;;grid-application->openformula : application? -> string?
  (define (grid-application->openformula app)

    (define fn (application-fn app))
    (if (not (builtin? fn))

        (error (string-append (~a fn) " function not in builtins"))
      
        (let ([args (application-args app)])
          (cond [(= (length args) 1)
                 (let ([arg (car args)])
                   (if (ormap (curry eq? fn) '(+ -))
                       (string-append (~a fn)
                                      (grid-expression->openformula arg))
                       (error (string-append (~a fn) " not supported as a unitary operator"))))]
              
                [(= (length args) 2)
                 (if (ormap (curry eq? fn) '(+ - * /))
                     (string-append (grid-expression->openformula (car args))
                                    (~a fn)
                                    (grid-expression->openformula (cadr args)))
                     (error (string-append (~a fn) " not yet supported")))]
              
                [(> (length args) 2)
                 (error "more than 2 arguments not currently supported")]))))

  ;;parse-formula : expression? -> [listof string?]
  (define (grid-expression->openformula xpr)
    (cond
      [(string? xpr) (error "string types not currently supported in formulae")]

      [(number? xpr) (~a xpr)]

      [(boolean? xpr) (error "boolean types not currently supported in formulae")]

      [(error? xpr) (error "error types not currently supported in formulae")]

      [(reference? xpr) (grid-reference->openformula xpr pos #:cell-hash cell-hash)]

      [(application? xpr) (string-append "(" (grid-application->openformula xpr) ")")]
    
      [else (error "unrecognised type")]))

  (grid-application->openformula xpr))

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
    [(ormap (curry eq? type) '("flat" "fods" "f"))
     (sxml->flat-ods sxml-program filename)]

    [(ormap (curry eq? type) '("extended" "ods" "e"))
     (sxml->extended-ods sxml-program filename)]
      
    [else (error "unrecognised type")]))

;;sxml->flat-ods : sxml-program? string? -> xml
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
  

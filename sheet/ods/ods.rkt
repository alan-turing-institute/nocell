#lang racket/base

(require sxml
         "./namespaces.rkt"
         "../sheet.rkt"
         "../builtins.rkt"
         "./column-letters.rkt"
         racket/format
         racket/function
         racket/file
         racket/string
         racket/list
         racket/system
         racket/contract
         racket/match)

#| 
A backend for Sheet which produces Open Document Format spreadsheets. 
See the github wiki page for accompanying notes: 
https://github.com/alan-turing-institute/nocell/wiki/Creating-ODF-files-from-scratch 

Note: the commands sxml->extended-ods, called externally from sxml->ods, require 
that an executable `zip` program is in the user's path. 

1. Interface
2. Conversion of Sheet spreadsheet to sxml-spreadsheet structure with content and styles.
3. Layout
4. References.
5. Formulas.
6. Adding necessary headers to sxml-spreadsheet for flat ods or extended ods. 
7. Serialisation - convert sxml-spreadsheet to flat or extended ods (includes zipping).
8. Tests.

|#

;; ---------------------------------------------------------------------------------------------------
;; Interface

(provide
 (contract-out
  [sxml->ods (->* (sxml-spreadsheet?)
                  (#:type (or/c 'ods 'fods))
                  bytes?)]
  [sheet-spreadsheet->sxml (->* (spreadsheet?)
                           (#:blank-rows-before (listof integer?)
                            #:blank-cols-before (listof integer?))
                           sxml-spreadsheet?)]
  [bytes->file (-> bytes? string? exact-nonnegative-integer?)]))          



;; ---------------------------------------------------------------------------------------------------
;; Conversion of Sheet spreadsheet to sxml-spreadsheet structure with content and styles.

(struct sxml-spreadsheet (content styles) #:transparent)
(struct indices (row column) #:transparent)

;; sheet-spreadsheet->sxml : spreadsheet? [listof? integer?] -> sxml-spreadsheet?
(define (sheet-spreadsheet->sxml spreadsheet
                            #:blank-rows-before [blank-rows-before '()]
                            #:blank-cols-before [blank-cols-before '()])
  (sxml-spreadsheet  (list `(office:automatic-styles
                         (style:style (@ (style:family "table-row") (style:name "row-default"))
                                      (style:table-row-properties (@ (style:row-height "16pt")
                                                                     (style:use-optimal-row-height "true"))))
                         ,@(column-styles (car (spreadsheet-sheets spreadsheet))))
                       `(office:body
                         ,@(map (curryr sheet-sheet->sxml
                                        #:blank-rows-before blank-rows-before
                                        #:blank-cols-before blank-cols-before)
                                (spreadsheet-sheets spreadsheet))))
                       
                 '(office:styles
                   (style:style (@ (style:family "table-cell") (style:name "default"))
                                (style:table-cell-properties
                                 (@ (fo:padding-bottom "0.200cm")
                                    (fo:padding-left "0.100cm")
                                    (fo:padding-right "0.100cm")
                                    (fo:padding-top "0.200cm"))))
                   (style:style (@ (style:family "table-cell") (style:name "plain") (style:parent-style-name "default")))
                   (style:style (@ (style:family "table-cell") (style:name "column-label") (style:parent-style-name "default"))
                                (style:table-cell-properties (@ (fo:background-color "#DCDCDC")))
                                (style:text-properties (@ (fo:font-weight "bold"))))
                   (number:number-style (@ (style:name "positive") (style:volatile "true"))
                                        (number:number (@ (number:decimal-places "2") (number:min-integer-digits "1"))))
                   (number:number-style (@ (style:name "negative") (style:volatile "true"))
                                        (style:text-properties (@ (fo:color "#ff0000")))
                                        (number:text "-")
                                        (number:number (@ (number:decimal-places "2") (number:min-integer-digits "1"))))
                   (number:number-style (@ (style:name "n_output"))
                                        (number:text "-    ")
                                        (style:map (@ (style:condition "value()>0") (style:apply-style-name "positive")))
                                        (style:map (@ (style:condition "value()<0") (style:apply-style-name "negative"))))
                                           
                   (style:style (@ (style:family "table-cell") (style:name "output") (style:data-style-name "n_output"))))
                 ))
                               

;; sheet-sheet->sxml : sheet? [listof? integer?] -> pair?
(define (sheet-sheet->sxml sheet
                          #:blank-rows-before [blank-rows-before '()]
                          #:blank-cols-before [blank-cols-before '()])

  (define cell-hash (locate-labelled-cells sheet))

  `(office:spreadsheet
    (table:table
     ,@(insert-columns sheet #:blank-cols-before blank-cols-before)
     ,@(for/fold ([row-list '()])
                 ([(row i) (in-indexed (sheet-rows sheet))])
         (let ([new-rows (append
                          (insert-rows-before i #:blank-rows-before blank-rows-before)
                          (list
                           (sheet-row->sxml row
                                           i
                                           #:cell-hash cell-hash
                                           #:blank-rows-before blank-rows-before
                                           #:blank-cols-before blank-cols-before)))])
           (append row-list new-rows))))))
   

;; sheet-row->sxml : [listof cell?] integer? [hash-of label? indices?] [listof? integer?]  -> pair?
(define (sheet-row->sxml row
                        i
                        #:cell-hash [cell-hash (hash)]
                        #:blank-rows-before [blank-rows-before '()]
                        #:blank-cols-before [blank-cols-before '()])
  `(table:table-row (@ (table:style-name "row-default"))
                    ,@(for/fold ([cell-list '()])
                                ([(cell j) (in-indexed row)])
                        (let ([new-cells (append
                                          (insert-cells-before j #:blank-cols-before blank-cols-before)
                                          (list
                                           (sheet-cell->sxml cell
                                                            (indices i j)
                                                            #:cell-hash cell-hash
                                                            #:blank-rows-before blank-rows-before
                                                            #:blank-cols-before blank-cols-before)))])
                          (append cell-list new-cells)))))

;; sheet-cell->sxml : cell? indices? [hash-of label? indices?] [listof? integer?]  -> pair?
(define (sheet-cell->sxml cell
                         pos
                         #:cell-hash [cell-hash (hash)]
                         #:blank-rows-before [blank-rows-before '()]
                         #:blank-cols-before [blank-cols-before '()])
  (cond [(nothing? (cell-xpr cell))
         empty-cell]
        [else 
         `(table:table-cell (@ (table:style-name ,(style-cell (cell-attrs cell))))
                            ,(sheet-expression->sxml-attributes
                              (cell-xpr cell)
                              pos
                              #:cell-hash cell-hash
                              #:blank-rows-before blank-rows-before
                              #:blank-cols-before blank-cols-before))]))
  

;; sheet-expression->sxml-attributes : expression? indices? [hash-of label? indices?]
;; [listof? integer?]  -> string?
(define (sheet-expression->sxml-attributes xpr
                                          pos
                                          #:cell-hash [cell-hash (hash)]
                                          #:blank-rows-before [blank-rows-before '()]
                                          #:blank-cols-before [blank-cols-before '()])
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
     `(@ (table:formula ,(sheet-reference->openformula xpr
                                                      pos
                                                      #:cell-hash cell-hash
                                                      #:blank-rows-before blank-rows-before
                                                      #:blank-cols-before blank-cols-before)))]

    [(application? xpr)
     `(@ (table:formula ,(build-openformula xpr
                                            pos
                                            #:cell-hash cell-hash
                                            #:blank-rows-before blank-rows-before
                                            #:blank-cols-before blank-cols-before)))]

    [(date? xpr)
     `(@ (table:formula ,(format-date xpr)))]
    
    [else (raise-user-error "unrecognised type")]))

(define errors (make-hash (list (cons 'error:arg "#VALUE!")
                                (cons 'error:undef "#N/A")
                                (cons 'error:val "#N/A"))))

(define empty-row  '(table:table-row))
(define empty-cell '(table:table-cell))
(define empty-col '(table:table-column))


;;insert-rows-before : integer? [listof integer?] -> [listof string?]
(define (insert-rows-before i #:blank-rows-before [blank-rows-before '()])
  (cond [(empty? blank-rows-before) '()]
        [(> i (sub1 (length blank-rows-before))) '()] 
        [else (make-list (list-ref blank-rows-before i) empty-row)]))

;;insert-cells-before : integer? [listof integer?] -> [listof string?]
(define (insert-cells-before i #:blank-cols-before [blank-cols-before '()])
  (cond [(empty? blank-cols-before) '()]
        [(> i (sub1 (length blank-cols-before))) '()]
        [else (make-list (list-ref blank-cols-before i) empty-cell)]))

;;insert-cols-before : integer? [listof integer?] -> [listof string?]
(define (insert-cols-before i #:blank-cols-before [blank-cols-before '()])
  (cond [(empty? blank-cols-before) '()]
        [(> i (sub1 (length blank-cols-before))) '()]
        [else (make-list (list-ref blank-cols-before i) empty-col)]))




;; ---------------------------------------------------------------------------------------------------
;; Layout

;;insert-columns : sheet? [listof integer?] -> [listof pair?]
(define (insert-columns sheet
                        #:blank-cols-before [blank-cols-before '()])
  (define widths (column-widths sheet))
  (for/fold ([col-list '()])
            ([(width i) (in-indexed widths)])
    (let ([new-cols (append
                     (insert-cols-before i #:blank-cols-before blank-cols-before)
                     (list
                      `(table:table-column (@ (table:style-name
                                               ,(string-append "col" (~a width)))))))])
      (append col-list new-cols))))


;;column-styles
(define (column-styles sheet)
  (define widths (remove-duplicates (column-widths sheet)))
  (for/list ([width widths])
    `(style:style (@ (style:family "table-column") (style:name ,(string-append "col" (~a width))))
                  (style:table-column-properties
                   (@ (style:column-width ,(string-append (~a width) "cm")))))))
                     

;; column-widths : sheet? positive? positive? -> [listof positive?] 
;; Estimate column widths based on cell-content
(define (column-widths sheet [default-width 1.5] [min-width 1])
  (define cell-widths (for/list ([(row i) (in-indexed (sheet-rows sheet))])
                        (for/list ([(cell j) (in-indexed row)])
                          (cond [(string? (cell-xpr cell))
                                 (cond [(member 'column-label (cell-attrs cell))
                                        (max min-width (* (string-length (cell-xpr cell)) .22))]
                                       [else
                                        (max min-width (* (string-length (cell-xpr cell)) .18))])]
                                [else default-width]))))
  (define transposed (apply map list cell-widths))
  (map (curry apply max) transposed))
              

;; locate-labelled-cells : sheet? -> [hash-of label? indices?]
;; Determine the sheet locations of labelled cells
(define (locate-labelled-cells sheet)
  (for*/hash ([(row i) (in-indexed (sheet-rows sheet))]
              [(cell j) (in-indexed row)]
              #:when (labelled-cell? cell))
    (values (labelled-cell-lbl cell)
            (indices i j))))


;;get-output-index : indices? [listof integer?] -> integer?
;; map sheet-index to outputted spreadsheet index along the given dimension
(define (get-output-index sheet-index blanks-before)
  (cond [(empty? blanks-before) sheet-index]
        [else (define blanks-sum (apply + (take blanks-before (add1 sheet-index))))
              (+ sheet-index blanks-sum)]))


;; style-cell : list? -> string?
;;
;; Takes the list of cell attributes and returns the name of the style
;; for this cell, which should be one of a few styles hardcoded in the
;; spreadsheet output
(define (style-cell cell-attributes)
  (cond
    [(null? cell-attributes) "plain"]
    [(member 'column-label cell-attributes) "column-label"]
    [(member 'output cell-attributes) "output"]
    [else (error "unknown style")]))

;; ---------------------------------------------------------------------------------------------------
;; References

;;sheet-reference->openformula : expression? indices? [hash-of label? indices?] -> string?
(define (sheet-reference->openformula xpr [pos (indices 0 0)]
                                     #:cell-hash [cell-hash (hash)]
                                     #:blank-rows-before [blank-rows-before '()]
                                     #:blank-cols-before [blank-cols-before '()])

  (cond
    [(cell-reference? xpr) (resolve-location (cell-reference-loc xpr)
                                             pos
                                             #:cell-hash cell-hash
                                             #:blank-rows-before blank-rows-before
                                             #:blank-cols-before blank-cols-before)]
    [(range-reference? xpr)
     (string-append (resolve-location (range-reference-tl xpr)
                                      pos
                                      #:cell-hash cell-hash
                                      #:blank-rows-before blank-rows-before
                                      #:blank-cols-before blank-cols-before)
                    ":"
                    (resolve-location (range-reference-br xpr)
                                      pos
                                      #:cell-hash cell-hash
                                      #:blank-rows-before blank-rows-before
                                      #:blank-cols-before blank-cols-before))]))

;;resolve-location : location? -> string?
(define (resolve-location loc
                          [pos (void)]
                          #:cell-hash [cell-hash (hash)]
                          #:blank-rows-before [blank-rows-before '()]
                          #:blank-cols-before [blank-cols-before '()])
  (cond
    [(absolute-location? loc)
     (cond [(hash-has-key? cell-hash (absolute-location-label loc))
            (let ([output-pos (resolve-indices (hash-ref cell-hash (absolute-location-label loc))
                                               #:blank-rows-before blank-rows-before
                                               #:blank-cols-before blank-cols-before)])
              (get-absolute-formula output-pos))]
           [else "#N/A"])]
      
    [(relative-location? loc)
     (let ([output-pos (resolve-indices (get-referent-indices loc pos #:cell-hash cell-hash)
                                        #:blank-rows-before blank-rows-before
                                        #:blank-cols-before blank-cols-before)])
       (get-relative-formula output-pos))]))

;;resolve-indices : indices? -> indices?
(define (resolve-indices pos
                         #:blank-rows-before [blank-rows-before '()]
                         #:blank-cols-before [blank-cols-before '()])
  (let ([ri (get-output-index (indices-row pos) blank-rows-before)]
        [ci (get-output-index (indices-column pos) blank-cols-before)])
    (indices ri ci)))


 

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


;; ---------------------------------------------------------------------------------------------------
;; Formulas

;;build-openformula : application? indices? [hash-of label? indices?] -> string?
(define (build-openformula xpr
                           [pos (indices 0 0)]
                           #:cell-hash [cell-hash (hash)]
                           #:blank-rows-before [blank-rows-before '()]
                           #:blank-cols-before [blank-cols-before '()])

  ;;sheet-application->openformula : application? -> string?
  (define (sheet-application->openformula app)
    (if (not (builtin? (application-fn app)))
        (raise-user-error (string-append (~a (application-fn app) " function not in builtins"))) 
        (format-app app)))
       
  ;;format-binary : builtin? [listof expression?] -> string?
  (define (format-binary fn args)
    (string-join (map sheet-expression->openformula args) (~a fn)))

  ;;format-binary-str : string? [listof expression?] -> string?
  (define (format-binary-str fstr args)
    (string-append fstr "("
                   (string-join
                    (map sheet-expression->openformula args) ",")
                   ")"))

  ;;format-unary : string? expression? -> string?
  (define (format-unary-str fstr arg)
    (string-append fstr "("
                   (sheet-expression->openformula arg)
                   ")"))
  
  ;;format-app : application? -> string?
  (define (format-app app)
    (let ([fn (application-fn app)]
          [args (application-args app)])
      (match fn 
        ;; a lookup table
        ;;binary basic maths
        ['+ (format-binary fn args)]
        ['- (format-binary fn args)]
        ['* (format-binary fn args)]
        ['/ (format-binary fn args)]
        ['quotient (format-binary-str "QUOTIENT" args)]
        ['remainder (string-append (format-binary-str "MOD" args) "*SIGN(" (~a (car args)) ")")]
        ['modulo (format-binary-str "MOD" args)]
        ;;unary basic maths
        ['neg (string-append "-" (sheet-expression->openformula (car args)))]
        ['abs (format-unary-str "ABS" (car args))]
        ['sgn (format-unary-str "SIGN" (car args))]
        ['inv (format-unary-str "MINVERSE" (car args))]
        ['floor (format-unary-str "FLOOR" (car args))]
        ['ceiling (format-unary-str "CEILING" (car args))]
        ['truncate (format-unary-str "TRUNC" (car args))]
        ;;binary comparison
        ['> (format-binary fn args)]
        ['< (format-binary fn args)]
        ['>= (format-binary fn args)]
        ['<= (format-binary fn args)]
        ['= (format-binary fn args)]
        ['!= (format-unary-str "NOT" (format-binary '= args))]
        ;; unary trig
        ['exp (format-unary-str "EXP" (car args))]
        ['ln (format-unary-str "LN" (car args))]
        ['sqrt (format-unary-str "SQRT" (car args))]
        ['acos (format-unary-str "ACOS" (car args))]
        ['asin (format-unary-str "ASIN" (car args))]
        ['atan (format-unary-str "ATAN" (car args))]
        ['cos (format-unary-str "COS" (car args))]
        ['sin (format-unary-str "SIN" (car args))]
        ['tan (format-unary-str "TAN" (car args))]
        ;;binary trig
        ['expt (format-binary '^ args)]
        ['log (format-binary-str "LOG" (reverse args))]
        ;;unary combin
        ['factorial (format-unary-str "FACT" (car args))]
        ;;logical
        ['not (format-unary-str "NOT" (car args))]
        ['and (format-binary-str "AND" args)]
        ['or (format-binary-str "OR" args)]
        ['if (format-binary-str "IF" args)]
        ;; date functions
        ['date (format-date (make-date args))]
        ['date-day (~a (date-day (car args)))]
        ['date-month (~a (date-month (car args)))]
        ['date-year (~a (date-year (car args)))]
        ['date-days (format-binary '- (map format-date args))]
        ['date-add-days (format-binary '+ (list (format-date (car args))
                                                (cadr args)))]
                                                             
        
               
        [else (raise-user-error string-append (~a fn) " not yet supported")])))
    
 
  ;;sheet-expression->openformula : expression? -> string?
  (define (sheet-expression->openformula xpr)
    (cond
      [(string? xpr) xpr]
      [(number? xpr) (~a xpr)]
      [(boolean? xpr) (if xpr "TRUE()" "FALSE()")]
      [(error? xpr) (raise-user-error "error types not currently supported in formulae")]
      [(reference? xpr) (sheet-reference->openformula xpr
                                                     pos
                                                     #:cell-hash cell-hash
                                                     #:blank-rows-before blank-rows-before
                                                     #:blank-cols-before blank-cols-before)]
      [(application? xpr) (string-append "(" (sheet-application->openformula xpr) ")")]
      [(date? xpr) (format-date date)]
      [else (raise-user-error "unrecognised type")]))

  (sheet-application->openformula xpr))


;;make-date : [listof integer?] -> date?
(define (make-date lst)
  (date (first lst) (second lst) (third lst)))

;;format-date : date? -> string?
(define (format-date date)
  (string-append "DATE(" 
                 (string-join (map ~a (list (date-year date)
                                            (date-month date)
                                            (date-day date))) ",")
                 ")"))

;; ---------------------------------------------------------------------------------------------------
;; Adding necessary headers to sxml-spreadsheet for flat ods or extended ods. 
;; Used in sxml->flat-ods and sxml->extended-ods

(define PI '(*PI* xml "version=\"1.0\" encoding=\"UTF-8\""))
(define MIME "application/vnd.oasis.opendocument.spreadsheet")
(define TYPE `(@ (office:version "1.3")
                 (office:mimetype ,MIME)))

(define (flat-sxml sxml-spreadsheet)
  `(*TOP* ,NS ,PI
          (office:document ,TYPE
                           ,(sxml-spreadsheet-styles sxml-spreadsheet)
                           ,@(sxml-spreadsheet-content sxml-spreadsheet))))

(define (extended-sxml sxml-spreadsheet)
  (list MIME 
        (xml-content (sxml-spreadsheet-content sxml-spreadsheet))
        (xml-styles (sxml-spreadsheet-styles sxml-spreadsheet))
        xml-manifest))

(define (xml-content sxml-content)
  `(*TOP* ,NS ,PI
          (office:document-content ,TYPE
                                   ,@sxml-content)))
  
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
;; Serialisation - convert sxml-spreadsheet to flat or extended ods byte string. Uses filesystem commands.
;; WARNING: uses (system zip) because Racket file/zip 
;; doesn't support leaving the first file in the archive uncompressed.

;;sxml->ods-bytes : sxml-spreadsheet? -> bytes? 
(define (sxml->ods sxml-spreadsheet
                   #:type [type 'fods])
  (cond
    [(eq? type 'fods) (sxml->flat-ods-bytes sxml-spreadsheet)]
    [(eq? type 'ods) (sxml->extended-ods-bytes sxml-spreadsheet)]
    [else (error "unrecognised type")]))

;;sxml->flat-ods-bytes : sxml-spreadsheet? -> bytes?
(define (sxml->flat-ods-bytes sxml-spreadsheet)
  (let ([op1 (open-output-bytes)])
    (srl:sxml->xml (flat-sxml sxml-spreadsheet) op1)
    (get-output-bytes op1)))

;;sxml->extended-ods-bytes : sxml-spreadsheet? -> bytes?
(define (sxml->extended-ods-bytes sxml-spreadsheet)

  (define (add-to-tmp fn) (path->string (build-path tmp fn)))
  
  ;;save temporary files
  (define tmp (make-temporary-file "nocelltmp~a" 'directory))
  (define meta (add-to-tmp "META-INF"))
  (make-directory meta)

  (define filelist (list "mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml"))

  (for ([fn filelist]
        [xml (extended-sxml sxml-spreadsheet)])
    (call-with-output-file (add-to-tmp fn)
      (lambda (out) (srl:sxml->xml xml out))))
  
  ;;zip.
  (define curdir (current-directory))
  (current-directory tmp)

  (define ods "tmp.ods")
  
  (system (string-join (list "zip -0 -X" ods (first filelist)) " "))
  (for ([fn (rest filelist)])
    (system (string-join (list "zip -r" ods fn) " ")))

  ;; re-read as bytes
  (define bytefile (file->bytes ods #:mode 'binary))

  ;; Clean up
  (map (lambda(x) (delete-file (add-to-tmp x))) (append filelist (list ods)))
  (map delete-directory (list meta tmp))
  (current-directory curdir)

  bytefile)

;;bytes->file : bytes? string? -> exact-nonnegative-integer?
; writes bytes to file. File extension supplied by user.
(define (bytes->file bstr fn)
  (call-with-output-file fn #:exists 'replace
    (lambda (out) (write-bytes bstr out))))


;; ---------------------------------------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit)

  ;; number?
  (check-equal?
   (sheet-cell->sxml (cell 1 '()) 0)
   '(table:table-cell
     (@ (table:style-name "plain"))
     (@ (office:value-type "float") (office:value "1"))))
  
  (check-equal?
   (sheet-cell->sxml (cell 42.0 '()) 0)
   '(table:table-cell
     (@ (table:style-name "plain"))
     (@ (office:value-type "float") (office:value "42.0"))))
 

  ;; string?
  (check-equal?
   (sheet-cell->sxml (cell "" '()) 0)
   '(table:table-cell
     (@ (table:style-name "plain"))
     (@ (office:value-type "string") (office:string-value ""))))

  (check-equal?
   (sheet-cell->sxml (cell "hello" '()) 0)
   '(table:table-cell
     (@ (table:style-name "plain"))
     (@ (office:value-type "string") (office:string-value "hello"))))

  
  ;; boolean?
  (check-equal?
   (sheet-cell->sxml (cell #t '()) 0)
   '(table:table-cell
     (@ (table:style-name "plain"))
     (@ (office:value-type "boolean") (office:boolean-value "true"))))


  (check-equal?
   (sheet-cell->sxml (cell #f '()) 0)
   '(table:table-cell
     (@ (table:style-name "plain"))
     (@ (office:value-type "boolean") (office:boolean-value "false"))))

   
  ;; nothing?
  (check-equal?
   (sheet-cell->sxml (cell 'nothing '()) 0)
   '(table:table-cell))

  ;; error?
  (check-equal?
   (sheet-cell->sxml (cell 'error:arg '()) 0)
   '(table:table-cell
     (@ (table:style-name "plain"))
     (@ (table:formula "#VALUE!"))))

  (check-equal?
   (sheet-cell->sxml (cell 'error:arg '()) 0)
   '(table:table-cell 
     (@ (table:style-name "plain"))
     (@ (table:formula "#VALUE!"))))

  (check-equal?
   (sheet-cell->sxml (cell 'error:undef '()) 0)
   '(table:table-cell 
     (@ (table:style-name "plain"))
     (@ (table:formula "#N/A"))))

  (check-equal?
   (sheet-cell->sxml (cell 'error:val '()) 0)
   '(table:table-cell 
     (@ (table:style-name "plain"))
     (@ (table:formula "#N/A"))))

  ;;date?
  (check-equal?
   (format-date (date 2020 11 18))
   "DATE(2020,11,18)")

  ;; application?
  (define binary-tests
    (list
     (cons '+ "3+2")
     (cons '- "3-2")
     (cons '* "3*2")
     (cons '/ "3/2")
     (cons 'quotient "QUOTIENT(3,2)")
     (cons 'remainder "MOD(3,2)*SIGN(3)")
     (cons 'modulo "MOD(3,2)")
     (cons '> "3>2")
     (cons '< "3<2")
     (cons '>= "3>=2")
     (cons '<= "3<=2")
     (cons '= "3=2")
     (cons '!= "NOT(3=2)")
     (cons 'expt "3^2")
     (cons 'log "LOG(2,3)")))

  (test-case
   "Binary Functions"
   (for ([test binary-tests])
     (check-equal?
      (build-openformula (application (car test) '(3 2)))
      (cdr test))))
   
  (define unary-tests
    (list
     (cons 'neg "-4")
     (cons 'abs "ABS(4)")
     (cons 'sgn "SIGN(4)")
     (cons 'inv "MINVERSE(4)")
     (cons 'floor "FLOOR(4)")
     (cons 'ceiling "CEILING(4)")
     (cons 'truncate "TRUNC(4)")
     (cons 'exp "EXP(4)")
     (cons 'ln "LN(4)")
     (cons 'sqrt "SQRT(4)")
     (cons 'acos "ACOS(4)")
     (cons 'asin "ASIN(4)")
     (cons 'atan "ATAN(4)")
     (cons 'cos "COS(4)")
     (cons 'sin "SIN(4)")
     (cons 'tan "TAN(4)")
     (cons 'factorial "FACT(4)")))

  (test-case
   "Unary Functions"
   (for ([test unary-tests])
     (check-equal?
      (build-openformula (application (car test) '(4)))
      (cdr test))))

  (define logical-tests
    (list
     (list 'not '(#t) "NOT(TRUE())")
     (list 'and '(#t #f) "AND(TRUE(),FALSE())")
     (list 'or '(#t #f) "OR(TRUE(),FALSE())")
     (list 'if '(#t #t #f) "IF(TRUE(),TRUE(),FALSE())")))

  (test-case
   "Logical Functions"
   (for ([test logical-tests])
     (check-equal?
      (build-openformula (application (first test) (second test)))
      (third test))))

  (define date-tests
    (list
     (list 'date
           '(2020 11 18)
           "DATE(2020,11,18)")
     (list 'date-day
           (list (date 2020 11 18))
           "18")
     (list 'date-month
           (list (date 2020 11 18))
           "11")
     (list 'date-year
           (list (date 2020 11 18))
           "2020")
     (list 'date-days
           (list (date 2020 11 18)
                 (date 2020 11 19))
           "DATE(2020,11,18)-DATE(2020,11,19)")
     (list 'date-add-days
           (list (date 2020 11 18)
                 2)
           "DATE(2020,11,18)+2") 
     ))

  (test-case
   "Data functions"
   (for ([test date-tests])
     (check-equal?
      (build-openformula (application (first test) (second test)))
      (third test))))
                                     
  ;;layout tests
  (check-equal?
   (get-output-index 0 (list 2))
   2)

  (check-equal?
   (get-output-index 2 (list 1 1 0 0))
   4)
  
  ;; see further below for references within applications tests
  ;; define a cell-hash to test references within applications.
  (define cell-refs (locate-labelled-cells
                     (sheet
                      (list
                       (list (labelled-cell "1" '() "cellA1") (labelled-cell "2" '() "cellB1"))
                       (list (labelled-cell "3" '() "cellA2") (labelled-cell "4" '() "cellB2"))))))

  (check-equal? cell-refs
                (hash "cellA1" (indices 0 0)
                      "cellB1" (indices 0 1)
                      "cellA2" (indices 1 0)
                      "cellB2" (indices 1 1)))
    

  ;;references within applications
  ;; in the above cell-hash cell1 is 00 and cell4 is 10, so indices 02 should return 12 (i.e."=C2
  (check-equal?
   (build-openformula
    (application '/
                 (list
                  (cell-reference (absolute-location "cellB1"))
                  (application '*
                               (list
                                (cell-reference (relative-location "cellA2" "cellA1"))
                                30))))
    (indices 2 1) ;B3
    #:cell-hash cell-refs)
   "$B$1/(B2*30)")


  ;;cell-hash test
  ;;use below cell-hash in cell-reference? tests.
  (define cell-hash (locate-labelled-cells
                     (sheet
                      (list
                       (list (labelled-cell "" '() "cell1") (cell "" '()) (labelled-cell "" '() "cell3"))
                       (list (labelled-cell "" '() "cell4") (labelled-cell "" '() "cell5") (cell "" '()))))))

  (check-equal? cell-hash
                (hash "cell1" (indices 0 0)
                      "cell3" (indices 0 2)
                      "cell4" (indices 1 0)
                      "cell5" (indices 1 1))) 


  
  ;;cell-reference? absolute-location? test
  (define absolute-cell (cell (cell-reference (absolute-location "cell5")) '()))

  (check-equal?
   (sheet-reference->openformula (cell-xpr absolute-cell) #:cell-hash cell-hash)
   "$B$2")
  (check-equal?
   (sheet-reference->openformula (cell-reference (absolute-location "nonexistent"))
                                #:cell-hash cell-hash)
   "#N/A")

  (check-equal? (get-absolute-formula (indices 10 5)) "$F$11")

  ;;cell-reference relative-location? test
  ;; in the above cell-hash cell1 is 00 and cell4 is 10, so indices 02 should return 12 (i.e."=C2")
  (define relative-cell (cell (cell-reference (relative-location "cell1" "cell4")) '()))
  (check-equal?
   (sheet-reference->openformula (cell-xpr relative-cell) (indices 0 2) #:cell-hash cell-hash)
   "C2")

  (check-equal? (get-relative-formula (indices 20 10)) "K21")

  (check-equal? (get-relative-formula (indices 0 -1)) "#N/A")
  (check-equal? (get-relative-formula (indices -1 0)) "#N/A")
  (check-equal? (get-relative-formula (indices -1 -1)) "#N/A")
  (check-equal? (get-relative-formula (indices 0 0)) "A1")

  ;;range-reference?
  (check-equal?
   (sheet-reference->openformula
    (range-reference (absolute-location "cell1") (absolute-location "cell3"))
    #:cell-hash cell-hash)
   "$A$1:$C$1")
  
  ;;multiple cells test
  (check-equal?
   (sheet-sheet->sxml (sheet
                      (list (list (cell 1 '())) (list (cell 2 '())))))
   `(office:spreadsheet
     (table:table
      (table:table-column (@ (table:style-name "col1.5")))
      (table:table-row
       (@ (table:style-name "row-default"))
       (table:table-cell
        (@ (table:style-name "plain"))
        (@ (office:value-type "float") (office:value "1"))))
      (table:table-row
       (@ (table:style-name "row-default"))
       (table:table-cell
        (@ (table:style-name "plain"))
        (@ (office:value-type "float") (office:value "2")))))))

  ;;empty rows test
  (check-equal?
   (sheet-sheet->sxml (sheet (list (list (cell 1 '()))))
                     #:blank-rows-before '(2))
   `(office:spreadsheet
     (table:table
      (table:table-column (@ (table:style-name "col1.5")))
      (table:table-row)
      (table:table-row)
      (table:table-row
       (@ (table:style-name "row-default"))
       (table:table-cell
        (@ (table:style-name "plain"))
        (@ (office:value-type "float") (office:value "1")))))))


  ;;empty cols test
  (check-equal?
   (sheet-sheet->sxml (sheet (list (list (cell 1 '()))))
                     #:blank-cols-before '(2))
   `(office:spreadsheet
     (table:table
      (table:table-column)
      (table:table-column)
      (table:table-column (@ (table:style-name "col1.5")))
      (table:table-row
       (@ (table:style-name "row-default"))
       (table:table-cell)
       (table:table-cell)
       (table:table-cell
        (@ (table:style-name "plain"))
        (@ (office:value-type "float") (office:value "1")))))))
                     
  ) 
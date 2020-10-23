#| JG:
Comments starting with "JG:" are review comments.

Summary of comments: 

1. My main comment is to aim for precise and thoughtful comments. This is highly
non-trivial. In addition, names of functions are really hard to get right. 

In general, the code should self-document its specific purpose and its place in
the overall organisation. Comments could elucidate an algorithm; make something
clear that would otherwise be opaque; outline what assumptions are being made at
any point, and so on.

2. Scheme comment style is:
   - ;;; Three semi-colons for major sections (aligned left)
   - ;;  Two semi-colons for comments on their own line (indent to same level as
code)
   - ; Single semi-colon for comments following code, on the same line (use sparingly)

3. It's probably okay to rely on external zip, though it's not great. But
definitely don't do system "rm" -- there must be a racket interface to the
filesystem. In particular, rm won't work on Windows. 

4. Write this module as if it were genuinely a utility library already, not a
"getting things working" scratchpad. If you need to test things out, have that
in a different file which require's this one. For example, stuff like opening
libreoffice should be in a different module.

5. I feel like the filesystem stuff is rather mixed up with the ->sxml stuff. It
really wants to be extremely clear. In fact, I would go so far as to say that,
if the main purpose of this module is to have effects on the filesystem, then
*all* the grid->sxml stuff should go in one separate module, and all the
filesystem stuff should go in another. 
|#


;; JG: Prefer racket/base
#lang racket

#| JG:
Probably one should have a module purpose right at the top. Suggest something
like:

A backend for grid which produces Open Document Format spreadsheets. 
See here for the specification: 

Notable differences between Grid and ODF:
- blah blah

Note: the commands foo, bar require that an executable `zip` program is in the
user's path. (What happens if it isn't?)

Then perhaps a description of the structure of this file, for example:
and then make headers that match these descriptions (or better versions thereof).

1. Interface
2. Conversion of the consituents of a grid program to xml
3. Adding the necessary wrappers and headers to the xml output 
4. Serialisation (including zipping) 
5. Utilities private to this module

It's not really critical that the structure is exactly that, but there should be
some structure.

Some Racket style guide recommends lines no more than 102 characters. That's
this long:
;; ---------------------------------------------------------------------------------------------------

You can also use that line as a section separator.

|#


#| JG: 
I think the names here need some thought. I don't have a good answer,
but:

- I think grid->foo is better than prog->foo. (And program->foo would be better
than prog->foo. Scheme tends to abbreviate less than other
languages. grid-program->foo would be fine, too.)

- The difference between fsxml, esxml, fods, and eods isn't clear. 

- Is a thing that is an `sxmlfile?` actually a file? (btw, in Scheme one tends to
use "snake-case" a lot, so this would be `sxml-file?`) What would a file even
be? Maybe if it's grid->fsxml the output should be of type `fsxml`?


|#
(provide
 (contract-out
  [prog->fsxml (-> program? sxmlfile?)]
  [prog->esxml (-> program? esxmlfile?)]
  [fsxml->fods (->* (sxmlfile?)
                    ;; JG: I think the next line should align with the one
                    ;; above, for example.
                   (#:open boolean? #:path string?)
                   any)]
  [esxml->eods (->* (esxmlfile?)
                   (#:open boolean? #:dir string?)
                   any)]
  (struct esxmlfile ([files (listof sxmlfile?)]))))



;output basic grid sheets to ods.
(require sxml
         "./namespaces.rkt"
         "../grid/grid.rkt"
         "./column-letters.rkt")


;; JG: What kinds of "headers" have information here? Is it headers for this
;; module, or something else?
;----- HEADER INFO -----------

;; JG: Maybe ` should be ' ?
(define PI `(*PI* xml "version=\"1.0\" encoding=\"UTF-8\""))
(define MIME "application/vnd.oasis.opendocument.spreadsheet")
;; JG: Maybe just embed MIME in TYPE unless it is likely to be used elsewhere?
(define TYPE `(@ (office:version "1.3") (office:mimetype ,MIME)))

;; JG: Probably don't need the word "function". How about the following (where
;; "fods" should be explained somewhere else.):

;; ---------------------------------------------------------------------------------------------------
;; Convert an sxml description of a spreadsheet to fods   

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

;; JG: Maybe content/xml, styles/xml etc? Or xml-content, xml-styles, ...
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

  ;; JG: Changed formatting here. Also ";;then clear up" could be ";; Remove
  ;; temporary files."
  ;;then clear up.
  (for ([fn fnlist])
    (system (string-append "rm " fn)))
       
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
boolean?                       X
error?                         x
nothing?                       x
matrix?
reference?
- cell-reference
  - absolute-location          -
  - relative-location
- range-reference
  - absolute-location
  - relative-location
application?


Note that booleans in excel seem to be "1" or "0". You can add a boolean, e.g. 30+TRUE = 31.


Need to convert a lists of lists into a hash table of cell positions.

|#


;; builds a hash table of the labelled cells and their alphanumeric positions
(define (cell-hash sheet)
  ;; rows go 1,2,3,4 ...
  ;; columns go A,B,C,D...
  ;; if not a labelled cell add it as a hash pointing to itself.
  (let ([rows (sheet-rows sheet)]
        [h (make-hash)])
    (for ([i (length rows)] [row rows])
      (for ([j (length row)] [cell row])
        (if (labelled-cell? cell)
            (hash-set! h
                       (labelled-cell-lbl cell)
                       (string-append (integer->column-letter j) (~a (add1 i))))
            (hash-set! h
                       (string-append (integer->column-letter j) (~a (add1 i))) 
                       (string-append (integer->column-letter j) (~a (add1 i)))))))
    h))


(define errors (make-hash (list (cons 'error:arg "of:=#VALUE!")
                                (cons 'error:undef "of:=#N/A")
                                (cons 'error:val "of:=#N/A"))))


;; JG: Here's a good example where you could usefully be much more precise with
;; documentation. Also, `cellattr` isn't a great name. 
(define (cellattr cell);change attributes depending on cell contents
  ;; JG: I think we're extracting the cell contents (`xpr`), which is an
  ;; `expression?`, and converting thatto the attributes of a table:table-cell
  ;; tag? maybe pull out the xpr before calling this function, and maybe it's
  ;; just `expression->xml/attributes`?
  (define xpr (cell-xpr cell))
  (cond
    [(string? xpr)
     `(@ (office:value-type "string") (office:string-value ,(~a xpr)))]
    ;; JG: If you'd like to leave a blank line, you can just do so: you don't
    ;; need a comment marker.
    ;;
    [(number? xpr)
     `(@ (office:value-type "float") (office:value ,(~a xpr)))]
    ;; office:value-type="float" office:value="<xpr>"
    ;; 
    [(boolean? xpr)
      `(@ (office:value-type "boolean") (office:boolean-value ,(if xpr "true" "false")))]
    ;;
    [(error? xpr)
     `(@ (table:formula ,(hash-ref errors xpr)))]
     ;;
    [(reference? xpr)
     ;; JG: Here you might want to call out to a helper function. 
     (cond
       [(cell-reference? xpr)
        (let ([loc (cell-reference-loc xpr)])
          (println "here") ;; JG: Is this for debugging?
          (cond
            [(absolute-location? loc)
             `(@ (table:formula ,(string-append "=" (absolute-location-label loc))))]
            ;;
            [(relative-location? loc) (error "relative loc todo")]))]
          ;;
       [(range-reference? xpr) (error "range reference to do")])]
    ;;
    [else (error "unrecognised type")]))


;; JG: I think I'd probably define the more general function first, for
;; readability. and probably all the grid-foo->sxml functions should go in one
;; section. 
;; ---- build to sxml
(define (cell->sxml cell)
  
  (if (nothing? (cell-xpr cell))
      '(table:table-cell)
      `(table:table-cell , (cellattr cell)))) ;the type should return different values depending on cell-xpr

(define (row->sxml row)
  `(table:table-row
    ,@(map cell->sxml row)))

(define (sheet->sxml sheet)
  `(office:spreadsheet
     (table:table
      ,@(map row->sxml (sheet-rows sheet)))))

;; -------------------------------- TESTS ----------------------

(module+ test
  (require rackunit)

  ;; Atomic values, number? string? boolean? error? nothing?

  ;; number?
  (check-equal? (cell->sxml (cell 1)) '(table:table-cell (@ (office:value-type "float") (office:value "1"))))
  (check-equal? (cell->sxml (cell 42.0)) '(table:table-cell (@ (office:value-type "float") (office:value "42.0"))))

  ;; string?
  (check-equal? (cell->sxml (cell "")) '(table:table-cell (@ (office:value-type "string") (office:string-value ""))))
  (check-equal? (cell->sxml (cell "hello")) '(table:table-cell (@ (office:value-type "string") (office:string-value "hello"))))
  
  ;; boolean?
  (check-equal? (cell->sxml (cell #t)) '(table:table-cell (@ (office:value-type "boolean") (office:boolean-value "true"))))
  (check-equal? (cell->sxml (cell #f)) '(table:table-cell (@ (office:value-type "boolean") (office:boolean-value "false"))))
   
  ;; nothing?
  (check-equal? (cell->sxml (cell 'nothing)) '(table:table-cell))

  ;; error?
  (check-equal? (cell->sxml (cell 'error:arg)) '(table:table-cell (@ (table:formula "of:=#VALUE!")))) 
  (check-equal? (cell->sxml (cell 'error:undef)) '(table:table-cell (@ (table:formula "of:=#N/A"))))
  (check-equal? (cell->sxml (cell 'error:val)) '(table:table-cell (@ (table:formula "of:=#N/A")))) 

  ;'error:arg - argument is wrong type #VALUE!
  ;'error:undef - function cannot be evaluated (1/0) #N/A
  ;'error:val - missing value #N/A
  ; excel errors: #DIV/0! #NAME? #NULL! #REF! #N/A #VALUE!

#|
  ;; application?
  (check-equal? (cell->sxml (cell (+ 1 2))) '(table:table-cell (@ (table:formula "of:=1+2"))))
|#


  ;;cell-hash test
  (check-equal? (cell-hash (sheet
                           (list
                            (list (labelled-cell "" "cell1") (cell "") (labelled-cell "" "cell3"))
                            (list (labelled-cell "" "cell4") (labelled-cell "" "cell5") (cell "")))))
               #hash(("B1" . "B1") ("C2" . "C2") ("cell1" . "A1") ("cell3" . "C1") ("cell4" . "A2") ("cell5" . "B2"))) ;at the moment the keys return alphabetically

  
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

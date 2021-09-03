#lang racket

(require sxml
         "../sheet.rkt"
         "../builtins.rkt"
         racket/format
         racket/function
         racket/file
         racket/string
         racket/list
         racket/system
         racket/contract
         racket/match)


#| 
A backend for Sheet which produces simple tables for scribble.


Unicode reference: https://unicode-search.net/unicode-namesearch.pl


Thoughts:

- might need an evaluator for all outputs. This will be simple enough, just store the cell content in a look up table then recursively resolve the locations in the normal way.
I can just use racket for the evaluator so will not need a complex formula builder
- might also need to somehow peek into a preceding or subseqent row's contents, for bordering. 

|#

;; ---------------------------------------------------------------------------------------------------
;; Interface

(provide
 (contract-out
  [sheet-spreadsheet->ascii-spreadsheet (->* (spreadsheet?)
                                    (#:blank-rows-before (listof integer?)
                                     #:blank-cols-before (listof integer?))
                                    ascii-spreadsheet?)]
  [ascii-spreadsheet->string (-> ascii-spreadsheet? string?)]))  



;; ---------------------------------------------------------------------------------------------------
;; Conversion of Sheet spreadsheet to ascii-spreadsheet
(struct ascii-spreadsheet (content) #:transparent) ; content is list of sheets, which is a list of rows, rowls are list of cells, cells are strings. There should be a separate function that joins these together and prints it.
(struct indices (row column) #:transparent)

;; sheet-spreadsheet->ascii : sheet? [listof? integer?] -> ascii-sheet?
(define (sheet-spreadsheet->ascii-spreadsheet spreadsheet
                                     #:blank-rows-before [blank-rows-before '()]
                                     #:blank-cols-before [blank-cols-before '()])
  (ascii-spreadsheet (map (curryr sheet-sheet->ascii
                              #:blank-rows-before blank-rows-before
                              #:blank-cols-before blank-cols-before)
                      (spreadsheet-sheets spreadsheet))))

;; sheet-sheet->ascii : sheet? [listof? integer?] -> pair?
(define (sheet-sheet->ascii sheet
                           #:blank-rows-before [blank-rows-before '()]
                           #:blank-cols-before [blank-cols-before '()])

  (define cell-hash (locate-labelled-cells sheet)) ;store labelled cells stores the formula, we will recursively evaluate
  (define border-hash (locate-borders sheet))

  (for/fold ([row-list '()])
            ([(row i) (in-indexed (sheet-rows sheet))])
            
    (let ([new-rows (append
                     (insert-rows-before i #:blank-rows-before blank-rows-before)
                     (make-border-row i (length row) #:blank-cols-before blank-cols-before #:border-hash border-hash)
                     (list
                      (sheet-row->ascii row
                                       i
                                       #:cell-hash cell-hash
                                       #:blank-rows-before blank-rows-before
                                       #:blank-cols-before blank-cols-before)))])
      (append row-list new-rows))))


;; sheet-row->ascii : [listof cell?] integer? [hash-of label? indices?] [listof? integer?]  -> pair?
(define (sheet-row->ascii row
                         i
                         #:cell-hash [cell-hash (hash)]
                         #:blank-rows-before [blank-rows-before '()]
                         #:blank-cols-before [blank-cols-before '()])
  (for/fold ([cell-list '()])
            ([(cell j) (in-indexed row)])
    (let ([new-cells (append
                      (insert-cells-before j #:blank-cols-before blank-cols-before)
                      (list
                       (sheet-cell->ascii cell
                                         (indices i j)
                                         #:cell-hash cell-hash
                                         #:blank-rows-before blank-rows-before
                                         #:blank-cols-before blank-cols-before)))])
      (append cell-list new-cells))))

;; sheet-cell->sxml : cell? indices? [hash-of label? indices?] [listof? integer?]  -> pair?
(define (sheet-cell->ascii cell
                          pos
                          #:cell-hash [cell-hash (hash)]
                          #:blank-rows-before [blank-rows-before '()]
                          #:blank-cols-before [blank-cols-before '()])
  (cond [(nothing? (cell-xpr cell))
         empty-cell]
        [else 
         (style-cell (cell-attrs cell)
                     (sheet-expression->ascii (cell-xpr cell)
                                             pos
                                             #:cell-hash cell-hash
                                             #:blank-rows-before blank-rows-before
                                             #:blank-cols-before blank-cols-before))]))
  

;; sheet-expression->sxml-attributes : expression? indices? [hash-of label? indices?]
;; [listof? integer?]  -> string?
(define (sheet-expression->ascii xpr
                                pos
                                #:cell-hash [cell-hash (hash)]
                                #:blank-rows-before [blank-rows-before '()]
                                #:blank-cols-before [blank-cols-before '()])
  (cond
    [(string? xpr)
     (~a xpr)]

    [(number? xpr)
     (~a xpr)]

    [(boolean? xpr)
     (if xpr "true" "false")]

    [(error? xpr)
     (hash-ref errors xpr)]

    [(reference? xpr)
     (~a (sheet-reference->ascii xpr
                              pos
                              #:cell-hash cell-hash
                              #:blank-rows-before blank-rows-before
                              #:blank-cols-before blank-cols-before))]

    [(application? xpr)
     (~a (sheet-application->ascii xpr
                               pos
                               #:cell-hash cell-hash
                               #:blank-rows-before blank-rows-before
                               #:blank-cols-before blank-cols-before))]

    [(date? xpr)
     (format-date  xpr)]
    
    [else (raise-user-error "unrecognised type")]))

(define errors (make-hash (list (cons 'error:arg "#VALUE!")
                                (cons 'error:undef "#N/A")
                                (cons 'error:val "#N/A"))))

(define empty-row (list ""))
(define empty-cell "")


;;insert-rows-before : integer? [listof integer?] -> [listof string?]
(define (insert-rows-before i #:blank-rows-before [blank-rows-before '()])
  (cond [(empty? blank-rows-before) '()]
        [(> i (sub1 (length blank-rows-before))) '()] 
        [else (make-list (list-ref blank-rows-before i) empty-row)]))

;;insert-cells-before : integer? [listof integer?] -> [listof string?]
(define (insert-cells-before j #:blank-cols-before [blank-cols-before '()])
  (cond [(empty? blank-cols-before) '()]
        [(> j (sub1 (length blank-cols-before))) '()]
        [else (make-list (list-ref blank-cols-before j) empty-cell)]))

;;make-border-row : integer? [listof integer?] -> [listof string?]
(define (make-border-row row_idx row_length #:blank-cols-before [blank-cols-before '()] #:border-hash border-hash)
  (cond [(has-border? row_idx row_length border-hash)
         (list (for/fold ([cell-list '()])
                   ([j (build-list row_length values)])
           (let ([new-cells (append
                             (insert-cells-before j #:blank-cols-before blank-cols-before)
                             (list (hash-ref border-hash (indices row_idx j))))])
             (append cell-list new-cells))))]
        [else '()]))
           
                            
  
  

;; ---------------------------------------------------------------------------------------------------
;; Layout



;; locate-labelled-cells : sheet? -> [hash-of label? [pair-of indices? expression?]]
;; Determine the grid locations of labelled cells
;; expressions included for evaluation.
(define (locate-labelled-cells sheet)
  (for*/hash ([(row i) (in-indexed (sheet-rows sheet))]
              [(cell j) (in-indexed row)]
              #:when (labelled-cell? cell))
    (values (labelled-cell-lbl cell)
            (cons (indices i j) (cell-xpr cell)))))
    
;;get-output-index : indices? [listof integer?] -> integer?
;; map grid-index to outputted spreadsheet index along the given dimension
(define (get-output-index sheet-index blanks-before)
  (cond [(empty? blanks-before) sheet-index]
        [else (define blanks-sum (apply + (take blanks-before (add1 sheet-index))))
              (+ sheet-index blanks-sum)]))


;; locate-borders: sheet? -> [hash-of indices? any]
;; determine border type before cell
(define (locate-borders sheet)
  (for*/hash ([(row i) (in-indexed (sheet-rows sheet))]
              [(cell j) (in-indexed row)])
    (values (indices i j)
            (style-border (lookup-style (cell-attrs cell))))))

;; has-border? : integer? integer? [hash-of indices? any] -> boolean?
(define (has-border? row-idx row-length border-hash)
  (not (andmap (curry eq? "")
               (for/list ([j (build-list row-length values)])
                 (hash-ref border-hash (indices row-idx j))))))
                      

;; ---------------------------------------------------------------------------------------------------
;; STYLES

(struct style (border) #:transparent)
(define styles (make-hash (list (cons "plain" (style ""))
                                (cons "l1_output" (style "-"))
                                (cons "l2_output" (style "=")))))



;; lookup-style : list? -> style?
(define (lookup-style cell-attributes)
  (cond
    [(null? cell-attributes) (hash-ref styles "plain")]
    [(member 'input cell-attributes) (hash-ref styles "plain")]
    [(member 'l1_output cell-attributes) (hash-ref styles "l1_output")]
    [(member 'l2_output cell-attributes) (hash-ref styles "l2_output")]
    [else (error "unknown style")]))


;; Takes the list of cell attributes and returns the name of the style
;; for this cell, which should be one of a few styles hardcoded in the
;; spreadsheet output
(define (style-cell cell-attributes
                    cell-content)
  cell-content)
;(cond

;; ---------------------------------------------------------------------------------------------------
;; REFERENCES

(define (sheet-reference->ascii xpr [pos (indices 0 0)]
                            #:cell-hash [cell-hash (hash)]
                            #:blank-rows-before [blank-rows-before '()]
                            #:blank-cols-before [blank-cols-before '()])
  "REF")





;; ---------------------------------------------------------------------------------------------------
;; FORMULA
;;build-openformula : application? indices? [hash-of label? indices?] -> string?
(define (sheet-application->ascii xpr
                                 [pos (indices 0 0)]
                                 #:cell-hash [cell-hash (hash)]
                                 #:blank-rows-before [blank-rows-before '()]
                                 #:blank-cols-before [blank-cols-before '()])
  


  ;;evaluate-application : application? -> number?
  (define (evaluate-application app)
    (if (not (builtin? (application-fn app)))
        (raise-user-error (string-append (~a (application-fn app) " function not in builtins"))) 
        (format-app app)))

       
  ;;format-binary : builtin? [listof expression?] -> string?
  (define (format-binary fn args)
    (let ([args (map evaluate-expression args)])
      (fn (car args) (cdr args))))
  
  
  ;;format-app : application? -> string?
  (define (format-app app)
    (let ([fn (application-fn app)]
          [args (application-args app)])
      (match fn 
        ;; a lookup table
        ;;binary basic maths
        ['+ (format-binary + args)]
        ['- (format-binary - args)]
        ['* (format-binary * args)]
        ['/ (format-binary - args)]        
        [else (raise-user-error string-append (~a fn) " not yet supported")])))
    
 
  ;;evaluate-expression : expression? -> expression?
  (define (evaluate-expression xpr)
    (cond
      [(string? xpr) xpr]
      [(number? xpr) xpr]
      [(boolean? xpr) xpr]
      [(error? xpr) xpr]
      [(reference? xpr) (sheet-reference->ascii xpr
                                            pos
                                            #:cell-hash cell-hash
                                            #:blank-rows-before blank-rows-before
                                            #:blank-cols-before blank-cols-before)]
      [(application? xpr) (evaluate-application xpr)]
      [(date? xpr) (format-date date)]
      [else (raise-user-error "unrecognised type")]))

  evaluate-application xpr)


;;format-date : date? -> string?
(define (format-date date) 
  (string-join (map ~a (list (date-year date)
                             (date-month date)
                             (date-day date))) "-"))

;; ---------------------------------------------------------------------------------------------------
;; DISPLAY LAYOUT


(define (fill-column col-width contents)
  (cond [(>= (string-length contents) col-width) contents]
        [else (fill-column col-width (string-append contents " "))]))
 
(define (pad-row row-padding)
  (make-string row-padding #\newline))


(define (max-row-length ascii)
  (apply max (map length (car ascii))))

(define (header-string ascii col-width)
  (string-append (make-string (* (max-row-length ascii) col-width)
                              #\U002D) "\n"))

(define (ascii-spreadsheet->string ascii
                               #:col-width [col-width 5]
                               #:col-padding [col-padding 1]
                               #:row-padding [row-padding 0])
  (string-append
   (header-string (ascii-spreadsheet-content ascii) (+ col-width col-padding))
  (for*/fold ([output ""])
    ([sheet (ascii-spreadsheet-content ascii)]
     [row sheet])
    (string-append
     output
     (pad-row row-padding)
     (foldl (curry string-append (make-string col-padding #\space))
            ""
            (map (curry fill-column col-width) row))
     "\n"))
  (header-string (ascii-spreadsheet-content ascii) (+ col-width col-padding))))
    


;; ---------------------------------------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit)

  (let ([cell-tests
         (list
          ;; number?
          (cons 1 "1")
          (cons 42.0 "42.0")
          ;; string?
          (cons "" "")
          (cons "hello" "hello")
          ;;boolean?
          (cons #t "true")
          (cons #f "false")
          ;; nothing?
          (cons 'nothing "")
          ;; error? 
          (cons 'error:arg "#VALUE!")
          (cons 'error:undef "#N/A")
          (cons 'error:val "#N/A")
          ;; date?
          (cons (date 2020 11 18) "2020-11-18")
          )])
   
    (test-case
     "Cell Types"
     (for ([test cell-tests])
       (check-equal?
        (sheet-cell->ascii (cell (car test) '()) 0)
        (cdr test)))))


  ;; BORDER TESTS
  (let ([border-hash (make-hash (list
                                 (cons (indices 0 0) "")
                                 (cons (indices 0 1) "-")))]
        [cols-before '(0 1)])
    (check-equal?
     (make-border-row 0 2 #:blank-cols-before cols-before #:border-hash border-hash)
     (list (list "" "" "-"))))

  (let ([border-hash (make-hash (list
                                 (cons (indices 0 0) "")
                                 (cons (indices 0 1) "")))]
        [cols-before '(0 0)])
    (check-equal?
     (make-border-row 0 2 #:blank-cols-before cols-before #:border-hash border-hash)
     '()))

  (let ([border-hash (make-hash (list
                                 (cons (indices 0 0) "")
                                 (cons (indices 0 1) "")))])
    (check-equal?
     (has-border? 0 2 border-hash)
     #f))

  (let ([border-hash (make-hash (list
                                 (cons (indices 0 0) "-")
                                 (cons (indices 0 1) "")))])
    (check-equal?
     (has-border? 0 2 border-hash)
     #t))



    
          
  
  )

                               


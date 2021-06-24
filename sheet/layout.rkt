#lang racket/base

(require racket/format
         racket/function
         racket/string
         racket/list
         racket/match)


#|

Produces a ascii representation of a spreadsheet of an s-expression.

We wish to represent 4 types of brackets: 
1. Open bracket where the contents are atoms
2. Close bracket where the contents are atoms
3. Open bracket followed by an application (open applications)
4. Close the application (close applications)

Consider a tree. A node has children. In the expression '(1 2 3) the children are  1 2 3. 

Rule 1:
Arrays are laid out from top-bottom with a row per atom.

Rule 2: 
Children are seperated by an amount of blank rows corresponding to
the deepest tree depth of that node's children minus 1.

Programmatically, this means adding a number of "\n"s equal to the maximum tree depth of that node.

This rule takes care of 1 & 2.

Rule 3:
Open applications are given by a thin line.

Rule 4:
Close applications are given by a solid line.
The result of the application is laid out as an array immediately after the solid line

1. Interface
2. Tree traversal
3. Applications
4. Formatting (Serialise layout)
6. Post-processing for display
7. Recovering tree structure from formatting
8. Examples

|#


;; --------------------------------------------------------------------------------------------------
;;  INTERFACE

(provide sheet->display
         tree->sheet
         sheet->tree)

;; --------------------------------------------------------------------------------------------------
;; Tree Traversal

;(struct sheet (content) #:transparent)


;;tree->sheet: pair? -> [listof (or string? symbol?)] 
(define (tree->sheet x)
  (flatten (format-tree x)))

;;sheet->display: sheet? -> void?
(define (sheet->display x)
  (display-sheet (tidy-layout x)))

;;format-tree: pair? -> [listof [listof string?]]
;;each node becomes a list of strings
(define (format-tree x [depth 0])
  (cond
    [(leaf? x) (format-leaf x)]
    [(symbol? x) (format-symbol x)]
    [(result? x) (format-result x)]
    [(has-children? x)
     (cond
       [(application? x)
        (format-app x)]
       [else
        (format-array-traversal
         x
         (max-tree-depth x))])]))  


(define (leaf? x)
  (integer? x))

(define (has-children? x)
  (pair? x))

;; --------------------------------------------------------------------------------------------------
;; EVALUATING APPLICATIONS

(struct app (fn args result) #:transparent)
(struct result (result) #:transparent)

;; format-app: pair? -> [listof (or string? symbol?)]
(define (format-app x)
  (let ([app (structure-application x)])
    (append
     open-app
     (format-tree (app-args app))
     (format-tree (app-result app)))))

;; structure-application: pair? -> app?
(define (structure-application x)
  (app (car x)
       (cdr x)
       (result
        (evaluate-application x))))

;; evaluate-application: pair? -> [listof number?]
(define (evaluate-application x)
  (let ([fn (car x)]
        [args (cdr x)])
    ((symbol->application fn)
     (parse-args args))))

;; parse-args: pair? -> listof listof values?
(define (parse-args args)
  ;if any of the items are an application then recursively evaluate until they are not
  (let ([args (recursive-evaluate args)])
    (cond
      [(= (length args) 2)
       (cond 
         [(andmap list? args)
          ; zip if two lists that are not applications
          (zip (map get-atom (car args)) (map get-atom (cadr args)))]
         ; if a list and integer, extend the integer.
         [(and (atom? (car args))
               (list? (cadr args)))
          (zip (extend-cell (list (get-atom (car args))
                                  (cadr args)))
               (cadr args))]
         [(and (atom? (cadr args))
               (list? (car args)))
          (zip (extend-cell (list (get-atom (cadr args))
                                  (car args)))
               (car args))]
         ; if two integers, package them in lists
         [(andmap atom? args)            
          (list (map get-atom args))])]

      [(= (length args) 1)
       (car args)])))


(define (recursive-evaluate l)
  (cond
    [(application? l) (evaluate-application l)]
    [(has-children? l)
     (map recursive-evaluate l)]
    [else l]))

                 
(define (symbol->application sym)
  (match sym
    ['+ (curry map (curry apply +))]
    ['- (curry map (curry apply -))]
    ['* (curry map (curry apply *))]
    ['/ (curry map (curry apply /))]
    ['add1 (curry map (curry add1))]
    ['sum (curry apply +)]))


(define (extend-cell args)
  (make-list (length (cadr args))
             (car args)))

(define (application? l)
  (cond
    [(pair? l) (symbol? (car l))]
    [else #f]))

(define (not-application? l)
  (not (application? l)))

(define (contains-application? l)
  (cond
    [(application? l) #t]
    [(has-children? l)
     (ormap contains-application? l)]
    [else #f]))

(define (zip l1 l2)
  (map list l1 l2))

;; if it is an integer or a listof a single integer, treat the same
(define (atom? x)
  (cond
    [(integer? x) #t]
    [(and (= (length x) 1) (integer? (car x)))
     #t]
    [else #f]))

(define (get-atom x)
  (cond
    [(integer? x) x]
    [else (car x)]))

;; --------------------------------------------------------------------------------------------------
;; FORMATTING


#|
"+": #\U002B; "-": #\U002D; "_": #\U005F; ".": #\U002E,
borders: https://en.wikipedia.org/wiki/Box-drawing_character
|#

(define (format-leaf x)
  (list (~a x) "\n"))


(define (format-symbol x)
  (list (~a x) "\n"))


(define (format-result x)
  (list close-app
        (map format-tree (result-result x))))


(define (format-array-traversal x depth)
  (list
   (space-arguments
    (map format-tree x) depth)))


(define (space-arguments l depth)
  (append*
   (list (car l))
   (map (λ (x) (list (to-sibling depth) x)) (cdr l))))
                                      
  
;; tree-depth: pair? [integer?] -> integer?
;; any array with children, count the maximum depth (i.e. how many internal brackets) 
(define (max-tree-depth x [depth 0])
  (cond
    [(has-children? x)
     (apply max
            (map (curryr max-tree-depth (add1 depth)) x))]
    [else depth]))

;; used for borders only to replicate to column width
;; for newlines use "\n" elsewhere
(define ucode-hash
  (make-hash
   (list (cons 'open-app #\U2500)
         (cons 'close-app #\U2501))))

(define open-app (list 'open-app "\n"))
(define (to-sibling depth) (make-list (sub1 depth) "\n"))
(define close-app (list 'close-app "\n"))

;; --------------------------------------------------------------------------------------------------
;; TIDY OUTPUT POST-PROCESSING

;; tidy-layout: [listof string?] [integer? integer? integer?] -> [listof string?]
(define (tidy-layout sheet
                     #:col-width [col-width 3]
                     #:col-padding [col-padding 1]
                     #:row-padding [row-padding 0])
  (add-headers
   col-width
   ;(trim-head
   (trim-tail
    (align-columns col-width sheet))))

;; display-sheet: [listof string?] -> void?
(define (display-sheet contents)
  (displayln
   (string-join
    contents
    "")))

;; add-headers: integer? [listof string?] -> [listof string?]
(define (add-headers col-width contents)
  (append
   (list (header-string col-width))
   contents
   (list (footer-string col-width))))


;; align-columns: integer? [listof string?] -> [listof string?]
(define (align-columns col-width contents)
  (map
   (curry fill-column col-width)
   contents))


;; fill-column: integer?  [listof string?] -> [listof string?]
(define (fill-column col-width content)
  (cond
    [(eq? content "\n") content]
    [(symbol? content)
     (make-string col-width (hash-ref ucode-hash content))]
    [(>= (string-length content) col-width) content]
    [else (fill-column col-width (string-append " " content))]))

;; max-row-length: [listof string?] -> integer?
(define (max-row-length contents)
  (apply max (map string-length contents)))

;; trim-tail: [listof string?] -> [listof string?]
(define (trim-tail contents)
  (let ([item (last contents)])
    (cond
      [(ormap (curryr regexp-match-exact? item) (list ;#px".*\\D"
                                                 #px"[[:space:]]"))
       (trim-tail
        (take contents
              (sub1 (length contents))))]
      [else contents])))

(define (trim-head contents)
  (let ([item (first contents)])
    (cond
      [(ormap (curryr regexp-match-exact? item) (list #px".*\\D"
                                                      #px"[[:space:]]"))
       (trim-head (drop contents 1))]
      [else contents])))

(define (header-string col-width)
  (string-append
   (make-string
    1
    #\U002A)
   "\n"))
  
(define (footer-string col-width)
  (string-append
   "\n"
   (make-string
    1
    #\U002A)))

;; --------------------------------------------------------------------------------------------------
;; RECOVER TREE FROM FORMATTING

;;sheet->tree: [listof string?] -> datum? 
;; attempt to rebuild sexp from formatting
(define (sheet->tree s)
  (split-children
   (car (split-applications s))))


(define (split-applications x)
  (cond 
    [(contains-application? x)
     (for/fold ([expr-stack '()]
                [newlines 0]
                [open? #f]
                #:result expr-stack)
               ([contents x])
       (cond
         [(eq? contents 'open-app)
          (values (cons 'app expr-stack) 0 #t)]
         [(eq? contents 'close-app)
          (values (pop-app expr-stack) 0 #f)]
         ; not interested in single lines
         [(eq? contents "\n")
          (values expr-stack (add1 newlines) open?)]
         [else
          ;; append any newlines for a later split children, before you add the values
          (let ([xprs (cond
                        [(> newlines 1)
                         (append (make-list newlines "\n")
                                 expr-stack)]
                        [else expr-stack])])
               
            (cond
              [open?
               (values (cons contents xprs) 0 open?)]
              [else
               (cond
                 [(> newlines 1)
                  (values (cons contents xprs) 0 #t)]
                 [else
                  (values xprs 0 #f)])]))]))]          
           
    [else x]))

(define (pop-app x)
  (let-values ([(expr rest) (split-at
                             x
                             (add1 (index-of x 'app)))
                            ])
    (let* ([xpr (reverse expr)]
           [app (first xpr)]
           [args (cdr xpr)])
        (cons
         (cons app (split-children args))
         rest))))

(define (get-index m)
  (car m))

(define (get-newlines m)
  (cdr m))  


(define (digit-str? x)
  (cond
    [(string? x)
     (regexp-match-exact? #px".*\\d" x)]
    [else #f]))
 

(define (split-children s)
  (let* ([markers (find-markers s)]
         [depth (max-marker-depth markers)])
    (cond
      [(> depth 1)
       (map split-children (split-list (filter-markers markers depth)
                                       s))]
      [else s])))
      
    

(define (split-list markers s [children '()])
  (cond
    [(null? markers) (append children (list s))]
    [else
     (split-list
      (cdr markers)
      (drop s (+ (get-index (car markers))
                 (get-newlines (car markers))))
      (append
       children
       (list (take s (get-index (car markers))))))]))



(define (filter-markers markers depth)
  (for/list ([m markers]
             #:when (= (cdr m) depth))
    m))


(define (max-marker-depth markers)
  (cond
    [(null? markers) 0]
    [else (apply max (map cdr markers))]))
            
;; find-markers: [listof string?] -> [listof pair?]
(define (find-markers s)
  (for/list ([(contents i) (in-indexed s)]
             #:when (eq? contents "\n"))
    (cons i (count-consecutive-newlines (drop s i)))))


(define (count-consecutive-newlines s [count 0])
  (cond
    [(null? s) count]
    [else
     (let ([item (first s)])
       (cond
         [(eq? item "\n")
          (count-consecutive-newlines
           (drop s 1)
           (add1 count))]
         [else count]))]))

;; --------------------------------------------------------------------------------------------------
;; EXAMPLES

#|
Examples on slack of remora arrays
1. (+ 1 2)
2. (+ [1 2] [3 4])
3. (* (+ 3 2) (- 3 2))
4. (+ (* [1 2] [3 4]) (* [5 6] [7 8])))
5. (+ 1 [2 3])
6. (+ 1 (* 2 3))
7a. {1 [2 3]}
7b. {[1] [2 3]}
|#

(displayln "Ex 0. (0 1 2)")
(sheet->display (tree->sheet '(1 2 3)))


(displayln "Ex 1. (+ 1 2)")
(sheet->display (tree->sheet '(+ 1 2)))

(displayln "Ex 2. (+ [1 2] [3 4])")
(sheet->display (tree->sheet '(+ [1 2] [3 4])))

(displayln "Ex 3. (* (+ 3 2) (- 3 2))")
(sheet->display (tree->sheet '(* (+ 3 2) (- 3 2))))

(displayln "Ex 4. (+ (* [1 2] [3 4]) (* [5 6] [7 8])))")
(sheet->display (tree->sheet '(+ (* [1 2] [3 4]) (* [5 6] [7 8]))))

(displayln "Ex 5. (+ 1 [2 3])")
(sheet->display (tree->sheet '(+ 1 [2 3])))

(displayln "Ex 6. (+ 1 (* 2 3))")
(sheet->display (tree->sheet '(+ 1 (* 2 3))))


(displayln "Ex 7a. {1 [2 3]}")
(sheet->display (tree->sheet '{1 [2 3]}))

(displayln "Ex 7b. {[1] [2 3]}")
(sheet->display (tree->sheet '{[1] [2 3]}))


(displayln "Ex 8. {{{1 2} {3 4}} {{5 6} {7 8}}}")
(sheet->display (tree->sheet '{{{1 2} {3 4}} {{5 6} {7 8}}}))



(let ([sheet (tree->sheet '(+ 1 2))])
  (displayln sheet)
  (sheet->display sheet)
  (sheet->tree sheet))

(let ([sheet (tree->sheet '(+ (1 2) (3 4)))])
  (displayln sheet)
  (sheet->display sheet)
  (sheet->tree sheet))

(let ([sheet (tree->sheet '(+ ((+ 1 2) (+ 3 4)) (4 5)))])
  (displayln sheet)
  (sheet->display sheet)
  (sheet->tree sheet))
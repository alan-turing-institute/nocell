#lang racket/base

(require "diags/trees.rkt"
         racket/format
         racket/function
         racket/string
         racket/list
         racket/match)


#|


Produces a ascii representation of a spreadsheet of an s-expression.


RULES:
x is a node
- (leaf? x) = put in the cell value
- to-parent / close bracket = empty row. After all leaf nodes have been evaluated, add an empty row. Only one empty row is added.
- to-child / open-bracket = new line 
- to-sibling = new line.
- Grid sum type = a single column

Currently only deals with one-dimensional layouts

TODO:

- parse brackets to allow for differentiating product types and sum types.
- lay product types out horizontally

|#


;; --------------------------------------------------------------------------------------------------
;; 

;;spreadsheet: pair? -> ?
(define (spreadsheet sexp)
  (display-sheet (tidy-layout
                  (flatten (format-tree sexp)))))

;;format-tree: pair? -> [listof [listof string?]]
;;traverses sexp into list of  strings
(define (format-tree x)
  (cond
    [(leaf? x) (format-leaf x)]
    [(symbol? x) (format-symbol x)]
    [(result? x) (format-result x)]
    [(has-children? x)
     (cond
       [(application? x)
        (format-app x)]
       [else
        (format-array-traversal x)])]))

(define (format-leaf x)
  (list (~a x) to-sibling))

(define (format-symbol x)
  (list (~a x) to-sibling))

(define (format-result x)
  (list close-app
        (map format-tree (result-result x))
        empty-row))

(define (format-array-traversal x)
  (list
   open-array
   (map format-tree x)
   close-array))
;to-parent))
   

(define (leaf? x)
  (integer? x))

(define (has-children? x)
  (pair? x))


;; --------------------------------------------------------------------------------------------------
;; APPLICATIONS

(struct app (fn args result))
(struct result (result))
(define (get-result app)
  (result-result (app-result app)))
  
(define (format-app x)
  (let ([app (structure-application x)])
    (append
     open-app
     (map format-tree (app-args app))
     (format-tree (app-result app)))))


(define (structure-application x)
  (app (car x)
       (cdr x)
       (result
        (evaluate-application x))))

(define (evaluate-application x)
  (let ([fn (car x)]
        [args (cdr x)])
    ((symbol->application fn)
     (parse-args args))))


(define (zip l1 l2)
  (map list l1 l2))


(define (extend-cell args)
  (make-list (length (cadr args))
             (car args)))

(define (application? l)
  (cond
    [(pair? l) (symbol? (car l))]
    [else #f]))

(define (not-application? l)
  (not (application? l)))

;; parse-args: pair? -> listof listof values?
(define (parse-args args)
  ;if any of the items are an application then recursively evaluate until they are not
  (cond
    [(ormap application? args) 
     (parse-args
      (for/list ([arg args])
        (cond
          [(application? arg)
           (evaluate-application arg)]
          [else arg])))]
    [else 
     
     (cond
       [(= (length args) 2)
        (cond 
          [(andmap list? args)
           ; zip if two lists that are not applications
           (zip (car args) (cadr args))]
          ; if a list and integer, extend the integer.
          [(and (integer? (car args))
                (list? (cadr args)))
           (zip (extend-cell args) (cadr args))]
          [(and (integer? (cadr args))
                (list? (car args)))
           (zip (extend-cell (reverse args)) (car args))]
          ; if two integers, package them in lists
          [(andmap integer? args)
           (list args)])]

       [(= (length args) 1)
        (car args)])]))
    
        
;; most of these act upon listof lists                 
(define (symbol->application sym)
  (match sym
    ['+ (curry map (curry apply +))]
    ['- (curry map (curry apply -))]
    ['* (curry map (curry apply *))]
    ['/ (curry map (curry apply /))]
    ['add1 (curry map (curry add1))]
    ['sum (curry apply +)]))

;; --------------------------------------------------------------------------------------------------
;; LAYOUT RULES


(define empty-row "\n")
(define empty-cell "cell")


;; "+": #\U002B; "-": #\U002D; "_": #\U005F; ".": #\U002E
(define ucode-hash
  (make-hash
   (list (cons 'open-array #\U2504)
         (cons 'close-array #\U2500)
         (cons 'open-app #\U2505)
         (cons 'close-app #\U2550))))

;; one-dimensional
(define open-array (list 'open-array "\n"))
(define close-array (list 'close-array "\n"))
(define open-app (list 'open-app "\n"))
(define to-sibling (list "\n"))
(define close-app (list 'close-app "\n"))


;; lay out products horizontally
#|
(define to-parent (list "\n" 'close "\n"))
(define to-child (list "\n" 'open "\n"))
(define to-sibling (list " "))
(define to-result (list "\n" 'eval "\n"))

|#

;; --------------------------------------------------------------------------------------------------
;; TIDYING OUTPUT POST-PROCESSING

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

;; display-sheet: [listof string?] -> .
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
;; LAYOUT BUILDING BLOCKS






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
(displayln "RULES:")
(displayln ucode-hash)
(displayln "and new line after function")

(displayln "Ex 0. (0 1 2)")
(spreadsheet '(1 2 3))


(displayln "Ex 1. (+ 1 2)")
(spreadsheet '(+ 1 2))

(displayln "Ex 2. (+ [1 2] [3 4])")
(spreadsheet '(+ [1 2] [3 4]))

(displayln "Ex 3. (* (+ 3 2) (- 3 2))")
(spreadsheet '(* (+ 3 2) (- 3 2)))

(displayln "Ex 4. (+ (* [1 2] [3 4]) (* [5 6] [7 8])))")
(spreadsheet '(+ (* [1 2] [3 4]) (* [5 6] [7 8])))

(displayln "Ex 5. (+ 1 [2 3])")
(spreadsheet '(+ 1 [2 3]))

(displayln "Ex 6. (+ 1 (* 2 3))")
(spreadsheet '(+ 1 (* 2 3)))


(displayln "Ex 7a. {1 [2 3]}")
(spreadsheet '{1 [2 3]})

(displayln "Ex 7b. {[1] [2 3]}")
(spreadsheet '{[1] [2 3]})

(displayln "{ (+ 1 2) (+ 3 4)}")
(spreadsheet '{ (+ 1 2) (+ 3 4)})

#|
(make-tree '(1 2 3))
(spreadsheet '(1 2 3))

(make-tree '((1 2) (3 4) (5 6)))
(spreadsheet '((1 2) (3 4) (5 6)))


(spreadsheet '(1 (2 3)))

(make-tree '((10 (20 40 (70 80)) (50 60))))
(spreadsheet '((10 (20 40 (70 80)) (50 60))))

(spreadsheet '(add1 (1 2 3)))
(spreadsheet '(+ (1 2 3) (10 11 12)))
|#

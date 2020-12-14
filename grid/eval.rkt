#lang racket/base

(require "builtins.rkt"
         "grid.rkt"
         racket/format
         racket/function
         racket/contract
         racket/list
         racket/match
         racket/math)

#| 

This module evaluates a Grid program.

1. Interface
2. Evaluation of references to atomic values
3. Evaluation of applications to atomic values.
4. Utility functions for resolving references in Grid coordinates.


|#


;; ---------------------------------------------------------------------------------------------------
;; Interface


(provide
 (contract-out
  [locate-labelled-cells (-> sheet? hash?)]
  [get-referent-indices (-> relative-location? indices? #:cell-hash hash? indices?)]
  [struct indices
    ([row exact-nonnegative-integer?]
     [column exact-nonnegative-integer?])]))
 

;; ---------------------------------------------------------------------------------------------------
;; Set up Grid program

(struct indices (row column) #:transparent)

;; evaluate-grid-program : program? -> program?
;; Return a grid-program with cells containing atomic values only 
(define (evaluate-grid-program program)
  (program (map evaluate-sheet (program-sheets program))))

;;evaluate-sheet : sheet? -> sheet?
(define (evaluate-sheet current-sheet)
  (define labels-indices (locate-labelled-cells current-sheet))
  (define indices-cell (build-indices current-sheet)) 
  (sheet
   (for/list ([(row i) (in-indexed (sheet-rows current-sheet))])
     (for/list ([(current-cell j) (in-indexed row)])
       (cell (evaluate-cell current-cell
                            (indices i j)
                            labels-indices
                            indices-cell)
             '())))))

;;evaluate-cell : cell? -> atomic-value?
(define (evaluate-cell current-cell
                       pos
                       labels-indices
                       indices-cell
                       [referee-chain '()])
  (cond [(circular-reference? referee-chain pos)  (raise-user-error (string-append
                                                                     "circular reference detected, referent: row "
                                                                     (~a (indices-row pos))
                                                                     ", column "
                                                                     (~a (indices-column pos))))]
        [else
         (define xpr (cell-xpr current-cell))
         (cond [(atomic-value? xpr) xpr]
               [(reference? xpr) (evaluate-reference xpr
                                                     pos
                                                     labels-indices
                                                     indices-cell
                                                     (append referee-chain (list pos)))]
               [(application? xpr) (evaluate-application xpr
                                                         pos
                                                         labels-indices
                                                         indices-cell)])]))

;; ---------------------------------------------------------------------------------------------------
;; Evaluate references

        

;; eq-indices? : indices? indices? -> boolean?
(define (eq-indices? pos1 pos2)
  (and
   (= (indices-row pos1)  (indices-row pos2))
   (= (indices-column pos1) (indices-column pos2))))

;; circular-reference? [listof indices?] indices? -> boolean?
(define (circular-reference? referee-chain current-referent)
  (cond [(empty? referee-chain) #f]
        [else (ormap (curry eq-indices? current-referent) referee-chain)]))

;;evaluate-reference : expression? indices? [hash-of label? indices?] -> atomic-value?
(define (evaluate-reference xpr [pos (indices 0 0)]
                            [labels-indices (hash)]
                            [indices-cell (hash)]
                            [referee-chain '()])

  (cond
    [(cell-reference? xpr)
     (let* ([referent-pos (resolve-location (cell-reference-loc xpr)
                                            pos
                                            labels-indices)]
            [referent-cell
             (cond [referent-pos
                    (hash-ref indices-cell referent-pos)]
                   [else (cell 'error:undef '())])])
       (evaluate-cell referent-cell
                      referent-pos
                      labels-indices
                      indices-cell
                      referee-chain))]
    [(range-reference? xpr)
     (raise-user-error "evaluation of range references not currently supported")]))




;;resolve-location : location? -> indices? 
(define (resolve-location loc
                          [pos (void)]
                          [cell-hash (hash)]
                          [source-label #f])
  (cond
    [(absolute-location? loc)
     (cond [(hash-has-key? cell-hash (absolute-location-label loc))
            (hash-ref cell-hash (absolute-location-label loc))]
           [else #f])]
              
    [(relative-location? loc)
     (get-referent-indices loc pos cell-hash)]))


;; ---------------------------------------------------------------------------------------------------
;; Evaluate Formulas

(define (evaluate-application app
                              [pos (indices 0 0)]
                              [labels-indices (hash)]
                              [indices-cell (hash)])


  (define (apply-fn fn args)
    (apply fn
           (map evaluate-expression args)))

  

  (define (format-app app)
    (let ([fn (application-fn app)]
          [args (application-args app)])
      (match fn
        ;; binary basic math
        ['+ (apply-fn + args)]
        ['- (apply-fn - args)]
        ['* (apply-fn * args)]
        ['/ (apply-fn / args)]
        ['quotient (apply-fn quotient args)]
        ['remainder (apply-fn remainder args)]
        ['modulo (apply-fn modulo args)]
        ;;unary basic maths
        ['neg (apply-fn - args)]
        ['abs (apply-fn abs args)]
        ['sgn (apply-fn sgn args)]
        ['inv (apply-fn (curry / 1) args)]
        ['floor (apply-fn floor args)]
        ['ceiling (apply-fn ceiling args)]
        ['truncate (apply-fn truncate args)]
        
        ;;binary comparison
        ['> (apply-fn > args)]
        ['< (apply-fn < args)]
        ['<= (apply-fn <= args)]
        ['>= (apply-fn >= args)]
        ['= (apply-fn = args)]
        ['!= (apply-fn (lambda (x y) (not (= x y))) args)]
        ;; unary trig
        ['exp (apply-fn exp args)]
        ['ln (apply-fn log args)]
        ['sqrt (apply-fn sqrt args)]
        ['acos (apply-fn acos args)]
        ['asin (apply-fn asin args)]
        ['atan (apply-fn atan args)]
        ['cos (apply-fn cos args)]
        ['sin (apply-fn sin args)]
        ['tan (apply-fn tan args)]
        ;; binary trig
        ['expt (apply-fn expt args)]
        ['log (apply-fn log args)]
        ;; unary combin
        ['factorial (apply-fn (lambda (x)
                                (for/product ([n (reverse (for/list ([n x]) (add1 n)))])
                                  n)) args)]
        ;;logical
        ['not (not (car args))]
        ['and (and (car args) (cadr args))]
        ['or (or (car args) (cadr args))]
        ['if (if (car args) (cadr args) (caddr args))]
        ;; date functions - TODO
        )))
        
        
  ;;evaluate-expression : expression? -> atomic-value?  
  (define (evaluate-expression xpr)
    (cond [(atomic-value? xpr) xpr]
          [(reference? xpr) (evaluate-reference xpr
                                                pos
                                                labels-indices
                                                indices-cell)]
          [(application? xpr) (evaluate-application app
                                                    pos
                                                    labels-indices
                                                    indices-cell)]))

  (if (not (builtin? (application-fn app)))
      (raise-user-error (string-append (~a (application-fn app) " function not in builtins"))) 
      (format-app app)))
          
          
     

;; ---------------------------------------------------------------------------------------------------
;; Utility functions for resolving references
  
;; locate-labelled-cells : sheet? -> [hash-of label? indices?]
;; Determine the grid locations of labelled cell
(define (locate-labelled-cells sheet)
  (for*/hash ([(row i) (in-indexed (sheet-rows sheet))]
              [(cell j) (in-indexed row)]
              #:when (labelled-cell? cell))
    (values (labelled-cell-lbl cell)
            (indices i j))))

;; build-indices : sheet? -> [hash-of indices? cell?]
(define (build-indices sheet)
  (for*/hash ([(row i) (in-indexed (sheet-rows sheet))]
              [(cell j) (in-indexed row)])
    (values (indices i j)
            cell)))

  

;; get-referent-indices :  relative-location? indices? hash? -> indices?
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
;; Tests

(module+ test
  (require rackunit)

  ;sheet with atomic values
  (define sheet-atomic (sheet
                        (list
                         (list (labelled-cell 1 '() "cellA1") (labelled-cell 2 '() "cellB1")))))
                     

  (check-equal? (locate-labelled-cells sheet-atomic)
                (hash "cellA1" (indices 0 0)
                      "cellB1" (indices 0 1)))

  (check-equal? (build-indices sheet-atomic)
                (hash (indices 0 0) (labelled-cell 1 '() "cellA1")
                      (indices 0 1) (labelled-cell 2 '() "cellB1")))

  (check-equal? (evaluate-sheet sheet-atomic)
                (sheet
                 (list
                  (list (cell 1 '()) (cell 2 '())))))

  ;sheet with atomic values and references
  (check-false
   (circular-reference? '() (indices 0 0))) ;not a reference call
  (check-false
   (circular-reference? (list (indices 0 1)) (indices 0 0))) ;a single reference call

  (check-true
   (circular-reference? (list (indices 0 1)
                              (indices 0 0))
                        (indices 0 1))) ;a double reference chain, with the first referent calling the initial referee cell.
   
  
  (define sheet-refs (sheet
                      (list
                       (list (labelled-cell 1 '() "cell1")
                             (cell (cell-reference (absolute-location "cell1")) '())))))
  (check-equal? (evaluate-sheet sheet-refs)
                (sheet
                 (list
                  (list (cell 1 '())
                        (cell 1 '())))))

  (check-exn
   exn:fail:user?
   (lambda ()
     (evaluate-sheet
      (sheet
       (list
        (list (labelled-cell
               (cell-reference (absolute-location "cell2")) '() "cell1")
              (labelled-cell
               (cell-reference (absolute-location "cell1")) '() "cell2"))))))
   "circular reference detected, referent: row 0, column 0")


  (check-equal?
   (evaluate-sheet (sheet (list (list (cell (application '+ '(3 2)) '())))))
   (sheet (list (list (cell 5 '())))))
  )
  

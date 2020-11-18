#lang racket/base

#|

Tests to exercise the grammar of Grid

TODO:
 - trickier expressions
 - cells, sheets, and programs
 - styles
 - maybe builtins with known arities
|#


(require "../grid/grid.rkt")

(module+ test
  (require rackunit)
  
  (define avs
    (list 42.0 "foo" #t #f 'error:arg 'error:val 'error:undef 'nothing))

  (test-case
      "Grid atomic values"
    (for ([v avs])
      (check-pred atomic-value? v)))
 
  (define M
    (matrix #(#(0.0 1.0) #(2.0 3.0))))

  (define rs
    (list (cell-reference  (absolute-location "the-cell"))
          (range-reference (absolute-location "the-cell-tl") (absolute-location "the-cell-br"))
          (cell-reference  (relative-location "the-cell-src" "the-cell-tgt"))
          (range-reference (absolute-location "the-cell")
                           (relative-location "the-cell-src" "the-cell-tgt"))))
  (test-case
      "Grid values"
    ;; Matrices are values
    (check-pred value? M)
    (check-equal? (matrix-ref M 1 0) 2.0)
    ;; References are values
    (for ([r rs])
      (check-pred reference? r)
      (check-pred value? r)
      (check-pred expression? r)))

  (test-case
      "Grid expressions"
    (check-pred expression? 1)
    (check-pred expression? (application '+ '(1 2)))
    
    (check-pred builtin? '+)
    (check-false (builtin? (gensym)))
    (check-exn exn:fail?
               (λ () (expression? (application (gensym) '(1 2)))))))

#lang racket/base

(require "grid.rkt"
         "builtins.rkt")

(module+ test
  (require rackunit)

  ;; labels
  (check-true (label? "a"))
  (check-false (label? 'a))
  (check-false (label? 1))

  ;; expressions
  (check-true (expression? 1))
  (check-true (expression? (application '+ '(1 2))))

  ;; application-fn should be a builtin
  (check-true (builtin? '+))
  (check-false (builtin? (gensym)))
  (check-exn exn:fail? (λ () (expression? (application (gensym) '(1 2)))))

  ;; values, atomic-values, references
  (check-true (matrix? (matrix #[#[1 2 3]
                                 #[4 5 6]
                                 #[7 8 9]])))

  (define avs
    (list 42.0 "foo" #t #f 'error:arg 'error:val 'error:undef 'nothing))

  (for ([v avs])
    (check-pred atomic-value? v)
    (check-pred value? v)
    (check-pred expression? v))

  (define m (matrix #(#(0.0 1.0) #(2.0 3.0))))
  (check-pred value? m)
  (check-pred expression? m)
  (check-equal? (matrix-ref m 1 0) 2.0)

  (define rs
    (list (cell-reference  (absolute-location "the-cell"))
          (range-reference (absolute-location "the-cell-tl") (absolute-location "the-cell-br"))
          (cell-reference  (relative-location "the-cell-src" "the-cell-tgt"))
          (range-reference (absolute-location "the-cell")
                           (relative-location "the-cell-src" "the-cell-tgt"))))

  (for ([r rs])
    (check-pred reference? r)
    (check-pred value? r)
    (check-pred expression? r))

  ;;

  )

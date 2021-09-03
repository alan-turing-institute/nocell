#lang racket/base


(require pict/tree-layout
         pict
         racket/format
         racket/class
         racket/function
         racket/string)


#|

Saves an expression tree from a (quote s-expression?)

|#


;; --------------------------------------------------------------------------------------------------
;;  INTERFACE


(provide tree->diag)

;; --------------------------------------------------------------------------------------------------
;; Tree Traversal

;;tree->diag: pair? -> tree-layout?
(define (tree->diag x)
  (naive-layered (traverse-tree x)))


;;traverse-tree: datum? -> tree-layout? 
(define (traverse-tree x)
  (cond
    [(null? x) #f]
    [(integer? x) (make-node x)]
    [(pair? x)
     (cond
       [(application? x)
        (apply (curry tree-layout #:pict (app (car x)))
               (map traverse-tree (cdr x)))]
       [else
        (apply (curry tree-layout #:pict plus)
            (map traverse-tree x))])]))

(define (application? l)
  (cond
    [(pair? l) (symbol? (car l))]
    [else #f]))

;; --------------------------------------------------------------------------------------------------
;; Picture constructors

(define white-circle (colorize
                      (disk 25 #:draw-border? #t
                            #:border-color "black") "white"))

(define plus (cc-superimpose
              (colorize
               (disk 15) "white")
              (text "⊕" null 20)))

(define (app x) (cc-superimpose
              white-circle
              (text (~a x) null 10)))

(define (make-node sexp)
  (tree-layout #:pict (cc-superimpose
                       white-circle
                       (text (~a sexp)))))


(define (add-letter the-pict letter)
  (vl-append
   (text letter (cons 'bold 'roman) 12)
   the-pict))


(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))



;; --------------------------------------------------------------------------------------------------
;; Examples


(define examples
  (list
   '(10 20 30)
   '(10 (20 30))
   '(10 (20 (30 40)))
   '((10 20) (30 (40 50)))
   '((5 (10 20)) (30 (40 50)))
   '((5 (10 20)) (30 (40 (50 60))) (70 80))
   '(+ 1 2)
   '(+ 1 [2 3])
   '(+ 1 (* 2 3))
   '(+ (* [1 2] [3 4]) (* [5 6] [7 8]))
   '(app ((app 1 2) (app 3 4)) (4 5))
   ))
 
(for ([(eg i) (in-indexed examples)])
  (let ([diag (tree->diag eg)])
    (displayln (string-join (list
                             "Ex "
                             (~a i)
                             ": "
                             (~a eg))))
    (println diag)
    (save-pict diag (string-join (list
                                  "tree"
                                  (~a i)
                                  ".png")) 'png)))


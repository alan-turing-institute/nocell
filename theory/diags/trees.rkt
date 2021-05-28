#lang racket/base


(require pict/tree-layout
         pict
         racket/format
         racket/class)


#|

Saves an expression tree from a (quote s-expression?)

Only supports three sibling nodes.

|#






(define white-circle (colorize
                      (disk 25 #:draw-border? #t
                            #:border-color "black") "white"))

(define plus (cc-superimpose
              (colorize
               (disk 15) "white")
              (text "⊕" null 20)))

(define (make-node sexp)
  (tree-layout #:pict (cc-superimpose
                       white-circle
                       (text (~a sexp)))))


(define (make-tree sexp)
  (naive-layered (traverse-tree sexp)))

(define (traverse-tree sexp)
  (cond
    [(null? sexp) #f]
    [(integer? sexp) (make-node sexp)]
    
    [(= (length sexp) 2)
     (tree-layout #:pict plus
                  (traverse-tree (car sexp))
                  (traverse-tree (cadr sexp)))]
    [(= (length sexp) 3)
     (tree-layout #:pict plus
                  (traverse-tree (car sexp))
                  (traverse-tree (cadr sexp))
                  (traverse-tree (caddr sexp)))]))

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))


(define (add-letter the-pict letter)
  (vl-append
   (text letter (cons 'bold 'roman) 12)
   the-pict))


(make-tree '(10 20 30))
(save-pict (make-tree '(10 20 30)) "tree1.png" 'png)

(make-tree '(10 (20 30)))
(save-pict (make-tree '(10 (20 30))) "tree2.png" 'png)

(make-tree '(10 (20 (30 40))))
(save-pict (make-tree '(10 (20 (30 40)))) "tree3.png" 'png)

(make-tree '((10 20) (30 (40 50))))
(save-pict (make-tree '((10 20) (30 (40 50)))) "tree4.png" 'png)

(make-tree '((5 (10 20)) (30 (40 50))))
(save-pict (make-tree '((5 (10 20)) (30 (40 50)))) "tree5.png" 'png)


(make-tree '((5 (10 20)) (30 (40 (50 60))) (70 80)))
(save-pict (make-tree '((5 (10 20)) (30 (40 (50 60))) (70 80))) "tree6.png" 'png)








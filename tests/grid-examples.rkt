#lang racket/base

#|

Some larger examples of Grid

|#

(require racket/list)
(require "../grid/grid.rkt")

(provide multiplication-table
         bubbly
         budget)


;; A spreadsheet of numbers

(define multiplication-table
  (program
   (list (sheet 
          (for/list ([i 12])
            (for/list ([j 12])
              (cell (* (+ i 1) (+ j 1)))))))))

;; A row-by-row bubble sort

(define (zip l1 l2)
  (map cons l1 l2))

(define (bubble-one lst sorted acc)
  (if (null? lst)
      (reverse acc)
      (bubble-step '() (car lst) (cdr lst) sorted acc)))

(define (bubble-step left curr right sorted acc)
  (define new-acc (cons (stitch left curr right sorted) acc))
  (if (null? right)
      ;; curr is greater than all in left and smaller than all in sorted
      (bubble-one (reverse left)
                  (cons curr sorted)
                  new-acc)
      (let ([next (car right)])
        (let-values ([(small big) (order curr next)])
          (bubble-step (cons small left) big (cdr right) sorted new-acc)))))

(define (stitch left curr right sorted)
  (append (reverse left) (list curr) right sorted))

(define (order a b)
  (if (string<=? a b)
      (values a b)
      (values b a)))

(define bubbled
  (bubble-one '("Geddes" "Strickson" "Mole" "Counsell" "MacKay") '() '()))

(define (label-cell-with-row-index index c)
  (labelled-cell c (format "~a-~a" c index)))

(define (label-and-ref-cell-with-row-index index c)
  (labelled-cell
   (cell-reference (absolute-location (format "~a-~a" c (- index 1))))
   (format "~a-~a" c index)))

(define bubbled-rows
  (cons
   (map (λ (c) (label-cell-with-row-index 0 c)) (car bubbled))
   (for/list ([row-index (in-range (length (cdr bubbled)))]
              [row       (in-list (cdr bubbled))])
     (map (λ (c) (label-and-ref-cell-with-row-index (+ row-index 1) c)) row))))

(define bubbly
  (program (list (sheet bubbled-rows))))


;; a simple household budget

(define labels (list "Income" "Rent" "Energy" "Water" "Council Tax"))
(define inputs (list 1800 700 100 20 100))

(define (zip-labelled-columns l1 l2)
  (map (lambda(r1 r2)
         (list (cell r1) (labelled-cell r2 r1)))
       l1 l2))

(define (ref-cell lab)
  (cell-reference (absolute-location lab)))
  
(define (sum-labelled-cells lst)
  (if (= (length lst) 2)
      (application '+ (map ref-cell lst))
      (application '+ (list
                       (ref-cell (car lst))
                       (sum-labelled-cells (cdr lst))))))

(define budget
  (program
   (list (sheet
          (append
           (zip-labelled-columns labels inputs)
           (list
            (list (cell "Total known costs") (labelled-cell
                                              (sum-labelled-cells (cdr labels))
                                              "Total"))
            (list (cell "Spending budget") (cell
                                            (application '-
                                                         (list
                                                          (ref-cell "Income")
                                                          (ref-cell "Total")))))))))))
                  
                  


#lang racket

(module+ test (require rackunit))

(provide column-letter->integer
         integer->column-letter)

;; --------------------------------------------------------------------------------
;;
;; convert a numeric (zero-indexed) column reference to and from a
;; column letter (A,...,Z,AA,...,AZ,...,ZZ,AAA,...) 

(define codepoint-A 65)

(define (integer->column-letter n)
  (define (recur rems b)
    (if (< (car rems) b)
        rems
        (let-values ([(q r) (quotient/remainder (car rems) b)])
          (recur (list* (- q 1) r (cdr rems)) b))))
  (list->string
   (map (lambda (d) (integer->char (+ d codepoint-A)))
        (recur (list n) 26))))

(define (column-letter->integer cs)
  (let ((ds (map (lambda (c) (- (char->integer c) codepoint-A))
                 (string->list cs))))
    (- (foldl (lambda (a b) (+ (* 26 b) (+ a 1))) 0 ds) 1)))

;; Check we can write column names correctly
(module+ test
  (test-case "Column letter"
    (check-equal? (integer->column-letter 0)   "A")
    (check-equal? (integer->column-letter 1)   "B")
    (check-equal? (integer->column-letter 25)  "Z")
    (check-equal? (integer->column-letter 26)  "AA")
    (check-equal? (integer->column-letter 701) "ZZ")
    (check-equal? (integer->column-letter 702) "AAA")
    (check-equal? (integer->column-letter 728) "ABA")
    (for-each
     (lambda (n)
       (with-check-info
         (('current-element n))
         (check-equal? n
                       (column-letter->integer
                        (integer->column-letter n)))))
     (range 0 10000))))

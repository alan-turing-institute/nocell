#lang racket

(provide
 (contract-out
  [expr->stack (-> expr? stack?)]
  [stack->expr (-> stack? (listof expr?))]
  ;;
  (struct stack-entry ([item stack-item?]
                       [depth natural?]))
  [expr->fstack (-> expr? fstack?)]
  ;;
  [expr->expression (-> expr? expression?)]
  [expression-unwrap (-> expression? sheet?)]))

(require "grid.rkt"
         "builtins.rkt")

;;;;;;;;;;;;;;;;;;;;;;
;; Take an expression tree and produce a list of stack operations

(define (op? v)
  (and (pair? v)
       (builtin? (car v))
       (natural? (cdr v))))

(define op cons)
(define op-name car)
(define op-arity cdr)

(define (expr? v)
  (or (atomic-value? v)
      (and (builtin? (car v))
           (andmap expr? (cdr v)))))

(define stack-item? (or/c op? atomic-value?))
(define stack? (listof stack-item?))

;; expr->stack : expr? -> stack?
(define (expr->stack t)
  (if (atomic-value? t)
      (list t)
      (append
       (apply append (map expr->stack (cdr t)))
       (list (op (car t) (length (cdr t)))))))

;; stack->expr : stack? -> (listof expr?)
(define (stack->expr p [subexprs null])
  (cond
    [(null? p) (reverse subexprs)]
    ;;
    [(atomic-value? (car p))
     (stack->expr (cdr p) (cons (car p) subexprs))]
    ;;
    [(op? (car p))
     (let-values ([(args subexprs*) (split-at subexprs (op-arity (car p)))])
       (stack->expr (cdr p)
                    (cons (cons (op-name (car p)) (reverse args))
                          subexprs*)))]))

(module+ test
  (require rackunit)

  (check-equal? (stack->expr '(1 2 3)) '(1 2 3))

  (define test-exprs-and-stacks
    (list (cons '(+ (* 1 2) (* 3 4))
                '(1 2 (* . 2) 3 4 (* . 2) (+ . 2)))
          ;;
          (cons '(+ (* 1 2 (+ 3 4)) 5)
                '(1 2 3 4 (+ . 2) (* . 3) 5 (+ . 2)))
          ;;
          (cons '(+ 1 (+ 2 (+ 3 4)))
                '(1 2 3 4 (+ . 2) (+ . 2) (+ . 2)))))

  (for ([e+s test-exprs-and-stacks])
    (let ([e (car e+s)]
          [s (cdr e+s)])
      (check-equal? (expr->stack e) s)
      (check-equal? (stack->expr s) (list e))))
  ;;
  )

;;;;;;;;;;;;;;;;;;;;;;
;; With formatting information - leaves are 'high'

(struct stack-entry (item depth) #:transparent)

;; fstack -- stacks with formatting information
(define fstack? (listof stack-entry?))

(define (expr->fstack t [n 0])
  (if (atomic-value? t)
      (list (stack-entry t n))
      (append
       (apply append (map (curryr expr->fstack (add1 n)) (cdr t)))
       (list
        (stack-entry (op (car t) (length (cdr t)))
                     n)))))

(module+ test
  (check-equal?
   (expr->fstack '(+ 1 (* 2 3)))
   (list
    (stack-entry 1 1)
    (stack-entry 2 2)
    (stack-entry 3 2)
    (stack-entry '(* . 2) 1)
    (stack-entry '(+ . 2) 0)))
  ;;
  )

;;;;;;;;;;;;;;;;;;;;;;
;; Version taking a 'grid' expression and making a sheet with one
;; value per row.  No formatting.

;; --------------------
;; A few utilities from the nocell prototype
;;
(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))

(define (make-name base n)
  (format "~a~a" base n))

(define (name-generator name [name-counter 0])
  (λ () (make-name name (post-inc! name-counter))))

(define current-name-generator
  (make-parameter (name-generator "t")))

;; --------------------

;; list->expression : expr? -> expression?
(define (expr->expression t)
  (if (atomic-value? t)
      t
      (application (car t) (map expr->expression (cdr t)))))

(module+ test
  (require rackunit)
  (check-equal? (expr->expression '(+ 1 2))
                (application '+ '(1 2)))
  (check-equal? (expr->expression '(* 1 (+ 2 3) 4))
                (application '* (list 1 (application '+ '(2 3)) 4))))


(define (sheet-append . sheets)
  (sheet (apply append (map sheet-rows sheets))))

;; expression-unwrap : expression? -> sheet?
(define (expression-unwrap expr)
  (define unique-name (current-name-generator))
  (cond
    [(atomic-value? expr)
     (sheet (list (list (labelled-cell expr (unique-name)))))]
    ;;
    [(application? expr)
     (let* ([arg-sheets (map expression-unwrap (application-args expr))]
            [all-args-sheet (apply sheet-append arg-sheets)]
            [arg-labels
             ;; extract the label from the only cell in each row
             (map (compose1 labelled-cell-lbl car)
                  ;; the last row contains the result of the application
                  (map (compose1 last sheet-rows) arg-sheets))])
       (sheet-append
        all-args-sheet
        (sheet
         (list
          (list
           (labelled-cell
            (application
             (application-fn expr)
             (map (compose1 cell-reference absolute-location) arg-labels))
            (unique-name)))))))]))

(module+ test
  (parameterize [(current-name-generator (name-generator "t"))]
    (check-equal?
     (expression-unwrap (expr->expression '(* 1 (* 2 (* 3 4)))))
     (sheet
      (list
       (list (labelled-cell 1 "t0"))
       (list (labelled-cell 2 "t1"))
       (list (labelled-cell 3 "t2"))
       (list (labelled-cell 4 "t3"))
       (list
        (labelled-cell
         (application
          '*
          (list
           (cell-reference (absolute-location "t2"))
           (cell-reference (absolute-location "t3"))))
         "t4"))
       (list
        (labelled-cell
         (application
          '*
          (list
           (cell-reference (absolute-location "t1"))
           (cell-reference (absolute-location "t4"))))
         "t5"))
       (list
        (labelled-cell
         (application
          '*
          (list
           (cell-reference (absolute-location "t0"))
           (cell-reference (absolute-location "t5"))))
         "t6"))))))
  ;;
  )

#lang racket/base

(require racket/contract)

#| 

Declarations of built in-functions

Built-in functions are defined by their name, an arity, and the type of each
argument.

Arguments should be implicitly "promoted" in the following way:

1. If the function requires an atomic-value? and it is given a cell reference,
   then the value of the cell referred to is used as the argument. 

2. If the function requires a matrix? and it is given a range reference, then
   the matrix formed by the values of the cells referred to is used as the
   argument.

3. If the function requires a matrix? and is given an atomic-value?, them the
   atomic value is treated as a 1x1 matrix.

|#

(provide
 (contract-out
  
  ;; Types of arguments to, and return values from, a built-in function
  [builtin-arg-type? (-> any/c boolean?)]

  ;; The type of a built-in function
  [struct builtin-type
    ((arity     (or/c #f exact-nonnegative-integer?))
     (arg-types (or/c builtin-arg-type? (listof builtin-arg-type?)))
     (ret-type   builtin-arg-type?))]

  ;; Hashmap of builtin symbols and their types
  [builtins (hash/c symbol? builtin-type? #:immutable #t)]))

(struct builtin-type (arity arg-types ret-type) #:transparent)

(define builtin-arg-type?
  (or/c 'number? 'string? 'boolean? 'date? 'error?
        'matrix? 'reference?
        'value? ; A value? is a matrix? or a reference?
        ))

(define (make-unary-function fn arg-type ret-type)
  (cons fn (builtin-type 1 (list arg-type) ret-type)))

(define (make-binary-function fn arg-type1 arg-type2 ret-type)
  (cons fn (builtin-type 2 (list arg-type1 arg-type2) ret-type)))

(define (make-ternary-function fn arg-type1 arg-type2 arg-type3 ret-type)
  (cons fn (builtin-type 3 (list arg-type1 arg-type2 arg-type3) ret-type)))

;; A "list function" takes any number of arguments (but at least one) all of
;; which must be the same type
(define (make-list-function fn arg-type ret-type)
  (cons fn (builtin-type #f arg-type ret-type)))



;; ---------------------------------------------------------------------------------------------------
;; Constants

(define *constants*
  (list
   (cons 'pi    (builtin-type 0 null 'number?))
   (cons 'true  (builtin-type 0 null 'boolean?))
   (cons 'false (builtin-type 0 null 'boolean?))))

;; ---------------------------------------------------------------------------------------------------
;; Numerical functions

;; --- Basic maths functions
(define *maths-basic*
  (append
   (map (λ (f) (make-unary-function f 'number? 'number?))
        '(neg       ; additive inverse
          abs       ; absolute value
          sgn       ; sign function
          inv       ; multiplicative inverse
          floor     ; largest integer no more than (by >) its argument
          ceiling   ; smallest integer at least as large as its argument 
          truncate  ; integer farthest from 0 that is not father than its argument
          ))
   (map (λ (f) (make-binary-function f 'number? 'number? 'number?))
        '(+         ; addition
          -         ; subtraction
          *         ; multiplication
          /         ; division
          quotient  ; same as (truncate (/ m n))
          remainder ; remainder after quotient, having same sign as m
          modulo    ; See: https://docs.racket-lang.org/reference/generic-numbers.html#(def._((quote._~23~25kernel)._modulo))
          ))
   (map (λ (f) (make-binary-function f 'number? 'number? 'boolean?))
        '(>         ; greater than
          <         ; less than
          >=        ; greater than or equal 
          <=        ; less than or equal
          =         ; equality
          !=        ; not equal
          ))))

;; --- Exponentials, logarithms, and trigonometric functions
(define *maths-trig*
  (append
   (map (λ (f) (make-unary-function f 'number? 'number?))
        '(exp       ; exponentiation
          ln        ; natural logarithm
          sqrt      ; square root
          acos      ; arccosine
          asin      ; arcsine
          atan      ; arctangent
          cos       ; cosine
          sin       ; sine
          tan       ; tangent
          ))
    (map (λ (f) (make-binary-function f 'number? 'number? 'number?))
        '(expt      ; x^y
          log       ; log to base m
          ))))

;; --- Statistical 
(define *maths-stats*
  (append
   (map (λ (f) (make-unary-function f 'number? 'number?))
        '())))

;; Combinatorial
(define *maths-combin*
  (append
   (map (λ (f) (make-unary-function f 'number? 'number?))
        '(factorial ; factorial
          ))))


;; ---------------------------------------------------------------------------------------------------
;; Logical functions

(define *logical*
  (append
   (map (λ (f) (make-unary-function f 'boolean? 'boolean?))
        '(not ; Logical NOT
          ))
   (map (λ (f) (make-binary-function f 'boolean? 'boolean? 'boolean?))
        '(and ; logical AND
          or  ; logical OR
          ))
   (map (λ (f) (make-ternary-function f 'boolean? 'value? 'value? 'value?))
        '(if  ; IF expression 
          ))
   ))



;; ---------------------------------------------------------------------------------------------------
;; String functions

;; ---------------------------------------------------------------------------------------------------
;; Date fuctions

(define *date*
  (list
   ;; date : year month day -> date?
   (make-ternary-function 'date 'numeric? 'numeric? 'numeric? 'date?)

   ;; date-day, date-month, and date-year : extract relevant component
   (make-unary-function 'date-day 'date? 'numeric?)
   (make-unary-function 'date-month 'date? 'numeric?)
   (make-unary-function 'date-year 'date? 'numeric?)

   ;; Subtracting two dates gives a duration in days
   (make-binary-function 'date-days 'date? 'date? 'numeric?)
   (make-binary-function 'date-add-days 'date? 'numeric? 'date?)))

;; ---------------------------------------------------------------------------------------------------
;; Reference functions

;; ---------------------------------------------------------------------------------------------------
;; Combine all functions in a hash

(define builtins
  (make-immutable-hasheq
   (append
    *constants*
    *maths-basic*
    *maths-trig*
    ;   *maths-stats*
    *maths-combin*
    *logical*
    ; *string*
    *date*
    )))


#|

Note: the following are in the OpenFormula "Small" group but do not have a counterpart in Sheet

Infix Operator "&”; 
Postfix Operator “%”;
Infix Operator Reference Intersection ("!");
Infix Operator Range (":").

 ATAN2
 AVERAGE 6.18.3 
 AVERAGEIF 6.18.5 
 CHOOSE 6.14.3 
 COLUMNS 6.13.5 

 COUNT 6.13.6 
 COUNTA 6.13.7 
 COUNTBLANK 6.13.8 
 COUNTIF

 DAVERAGE 6.9.2 
 DCOUNT 6.9.3 
 DCOUNTA

 DDB 6.12.14 
 DEGREES 6.16.25 
 DGET 6.9.5 
 DMAX 6.9.6 
 DMIN 6.9.7 

 DPRODUCT 6.9.8 
 DSTDEV 6.9.9 
 DSTDEVP 6.9.10 
 DSUM 6.9.11 
 DVAR 6.9.12 

 DVARP 6.9.13 
 EVEN 6.16.30 
 EXACT 6.20.8 

 FIND 6.20.9 
 FV 6.12.20 
 HLOOKUP 6.14.5 
 HOUR 6.10.10 

 INDEX 6.14.6 
 INT 6.17.2 
 IRR 6.12.24 
 ISBLANK 6.13.14 
 ISERR 6.13.15 

 ISERROR 6.13.16 
 ISLOGICAL 6.13.19 
 ISNA 6.13.20 
 ISNONTEXT 6.13.21 

 ISNUMBER 6.13.22 
 ISTEXT 6.13.25 
 LEFT 6.20.12 
 LEN 6.20.13 

 LOWER 6.20.14 
 MATCH 6.14.9 
 MAX 6.18.45 
 MID 6.20.15 
 MIN 6.18.48 
 MINUTE 6.10.12 
 MOD 6.16.42 
 N 6.13.26 
 NA 6.13.27 
 NOW 6.10.15 
 NPER 6.12.29 
 NPV 6.12.30 

 ODD 6.16.44 
 PMT 6.12.36 
 POWER 6.16.46 
 PRODUCT 6.16.47 
 PROPER 6.20.16 
 PV 6.12.41 
 RADIANS 6.16.49 
 RATE 6.12.42 
 REPLACE 6.20.17 
 REPT 6.20.18 
 RIGHT 6.20.19 
 ROUND 6.17.5 
 ROWS 6.13.30 
 SECOND 6.10.16 
 SLN 6.12.45 
 STDEV 6.18.72 
 STDEVP 6.18.74 
 SUBSTITUTE 6.20.21 
 SUM 6.16.61 
 SUMIF 6.16.62 
 SYD 6.12.46 

 T 6.20.22 
 TIME 6.10.17 
 TODAY 6.10.19 
 TRIM 6.20.24 

 TRUNC 6.17.8 
 UPPER 6.20.27 
 VALUE 6.13.34 
 VAR 6.18.82 
 VARP 6.18.84 
 VLOOKUP 6.14.12 
 WEEKDAY 6.10.20 


|#

#lang racket

;; A racket program that emits the team's monthly project-person
;; allocation report for Finance.

;; https://github.com/alan-turing-institute/nocell/issues/49

;; This example could eventually move into its own repository, with
;; nocell and whatnow as dependencies


(require (rename-in gregor
                    [date gregor:date]
                    [date? gregor:date?])
         whatnow/forecast
         whatnow/schedule
         "../grid/grid.rkt"
         "../ods/ods.rkt")

;; ----------------------------------------
;; Utility

;; Orders a and b 'lexicographically' by applying each function in
;; 'keys' to both a and b in turn, and comparing the results with '<'
;; and '='.
;;
;; lexicographic<=?
;;  : (listof (-> any/c any/c)) (-> any/c any/c) (-> any/c any/c)
;;      -> any/c any/c -> boolean?
(define ((lexicographic<=? keys < =) a b)
  (or (null? keys)
      (let ([key (car keys)])
        (or (< (key a) (key b))
            (and (= (key a) (key b))
                 ((lexicographic<=? (cdr keys) < =) a b))))))


;; ----------------------------------------
;; Forecast monthly allocation data
;;
;; - Could use the data-frame library here (but avoiding the
;;   dependency for now)

;; project-code? : string? -> boolean?
(define (project-code? t)
  (regexp-match? #px"^R-[[:alnum:]]+-[[:alnum:]]+$" t))

;;
;; Allocation from people to projects for a range of months
;; 
;;  - The triple (client, project, person) uniquely specifies an allocation
;;
;;  - As a 'wide table': the field month-fraction is a hash table from
;;    month (the gregor:date of the first day in the month) to fractional
;;    allocation
(struct/contract allocation
  ([client       string?]
   [project      string?]
   [code         (or/c project-code? "")]
   [person       string?]
   [month-fraction (hash/c gregor:date? real? #:flat? #t)])
  #:transparent)

;; Order allocations lexicographically by (client, project, person)
;;
;; allocation<=? : allocation? allocation? -> boolean?
(define allocation<=? (lexicographic<=? (list allocation-client
                                              allocation-project
                                              allocation-person)
                                        string<?
                                        string=?))

(define example-allocations
  (list
   (allocation "Data science for science and the humanities"
               "Interesting Ocean"
               "R-XYZ-001"
               "Oliver Strickson"
               (hash (gregor:date 2020 04) 0.5
                     (gregor:date 2020 05) 0.5))
   (allocation "Data-centric engineering"
               "Ubiquitous Mist"
               "R-ABC-999"
               "Callum Mole"
               (hash (gregor:date 2020 04) 0.5))
   (allocation "Data-centric engineering"
               "Ubiquitous Mist"
               "R-ABC-999"
               "James Geddes"
               (hash (gregor:date 2020 05) 0.5))))


;; ----------------------------------------
;; Date interval helpers

;; Represents the half-open date interval [start, end).
;; (`start > end` is allowed, and represents an empty interval)
(struct/contract date-interval ([start gregor:date?]
                                [end gregor:date?])
                 #:transparent)

(define (date-min a b) (if (date<? a b) a b))
(define (date-max a b) (if (date>=? a b) a b))

;; date-interval-intersect : date-interval? date-interval? -> date-interval?
(define (date-interval-intersect I J)
  (let ([start (date-max (date-interval-start I) (date-interval-start J))]
        [end* (date-min (date-interval-end I) (date-interval-end J))])
    (date-interval start (date-max start end*))))

;; date-interval-days : date-interval? -> exact-integer?
(define (date-interval-days I)
  (days-between (date-interval-start I) (date-interval-end I)))

;; workday? : gregor:date? -> boolean?
(define (workday? d) (not (or (saturday? d) (sunday? d))))

;; in-date-interval : date-interval? -> stream?
(define (in-date-interval I)
  (let ([start (date-interval-start I)]
        [end (date-interval-end I)])
    (if (date>=? start end)
        empty-stream
        (stream-cons
         start
         (in-date-interval (date-interval (+days start 1) end))))))

;; The number of working days in the given date interval
;;
;; net-workdays : date-interval? -> exact-nonnegative-integer?
(define (net-workdays I)
  (let-values ([(start) (date-interval-start I)]
               [(w d) (quotient/remainder (date-interval-days I) 7)])
    ;; five working days per full week, plus remaining days determined by workday?
    (+ (* 5 w)
       (sequence-count workday?
                       (in-date-interval
                        (date-interval start (+days start d)))))))

;; A list of the months comprising the given financial year
;;
;; fy-months : exact-integer? -> (listof gregor:date?)
(define (fy-months y) (map (curry +months (gregor:date 2020 03)) (range 12)))


;; ----------------------------------------
;; Retreive data from Forecast via whatnow, returned as 'allocation' objects

;; Conversion factor for Forecast assignments (28800 seconds in an 8 hour workday)
(define secs/workday.fte-fraction 28800)

;; Return mappings (as hash tables) from ids to names, for the data
;; returned from Forecast:
;;  - person-id => full name 
;;  - project-id => (list client-name project-name project-code)
;; 
;; schedule-id-name-mappings : schedule? -> (values (hash/c number? string?)
;;                                                  (hash/c number? string?))
(define (schedule-id-name-mappings whatnow-schedule)
  (match-let*
      ([(schedule people projects programmes assignments) whatnow-schedule]
       ;; client-id to client name
       ;; : (hash/c number? string?)
       [client-id=>name
        (for/hash ([p programmes]) (values (client-id p) (client-name p)))])

    (values
     ;; - person-id to full name
     ;; : (hash/c number? string?)
     (for/hash ([p people]
                #:when (or (member "REG Permanent" (person-roles p))
                           (member "REG FTC" (person-roles p))))
       (values (person-id p)
               (string-join (list (person-first-name p) (person-last-name p)))))     
     ;; - project-id to list of client name, project name and project code
     ;; : (hash/c number? (listof (or/c string? #f)))
     (for/hash ([p projects])
       (values (project-id p)
               (list (hash-ref client-id=>name (project-client-id p) #f)
                     (project-name p)
                     (findf project-code? (project-tags p))))))))

;; Given a (whatnow) assigment, and a month (as a gregor date --
;; nominally the first day in the month, but could by any date),
;; return the full-time equivalent fraction of the assignment in that
;; month.
;; 
;; assignment-fte-in-month : assignment? gregor:date? -> inexact-real?
(define (assignment-fte-in-month a m)
  (let (;; the input month, as an interval
        [m* (date-interval m (+months m 1))]
        ;; the assigment date range as an interval 
        [a* (date-interval (assignment-start-date a)
                           ;; Forecast assignments include their end date
                           (+days (assignment-end-date a) 1))])
    (exact->inexact
     (*
      ;; fractional allocation per workday
      (/ (assignment-allocation a)
         secs/workday.fte-fraction)
      ;; fraction of allocated workdays in the given month
      (/ (net-workdays (date-interval-intersect a* m*))
         (net-workdays m*))))))

;; schedule->allocations : schedule? (listof gregor:date?) -> (listof allocation?)
(define (schedule->allocations whatnow-schedule months)
  (let-values ([(person-id=>name project-id=>data)
                (schedule-id-name-mappings whatnow-schedule)]
               ;; groups of assignments with common person-id and project-id
               ;; : (listof (listof assignment?))
               [(assignment-groups)
                 (group-by (λ (a) (cons (assignment-person-id a)
                                        (assignment-project-id a)))
                           (schedule-assignments whatnow-schedule))])

    ;; Helpers to extract the common data for each group of assignments
    (define (group-person g)
      (hash-ref person-id=>name (assignment-person-id (car g)) #f))
    (define (group-project g)
      (hash-ref project-id=>data (assignment-project-id (car g)) #f))

    (for/list ([assgns assignment-groups]
               ;; Ignore 'missing' people and projects
               #:when (and (group-person assgns) (group-project assgns)))
      (match-let ([person (group-person assgns)]
                  [(list client-name project-name code) (group-project assgns)])
        (allocation
         client-name project-name (or code "") person
         (for/hash ([m months])
           (values m
                   (for/sum ([a assgns]) (assignment-fte-in-month a m)))))))))


;; ----------------------------------------
;; Output to a nocell grid program

;; allocations->grid : (listof allocation?) (listof gregor:date?) -> program?
(define (allocations->grid allocations dates)
  (program
   (list
    (sheet
     (list*
      ;; first row: column headings
      (list* (cell "Client" '(column-label))
             (cell "Project name" '(column-label))
             (cell "Finance code" '(column-label))
             (cell "Person" '(column-label))
             (map (λ (mon) (cell (~t mon "MMM yyyy") '(column-label))) dates))
      
      ;; other rows: allocation entries, with some exclusions
      (for/list ([a (sort allocations allocation<=?)]
                 #:unless
                 (or (member (allocation-client a) '("Corporate Duties"
                                                     "REG Service Areas"
                                                     "Trac days"
                                                     "Turing Programme Support"
                                                     "Turing Service Areas"
                                                     "UNAVAILABLE"))
                     (andmap zero? (hash-values (allocation-month-fraction a)))))

        (list* (cell (allocation-client a) '())
               (cell (allocation-project a) '())
               (cell (allocation-code a) '())
               (cell (allocation-person a) '())
               (map (λ (mon)
                      (cell (hash-ref (allocation-month-fraction a) mon 0.0)
                            '(output)))
                    dates))))))))

;; make-report-ods : (listof allocation?) (listof gregor:date?) -> bytes?
(define (make-report-ods allocations dates)
  (sxml->ods
   (grid-program->sxml (allocations->grid allocations dates)
                       #:blank-rows-before '(1)
                       #:blank-cols-before '(1))
   #:type 'fods))


;; ----------------------------------------
;; Examples

(bytes->file (make-report-ods example-allocations (fy-months 2020))
             "example-report-to-finance.fods")

;; (let ([months (fy-months 2020)]
;;       [forecast-schedule (get-the-schedule)])
;;   (bytes->file
;;    (make-report-ods (schedule->allocations forecast-schedule months) months)
;;    "example-report-to-finance.fods"))

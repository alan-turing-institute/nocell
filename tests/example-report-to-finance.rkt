#lang racket

;; A racket program that emits the team's monthly project-person
;; allocation report for Finance.

;; https://github.com/alan-turing-institute/nocell/issues/49

;; This example could eventually move into its own repository, with
;; nocell and whatnow as dependencies


(require (prefix-in gregor: gregor)
         "../grid/grid.rkt"
         "../ods/ods.rkt")

;; (require whatnow/forecast)

;; ----------------------------------------
;; Forecast monthly allocation data

;; Could use the data-frame library here (but avoiding the dependency
;; for now)

;; forecast allocation data
;;
;; Define with struct/contract to get checking within the module
;; (not worried about performance of this yet)

(struct/contract allocation-key
  ([client       string?]
   [project      string?]
   [code         string?]
   [person       string?])
  #:transparent)

(struct/contract allocation
  allocation-key
  ([month        gregor:date?]
   [fraction     real?])
  #:transparent)


(define example-forecast-records
  (list
   (allocation "Data science for science and the humanities"
               "Interesting Ocean"
               "R-XYZ-001"
               "Oliver Strickson"
               (gregor:date 2020 04)
               0.5)
   (allocation "Data science for science and the humanities"
               "Interesting Ocean"
               "R-XYZ-001"
               "Oliver Strickson"
               (gregor:date 2020 05)
               0.5)
   (allocation "Data-centric engineering"
               "Ubiquitous Mist"
               "R-ABC-999"
               "Callum Mole"
               (gregor:date 2020 04)
               0.5)
   (allocation "Data-centric engineering"
               "Ubiquitous Mist"
               "R-ABC-999"
               "James Geddes"
               (gregor:date 2020 05)
               0.5)))

;; Allocations as a 'wide' table
(struct/contract allocation*
  allocation-key
  ([month-fraction (hash/c gregor:date? real?)])
  #:transparent)


(define (key a) (struct-copy allocation-key a))

(define (allocations-tall->wide forecast-records)
  (for/list ([group (group-by key forecast-records)])
    (let ([common (key (car group))])
      (allocation* (allocation-key-client common)
                   (allocation-key-project common)
                   (allocation-key-code common)
                   (allocation-key-person common)
                   (make-hash
                    (map (λ (a) (cons (allocation-month a)
                                      (allocation-fraction a)))
                         group))))))

;; ----------------------------------------
;; Output to nocell grid

(define (allocations->grid forecast-records dates)
  (program
   (list
    (sheet
     (list*
      ;; first row: column headings
      (list* (cell "Client" '(column-label))
             (cell "Project name" '(column-label))
             (cell "Finance code" '(column-label))
             (cell "Person" '(column-label))
             (map (λ (mon) (cell (gregor:~t mon "MMM yyyy") '(column-label))) dates))
      
      ;; other rows: allocation entries
      (for/list ([a (allocations-tall->wide example-forecast-records)])
        (list* (cell (allocation-key-client a) '())
               (cell (allocation-key-project a) '())
               (cell (allocation-key-code a) '())
               (cell (allocation-key-person a) '())
               (map (λ (mon)
                      (cell (hash-ref (allocation*-month-fraction a) mon 0.0)
                            '(output)))
                    dates))))))))


(define (write-report forecast-records dates)
  (bytes->file
   (sxml->ods
    (grid-program->sxml (allocations->grid example-forecast-records dates)
                        #:blank-rows-before '(1)
                        #:blank-cols-before '(1))
    #:type 'fods)
   "example-report-to-finance.fods"))


;; A list of the months comprising the given financial year
;;
;; fy-months : exact-integer? -> (listof gregor:date?)
(define (fy-months y) (map (curry gregor:+months (gregor:date 2020 03)) (range 12)))

(write-report example-forecast-records (fy-months 2020))

#lang racket

;; A racket program that emits the team's monthly project-person
;; allocation report for Finance.

;; https://github.com/alan-turing-institute/nocell/issues/49

;; This example could eventually move into its own repository, with
;; nocell and whatnow as dependencies

(provide
 ;; write 'example-report-to-finance.ods' with data from example-forecast-records
 write-report-example)

(require "../grid/grid.rkt"
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
  ([month        string?] ; - Could be a date type: For now, these are YYYY-MM strings (e.g. 2020-04)
   [fraction     real?])
  #:transparent)


(define example-forecast-records
  (list
   (allocation "Data science for science and the humanities"
               "Interesting Ocean"
               "R-XYZ-001"
               "Oliver Strickson"
               "2020-04"
               0.5)
   (allocation "Data science for science and the humanities"
               "Interesting Ocean"
               "R-XYZ-001"
               "Oliver Strickson"
               "2020-05"
               0.5)
   (allocation "Data-centric engineering"
               "Ubiquitous Mist"
               "R-ABC-999"
               "Callum Mole"
               "2020-04"
               0.5)
   (allocation "Data-centric engineering"
               "Ubiquitous Mist"
               "R-ABC-999"
               "James Geddes"
               "2020-05"
               0.5)))

;; Allocations as a 'wide' table
(struct/contract allocation*
  allocation-key
  ([month-fraction (hash/c string? real?)])
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
             (map (λ (mon) (cell mon '(column-label))) dates))
      
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


(define (write-report-example)
  (define dates '("2020-03" "2020-04" "2020-05" "2020-06"))
  (bytes->file
   (sxml->ods
    (grid-program->sxml (allocations->grid example-forecast-records dates)
                        #:blank-rows-before '(1)
                        #:blank-cols-before '(1))
    #:type 'fods)
   "example-report-to-finance.fods"))

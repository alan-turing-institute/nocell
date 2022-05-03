#lang racket

;; A racket spreadsheet that emits the team's monthly project-person
;; allocation report for Finance.

;; https://github.com/alan-turing-institute/nocell/issues/49

;; This example could eventually move into its own repository, with
;; nocell and whatnow as dependencies


(require (rename-in gregor
                    [date gregor:date]
                    [date? gregor:date?])
         whatnow/forecast
         whatnow/schedule
         racket/hash
         rackunit
         "../sheet/sheet.rkt"
         "../sheet/ods/ods.rkt"
         "finance-utils.rkt")


;; (bytes->file (make-report-ods example-allocations (fy-months 2020))
;;              "example-report-to-finance.fods")

(let* ([months (map (curry +months (gregor:date 2021 04)) (range 12))]

       [allocations
        (for/fold ([allocations null])
                  ([month (in-list months)])
          (let* ([iso-month-end (-days (iso-week-start (+months month 1)) 1)]
                 [forecast-schedule (get-the-schedule (iso-week-start month) iso-month-end)])
            (append allocations
                    (schedule->allocations forecast-schedule (list month)))))]

       [allocations-grouped
        (group-by (λ (a) (list (allocation-client a)
                               (allocation-person a)
                               (allocation-project a)))
                  allocations)]

       [allocations-combined
        (map (λ (as)
               (struct-copy monthly-allocation
                            (car as)
                            [month-fraction
                             (apply hash-union (map monthly-allocation-month-fraction as))]))
             allocations-grouped)])

  (bytes->file (make-report-ods allocations-combined months)
               "example-report-to-finance.ods"))

#lang racket/base

(require "./grid-examples.rkt"
         "../ods/ods.rkt")


(let ([multi-table-sxml (grid-program->sxml multiplication-table)])
  (sxml->ods multi-table-sxml #:filename "multi-flat" #:type "fods")
  (sxml->ods multi-table-sxml #:filename "multi-extended" #:type "ods"))


(let ([bubbly-sxml (grid-program->sxml bubbly)])
  (sxml->ods bubbly-sxml #:filename "bubbly-flat" #:type "flat")
  (sxml->ods bubbly-sxml #:filename "bubbly-extended" #:type "extended"))
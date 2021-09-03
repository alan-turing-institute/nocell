#lang racket/base

(require "./sheet-examples.rkt"
         "../sheet/ascii/ascii.rkt")

(let ([multi-table (sheet-spreadsheet->ascii-spreadsheet multiplication-table)])
  (display (ascii-spreadsheet->string multi-table)))

(let ([bubbly (sheet-spreadsheet->ascii-spreadsheet bubbly)])
  (display (ascii-spreadsheet->string bubbly)))
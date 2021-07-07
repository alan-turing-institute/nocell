#lang info
(define collection "nocell")
(define deps '("base"
               "rackunit-lib"
               "sxml"
               "gregor"
               "https://github.com/alan-turing-institute/whatnow.git#62055d008faea5fe3c5779804b986c6a3d97873d"
               "basedir" ; for whatnow
               ))
(define pkg-desc "A language for building probabilistic spreadsheets")
(define version "0.1")
(define pkg-authors '(callummole ots22 tamc triangle-man))
(define scribblings '(("scribblings/docs.scrbl" (multi-page))))

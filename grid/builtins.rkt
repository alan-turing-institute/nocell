#lang racket/base

#| 

Definition of built in functions

|#

(require racket/contract)

(provide
 (contract-out
  (builtin? (-> any/c boolean?))))


(define (builtin? v)
  (or/c 'pi 'e 'neg 'abs 'sgn 'inv 'round 'floor 'ceiling 'truncate 'exp 'ln 'log10 'not '+ '- '* '/
        'quotient 'remainder 'modulo '= '!= '< '< '<= '>= 'or 'and 'if 'fold/+ 'fold/* 'fold/and 'fold/or))

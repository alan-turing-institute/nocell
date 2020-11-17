3298
((3) 0 () 14 ((q lib "nocell/grid/grid.rkt") (q 451 . 5) (q 733 . 6) (q 181 . 4) (q 901 . 4) (q 256 . 4) (q 1570 . 5) (q 1304 . 5) (q 1157 . 3) (q 1210 . 4) (q 1473 . 4) (q 0 . 4) (q 89 . 4) (q 1421 . 3)) () (h ! (equal) ((c def c (c (? . 0) q application-args)) c (? . 1)) ((c def c (c (? . 0) q labelled-cell?)) c (? . 5)) ((c def c (c (? . 0) q matrix-rows)) c (? . 4)) ((c def c (c (? . 0) q relative-location-target)) c (? . 6)) ((c def c (c (? . 0) q range-reference?)) c (? . 7)) ((c def c (c (? . 0) q date-month)) c (? . 2)) ((c def c (c (? . 0) q error?)) q (681 . 3)) ((c def c (c (? . 0) q struct:application)) c (? . 1)) ((c def c (c (? . 0) q sheet)) c (? . 12)) ((c def c (c (? . 0) q absolute-location-label)) c (? . 10)) ((c def c (c (? . 0) q matrix-ref)) q (1006 . 5)) ((c def c (c (? . 0) q date-day)) c (? . 2)) ((c def c (c (? . 0) q cell)) c (? . 3)) ((c def c (c (? . 0) q struct:reference)) c (? . 8)) ((c def c (c (? . 0) q application-fn)) c (? . 1)) ((c def c (c (? . 0) q cell-xpr)) c (? . 3)) ((c def c (c (? . 0) q struct:cell-reference)) c (? . 9)) ((c def c (c (? . 0) q struct:date)) c (? . 2)) ((c def c (c (? . 0) q matrix?)) c (? . 4)) ((c def c (c (? . 0) q struct:labelled-cell)) c (? . 5)) ((c def c (c (? . 0) q relative-location?)) c (? . 6)) ((c def c (c (? . 0) q struct:location)) c (? . 13)) ((c def c (c (? . 0) q struct:range-reference)) c (? . 7)) ((c def c (c (? . 0) q date)) c (? . 2)) ((c def c (c (? . 0) q struct:cell)) c (? . 3)) ((c def c (c (? . 0) q expression?)) q (342 . 3)) ((c def c (c (? . 0) q reference)) c (? . 8)) ((c def c (c (? . 0) q application)) c (? . 1)) ((c def c (c (? . 0) q cell-reference)) c (? . 9)) ((c def c (c (? . 0) q struct:absolute-location)) c (? . 10)) ((c def c (c (? . 0) q nothing?)) q (627 . 3)) ((c def c (c (? . 0) q labelled-cell-lbl)) c (? . 5)) ((c def c (c (? . 0) q cell?)) c (? . 3)) ((c def c (c (? . 0) q program-sheets)) c (? . 11)) ((c def c (c (? . 0) q range-reference-tl)) c (? . 7)) ((c def c (c (? . 0) q relative-location)) c (? . 6)) ((c def c (c (? . 0) q cell-reference?)) c (? . 9)) ((c def c (c (? . 0) q program)) c (? . 11)) ((c def c (c (? . 0) q struct:relative-location)) c (? . 6)) ((c def c (c (? . 0) q reference?)) c (? . 8)) ((c def c (c (? . 0) q labelled-cell)) c (? . 5)) ((c def c (c (? . 0) q struct:program)) c (? . 11)) ((c def c (c (? . 0) q sheet?)) c (? . 12)) ((c def c (c (? . 0) q date-year)) c (? . 2)) ((c def c (c (? . 0) q relative-location-source)) c (? . 6)) ((c def c (c (? . 0) q range-reference-br)) c (? . 7)) ((c def c (c (? . 0) q program?)) c (? . 11)) ((c def c (c (? . 0) q location)) c (? . 13)) ((c def c (c (? . 0) q sheet-rows)) c (? . 12)) ((c def c (c (? . 0) q struct:matrix)) c (? . 4)) ((c def c (c (? . 0) q application?)) c (? . 1)) ((c def c (c (? . 0) q matrix)) c (? . 4)) ((c def c (c (? . 0) q struct:sheet)) c (? . 12)) ((c def c (c (? . 0) q range-reference)) c (? . 7)) ((c def c (c (? . 0) q atomic-value?)) q (568 . 3)) ((c def c (c (? . 0) q absolute-location)) c (? . 10)) ((c def c (c (? . 0) q date?)) c (? . 2)) ((c def c (c (? . 0) q cell-reference-loc)) c (? . 9)) ((c def c (c (? . 0) q absolute-location?)) c (? . 10)) ((c def c (c (? . 0) q location?)) c (? . 13)) ((c def c (c (? . 0) q value?)) q (399 . 3))))
struct
(struct program (sheets)
    #:transparent)
  sheets : (listof sheet?)
struct
(struct sheet (rows)
    #:transparent)
  rows : (listof (listof cell?))
struct
(struct cell (xpr)
    #:transparent)
  xpr : expression?
struct
(struct labelled-cell cell (lbl)
    #:transparent)
  lbl : string?
procedure
(expression? v) -> boolean?
  v : any/c
procedure
(value? v) -> boolean?
  v : any/c
struct
(struct application (fn args)
    #:transparent)
  fn : builtin?
  args : (listof expression?)
procedure
(atomic-value? v) -> boolean?
  v : any/c
procedure
(nothing? v) -> boolean?
  v : any/c
procedure
(error? v) -> boolean?
  v : any/c
struct
(struct date (year month day)
    #:transparent)
  year : exact-nonnegative-integer?
  month : (integer-in 1 12)
  day : (integer-in 1 31)
struct
(struct matrix (rows)
    #:transparent)
  rows : (vectorof (vectorof atomic-value?))
procedure
(matrix-ref m row col) -> atomic-value?
  m : matrix?
  row : exact-nonnegative-integer?
  col : exact-nonnegative-integer?
struct
(struct reference ()
    #:transparent)
struct
(struct cell-reference reference (loc)
    #:transparent)
  loc : location?
struct
(struct range-reference reference (tl br)
    #:transparent)
  tl : location?
  br : location?
struct
(struct location ()
    #:transparent)
struct
(struct absolute-location location (label)
    #:transparent)
  label : label?
struct
(struct relative-location location (source target)
    #:transparent)
  source : label?
  target : label?

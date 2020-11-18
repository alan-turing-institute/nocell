4022
((3) 0 () 17 ((q lib "nocell/grid/grid.rkt") (q lib "nocell/grid/builtins.rkt") (q 1826 . 5) (q 451 . 5) (q 789 . 6) (q 181 . 4) (q 957 . 4) (q 256 . 4) (q 1627 . 5) (q 1360 . 5) (q 1213 . 3) (q 1266 . 4) (q 1529 . 4) (q 0 . 4) (q 89 . 4) (q lib "nocell/ods/ods.rkt") (q 1477 . 3)) () (h ! (equal) ((c def c (c (? . 15) q grid-program->sxml)) q (2268 . 3)) ((c def c (c (? . 1) q builtins)) q (1757 . 2)) ((c def c (c (? . 0) q application-args)) c (? . 3)) ((c def c (c (? . 0) q labelled-cell?)) c (? . 7)) ((c def c (c (? . 0) q matrix-rows)) c (? . 6)) ((c def c (c (? . 0) q relative-location-target)) c (? . 8)) ((c def c (c (? . 0) q range-reference?)) c (? . 9)) ((c def c (c (? . 1) q builtin-type-arity)) c (? . 2)) ((c def c (c (? . 0) q date-month)) c (? . 4)) ((c def c (c (? . 1) q builtin-type-arg-types)) c (? . 2)) ((c def c (c (? . 0) q error?)) q (737 . 3)) ((c def c (c (? . 1) q builtin-type)) c (? . 2)) ((c def c (c (? . 0) q struct:application)) c (? . 3)) ((c def c (c (? . 0) q sheet)) c (? . 14)) ((c def c (c (? . 0) q absolute-location-label)) c (? . 12)) ((c def c (c (? . 0) q matrix-ref)) q (1062 . 5)) ((c def c (c (? . 0) q date-day)) c (? . 4)) ((c def c (c (? . 0) q cell)) c (? . 5)) ((c def c (c (? . 0) q struct:reference)) c (? . 10)) ((c def c (c (? . 0) q application-fn)) c (? . 3)) ((c def c (c (? . 0) q cell-xpr)) c (? . 5)) ((c def c (c (? . 0) q struct:cell-reference)) c (? . 11)) ((c def c (c (? . 0) q struct:date)) c (? . 4)) ((c def c (c (? . 0) q matrix?)) c (? . 6)) ((c def c (c (? . 0) q struct:labelled-cell)) c (? . 7)) ((c def c (c (? . 0) q relative-location?)) c (? . 8)) ((c def c (c (? . 0) q struct:location)) c (? . 16)) ((c def c (c (? . 1) q struct:builtin-type)) c (? . 2)) ((c def c (c (? . 0) q struct:range-reference)) c (? . 9)) ((c def c (c (? . 0) q date)) c (? . 4)) ((c def c (c (? . 0) q struct:cell)) c (? . 5)) ((c def c (c (? . 1) q builtin-type-ret-type)) c (? . 2)) ((c def c (c (? . 0) q expression?)) q (342 . 3)) ((c def c (c (? . 0) q reference)) c (? . 10)) ((c def c (c (? . 0) q application)) c (? . 3)) ((c def c (c (? . 0) q cell-reference)) c (? . 11)) ((c def c (c (? . 0) q struct:absolute-location)) c (? . 12)) ((c def c (c (? . 0) q nothing?)) q (683 . 3)) ((c def c (c (? . 0) q labelled-cell-lbl)) c (? . 7)) ((c def c (c (? . 0) q cell?)) c (? . 5)) ((c def c (c (? . 0) q program-sheets)) c (? . 13)) ((c def c (c (? . 0) q range-reference-tl)) c (? . 9)) ((c def c (c (? . 0) q relative-location)) c (? . 8)) ((c def c (c (? . 0) q cell-reference?)) c (? . 11)) ((c def c (c (? . 0) q program)) c (? . 13)) ((c def c (c (? . 0) q struct:relative-location)) c (? . 8)) ((c def c (c (? . 0) q builtin?)) q (568 . 3)) ((c def c (c (? . 0) q labelled-cell)) c (? . 7)) ((c def c (c (? . 0) q struct:program)) c (? . 13)) ((c def c (c (? . 0) q sheet?)) c (? . 14)) ((c def c (c (? . 0) q date-year)) c (? . 4)) ((c def c (c (? . 0) q relative-location-source)) c (? . 8)) ((c def c (c (? . 0) q range-reference-br)) c (? . 9)) ((c def c (c (? . 0) q program?)) c (? . 13)) ((c def c (c (? . 0) q reference?)) c (? . 10)) ((c def c (c (? . 0) q sheet-rows)) c (? . 14)) ((c def c (c (? . 0) q struct:matrix)) c (? . 6)) ((c def c (c (? . 0) q application?)) c (? . 3)) ((c def c (c (? . 15) q bytes->file)) q (2352 . 4)) ((c def c (c (? . 0) q matrix)) c (? . 6)) ((c def c (c (? . 0) q struct:sheet)) c (? . 14)) ((c def c (c (? . 0) q location)) c (? . 16)) ((c def c (c (? . 0) q range-reference)) c (? . 9)) ((c def c (c (? . 0) q atomic-value?)) q (624 . 3)) ((c def c (c (? . 1) q builtin-arg-type?)) q (2043 . 3)) ((c def c (c (? . 0) q absolute-location)) c (? . 12)) ((c def c (c (? . 1) q builtin-type?)) c (? . 2)) ((c def c (c (? . 0) q date?)) c (? . 4)) ((c def c (c (? . 0) q cell-reference-loc)) c (? . 11)) ((c def c (c (? . 15) q sxml->ods)) q (2106 . 4)) ((c def c (c (? . 0) q absolute-location?)) c (? . 12)) ((c def c (c (? . 0) q location?)) c (? . 16)) ((c def c (c (? . 0) q value?)) q (399 . 3))))
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
(builtin? v) -> boolean?
  v : symbol?
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
  label : string?
struct
(struct relative-location location (source target)
    #:transparent)
  source : string?
  target : string?
value
builtins : (hash/c symbol? builtin-type? #:immutable #t)
struct
(struct builtin-type (arity arg-types ret-type))
  arity : (or/c #f exact-nonnegative-integer?)
  arg-types : (or/c builtin-arg-type? (listof builtin-arg-type?))
  ret-type : builtin-arg-type?
procedure
(builtin-arg-type? v) -> boolean?
  v : any/c
procedure
(sxml->ods sxml-program #:type type) -> bytes?
  sxml-program : sxml-program?
  type : (or/c "f" "fods" "flat" "e" "extended" "ods")
procedure
(grid-program->sxml program) -> sxml-program?
  program : program?
procedure
(bytes->file bstr fn) -> exact-nonnegative-integer?
  bstr : bytes?
  fn : string?

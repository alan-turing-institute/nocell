#lang scribble/manual

@require[scribble/bnf]
@require[scribble/core
         scribble/decode]
@require[
  @for-label[@except-in[racket/base date? date date-month date-day date-year struct:date]]
  @for-label["../grid/grid.rkt"]]

@title{The Grid Language}
@author["James Geddes" "Oliver Strickson" "Callum Mole"]

@section{Structure}

@defmodule[nocell/grid]{The @racketmodname[grid/grid.rkt] library
defines the Grid language.}

Grid is a language for describing spreadsheets, although to call it a
``language'' is perhaps overblown. It is perhaps best thought of as ``assembly
language''for spreadsheets. A Grid program may be straightforwardly converted
into a specific spreadsheet application, such as the Open Document Format for
Office Applications (ODF).

The following is a conceptual definition of the structure of a Grid
program. Certain non-terminals are still undefined; these have an ellipsis in
place of a definition and are omitted in the structure definitions.

@(define (term name)
   (make-element 'bold name))

@(define (BNF-struct name . fields)
   (make-element #f (append (list (make-element 'roman "("))
                            (list (apply BNF-seq
                                         (cons name
                                               (decode-content fields))))
                            (list (make-element 'roman ")")))))

@; --- pseq makes a parenthesised sequence. 
@(define (BNF-pseq . fields)
   (make-element #f (append (list (make-element 'roman "("))
                            (list (apply BNF-seq (decode-content fields)))
                            (list (make-element 'roman ")")))))
@; --- an alternation of litchar 
@(define (BNF-litalt . fields)
   (apply BNF-alt (map litchar (decode-content fields))))

@BNF[(list @nonterm{program}
           @BNF-seq[@nonterm{grid-meta}
                    @kleeneplus[@nonterm{sheet}]])
     (list @nonterm{sheet}
           @BNF-seq[@nonterm{sheet-meta}
                    @optional[@nonterm{row-styles}]
                    @optional[@nonterm{column-styles}]
                    @kleenestar[@nonterm{row}]])
     (list @nonterm{row}
           @kleenestar[@nonterm{cell}])
     (list @nonterm{cell}
           @BNF-seq[@nonterm{expression} @optional[@nonterm{cell-style}] @optional[@nonterm{label}]])
     (list @nonterm{expression}
           @nonterm{value}
           @nonterm{application})
     (list @nonterm{value}
           @nonterm{atomic-value}
           @nonterm{matrix}
           @nonterm{reference})
     (list @nonterm{atomic-value} @BNF-alt[@litchar{numeric?}
                                           @litchar{string?}
                                           @litchar{boolean?}
                                           @nonterm{error}
                                           @litchar{nothing}])
     (list @nonterm{application}
           @BNF-seq[@nonterm{builtin} @kleenestar[@nonterm{expression}]])
     (list @nonterm{label} @litchar{string?})                           
     (list @nonterm{grid-meta}
           @BNF-etc)
     (list @nonterm{sheet-meta}
           @BNF-etc)
     (list @nonterm{row-styles}
           @BNF-etc)
     (list @nonterm{column-styles}
           @BNF-etc)
     (list @nonterm{cell-style}
           @BNF-etc)
     (list @nonterm{matrix}
           @kleeneplus[@nonterm{matrix-row}])
     (list @nonterm{matrix-row}
           @kleeneplus[@nonterm{atomic-value}])
     (list @nonterm{reference}
           @nonterm{single-cell-ref}
           @BNF-seq[@nonterm{single-cell-ref} @nonterm{single-cell-ref}])
     (list @nonterm{single-cell-ref}
           @nonterm{label}
           @BNF-seq[@nonterm{label} @nonterm{label}])
     (list @nonterm{error}  
           @BNF-alt[@litchar{error:arg} @litchar{error:undef} @litchar{error:val}])
     (list @nonterm{builtin}
           @BNF-alt[@nonterm{builtin/0} @nonterm{builtin/1} @nonterm{builtin/2} @nonterm{builtin/3}]
           @nonterm{builtin/*})
     (list @nonterm{builtin/0}
           @BNF-litalt["pi" "e"])
     (list @nonterm{builtin/1}
           @BNF-litalt["neg" "abs" "sgn"]
           @litchar{inv}
           @BNF-litalt["round" "floor" "ceiling" "truncate"]
           @BNF-litalt["exp" "ln" "log10"]
           @litchar{not})
     (list @nonterm{builtin/2}
           @BNF-litalt["+" "-" "*" "/"]
           @BNF-litalt["quotient" "remainder" "modulo"]
           @BNF-litalt["=" "!=" "<" "<=" ">" ">="]
           @BNF-litalt["or" "and"])
     (list @nonterm{builtin/3}
           @litchar{if})
     (list @nonterm{builtin/*}
           @BNF-litalt["fold/+" "fold/*"]
           @BNF-litalt["fold/and" "fold/or"])
           
]

@subsection{Sheets and Cells}

@deftogether[(@defstruct*[program ([sheets (listof sheet?)]) #:transparent]
              @defstruct*[sheet ([rows (listof (listof cell?))]) #:transparent]
              @defstruct*[cell ([xpr expression?]) #:transparent]
              @defstruct*[(labelled-cell cell) ([lbl string?]) #:transparent])]{

    A `spreadsheet,' such as an Excel document, is a list of worksheets, each of
    which is a two-dimensional array of cells. In Grid, a worksheet is called a
    @racket[sheet] and is represented as a list of rows, each of which is a list
    of cells. A labelled cell is just like a cell, except that it carries a
    label.}

Note: The `list of list of cell' structure is the same as the structure of the
XML description of a spreadsheet in ODF.

Spreadsheets carry some metadata along with them: author, date, and so on. We
haven't figured this out yet.

@subsection{Expressions and Values}

@deftogether[(@defproc[(expression? [v any/c]) boolean?]
              @defproc[(value? [v any/c]) boolean?]
              @defstruct*[application ([fn builtin?] [args (listof expression?)])
                          #:transparent])]{

   An @racket[expression] is the most general content of a cell and includes
   both values and applications of built-in functions.}

In most spreadsheet programs, such as Excel, the content of a cell may be either
the literal form of a value or a formula representing a computation. Formulas
are distinguished from values by starting with `@tt{=}.'

In Grid, both values and formulas are represented by @deftech{expressions}. An
expression is either a @tech{value} or an @tech{application}. Applications
themselves can contain expressions as arguments (but since this is a first-order
language, the function is not an expression).

@subsection{Values: Atomic values, Matrices, and References}

@deftogether[(@defproc[(atomic-value? [v any/c]) boolean?]
              @defproc[(nothing? [v any/c]) boolean?]
              @defproc[(error? [v any/c]) boolean?]
              @defstruct*[date ([year exact-nonnegative-integer?]
                                [month (integer-in 1 12)]
                                [day (integer-in 1 31)]) #:transparent])]{

              An atomic value is the `final' value of a cell.}

@deftogether[(@defstruct*[matrix ([rows (vectorof (vectorof atomic-value?))])
              #:transparent]
              @defproc[(matrix-ref [m matrix?]
                                   [row exact-nonnegative-integer?]
                                   [col exact-nonnegative-integer?]) atomic-value?])]{

              Matrices are an intermediate value.}

@deftogether[(@defstruct*[reference () #:transparent]
              @defstruct*[(cell-reference reference) ([loc location?])
              #:transparent]
              @defstruct*[(range-reference reference)
                          ([tl location?] [br location?]) #:transparent]
              @defstruct*[location () #:transparent]
              @defstruct*[(absolute-location location) ([label label?])
              #:transparent]
              @defstruct*[(relative-location location) ([source label?] [target
              label?])
              #:transparent])]{

              References refer to other cells.}

An @deftech{atomic value} is a number, a string, a boolean, a date, an error, or
`nothing.' A date is simply a year, a month, and a day. No attempt is made to
ensure that a particular day of the month exists. The special value
@racket['nothing] represents an empty cell. Any cells which exist in the final
spreadsheet but which were not specified in the Grid programme (for example,
cells to the right of or below those specified) implicitly contain the value
@racket['nothing].

We also allow `errors' as atomic values. At the moment we have three kinds of
error: one to indicate that an argument to a built-in function was the wrong
type; one to indicate that the function could not be evaluated on the argument
(eg, division by zero); and one to indicate a `missing' value.

@margin-note{@bold{TODO}: This description of errors is not very precise about
what the errors mean; presumably it will be more precise once we start thinking
about built-in functions.}

A @deftech{matrix} is a two-dimensional array of @tech{atomic values}. A
@tech{reference} is a pointer to another cell or to a range of cells. Both
matrices and references can be generated by, and consumed by, certain built-in
functions, as well as having literal forms.

@margin-note{In Excel, one can write @tech{matrices} and @tech{references} as
literals in formulas. For example, @tt{={1, 2, 3; 3, 4 5}} is a matrix of two
rows by three columns and the function @tt{SUM()} will take a matrix as an
argument. The formula @tt{=OFFSET(A1, 0, 0)} includes the literal reference
@tt{A1} and the function @tt{OFFSET()} itself produces a reference as its
value.}

An @deftech{application} represents the application of a built-in function to
arguments.

To @deftech{evaluate} an expression means to reduce it to an atomic value. Since
errors are atomic values and there are no unbounded loops in Grid (at least, we
assume that ``circular references'' are forbidden), the evaluation of an
expression always terminates. The atomic value of a matrix is its top-left
element, and the value of a reference is the referent.

@margin-note{The behaviour of Excel when the result of a formula is an array
value has changed in recent versions. Previously, a cell whose contents
evaluated to an array would appear to contain just the top-left element of the
array. The new behaviour is that the elements of the array `spill out' of the
cell into adjacent cells; if these cells are not empty then a @tt{#SPILL!}
error is reported in the cell. This change is not merely a presentational
change: a reference to a cell into which a value was spilled will now give the
spilled value whereas, previously, that cell would have been treated as empty.}

A @deftech{reference} refers to the value of another cell or a range of cells. A
@deftech{location} identifies the particular cell in question. Grid has two
methods of locating a cell: either by reference to directly to a labelled cell,
by means of its label (known as an @deftech{absolute location}); or by an
indirect reference, as an offset from the current cell (known as a
@deftech{relative location}). To refer indirectly to a cell requires two labels;
the offset applied to the current cell is the offset between the two labelled
cells.

To refer to a single cell requires a single location; to refer to a range of
cell reqires two locations: the top-left and bottom-right cells.

@margin-note{A @emph{named range} is a symbol which stands for a range. For now,
Grid does not support named ranges.}


@section{Built-in Functions}





@section{Styles}

Worksheets and cells carry style information (which, again, we haven't yet
figured out). The style information for worksheets refers to columns and rows
(for example, to say that certain columns or rows should be the same size, or
are `filler' columns or rows).

The style information for cells will be descriptive rather than
presentational. That is, it will not specify colours, or fonts, or other visual
effects; instead it will specify the kind of cell. For example, some cells are
`summaries' of the cells above them, in which case the system might choose to
insert a horizontal rule.



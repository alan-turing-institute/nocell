#lang scribble/manual

@require[scribble/bnf]
@require[scribble/core
         scribble/decode]

@title{The Grid Language (work in progress)}
@author["James Geddes" "Oliver Strickson" "Callum Mole"]

@centered{@bold{The following material is work in progress}}

Grid is a language for describing spreadsheets, although to call it a ``language''
is perhaps overblown. It is @emph{declarative}, in the sense that it merely
describes the structure and content of a spreadsheet. A grid program may be
straightforwardly converted into a specific spreadsheet application, such as the
Open Document Format for Office Applications (ODF).

The following is the definition of the `abstract syntax tree' of Grid. It is not
an external representation nor a data structure to be used as an internal
representation. However, it would be reasonably straightforward to convert it to
a data structure by replacing the non-terminals with structures.

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

@BNF[(list @nonterm{grid-program}
           @BNF-seq[@nonterm{grid-meta}
                    @kleeneplus[@nonterm{sheet}]])
     (list @nonterm{sheet}
           @BNF-seq[@nonterm{sheet-meta}
                    @optional[@nonterm{row-styles}]
                    @optional[@nonterm{column-styles}]
                    @kleeneplus[@nonterm{row}]])
     (list @nonterm{row}
           @kleeneplus[@nonterm{cell}])
     (list @nonterm{cell}
           @BNF-seq[@nonterm{cell-content} @optional[@nonterm{cell-style}] @optional[@nonterm{label}]])
     (list @nonterm{cell-content}
           @litchar{empty}
           @nonterm{expression})
     (list @nonterm{expression}
           @nonterm{value}
           @nonterm{application})
     (list @nonterm{value}
           @nonterm{atomic-value}
           @nonterm{array-value}
           @nonterm{reference})
     (list @nonterm{atomic-value} @BNF-alt[@litchar{numeric?}
                                           @litchar{string?}
                                           @litchar{boolean?}
                                           @nonterm{error}])
     (list @nonterm{application}
           @BNF-seq[@nonterm{built-in} @kleeneplus[@nonterm{expression}]])
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
     (list @nonterm{array-value}
           @BNF-etc)
     (list @nonterm{reference}
           @nonterm{single-cell-reference}
           @BNF-seq[@nonterm{single-cell-reference} @nonterm{single-cell-reference}])
     (list @nonterm{single-cell-reference}
           @nonterm{label}
           @BNF-seq[@nonterm{label} @nonterm{label}])
     (list @nonterm{error}  
           @BNF-alt[@litchar{error:arg} @litchar{error:undef} @litchar{error:val}])
     (list @nonterm{built-in}
           @BNF-alt[@nonterm{unary-built-in} @nonterm{binary-built-in} @nonterm{ternary-built-in} @nonterm{range-built-in}])
           
]

@section{Structure}

@subsection{Sheets}

A `spreadsheet,' such as an Excel spreadsheet, is a list of worksheets, each of
which is a two-dimensional array of cells. In Grid, a worksheet, or
@nonterm{sheet}, is represented as a list of rows, each of which is a list of
cells. (This is the same structure as the XML application underlying ODF.)

Spreadsheets carry some metadata along with them: author, date, and so on. We
haven't figured this out yet.


@subsection{Values and Expressions}

In most spreadsheet programs, such as Excel, the content of a cell may be either
the literal form of a value or a formula representing a computation. Formulas
are distinguishable from values because they start with `@tt{=}.'

In Grid, both values and formulas are represented by @deftech{expressions}. An
expression is either: an @tech{atomic value}; a @tech{non--atomic value} (see
below); or the @tech{application} of a built-in function to a set of
arguments. When a Grid program is translated to a spreadsheet the expression is
converted to a value or a formula as appropriate.

An @deftech{atomic value} is a number, a string, a boolean, or an error. (It may
be worthwhile thinking about adding other types -- for example, dates.)

A @deftech{non--atomic value} is either an array value or a reference. An
@deftech{array value} is a two-dimensional matrix of @tech{atomic values}. A
@tech{reference} is a pointer to another cell or to a range of cells. Both
array values and references can be generated by, and consumed by, certain
built-in functions, as well as having literal forms.

@margin-note{@bold{Excel}: In Excel, one can write array values and references
as literals in formulas. For example, @tt{={1, 2, 3; 3, 4 5}} is an array of two
rows and three columns and the function @tt{SUM()} will take an array as an
argument. The formula @tt{=OFFSET(A1, 0, 0)} includes the literal reference
@tt{A1} and the function @tt{OFFSET()} itself produces a reference as its
value.}

An @deftech{application} represents the application of a built-in function to
arguments.

To evaluate an expression means to reduce it to a value (atomic or
non-atomic). Since errors are values and there are no unbounded loops in Grid
(at least, we assume that ``circular references'' are forbidden), the evaluation
of an expression always terminates. To evaluate a @emph{cell} means to reduce
any value to an atomic value. The atomic value of an array value is its top-left
element, and the atomic value of a reference is the referent.

@margin-note{@bold{Excel}: The behaviour of Excel when the result of a formula
is an array value has changed in recent versions. Previously, a cell whose
contents evaluated to an array would appear to contain just the top-left element
of the array. The new behaviour is that the elements of the array `spill out' of
the cell into adjacent cells; if these cells are not empty then a @tt{#SPILL!}
error is reported in the cell. This change is not merely a presentational
change: a reference to a cell into which a value was spilled will now give the
spilled value whereas, previously, that cell would have been treated as empty.}

@subsection{References}

A @deftech{range} of cells is a rectangular set of contiguous cells (possibly
just a single cell). @margin-note{@bold{Excel}: In Excel, a range is allowed to
be the union of rectangular ranges.}

A @deftech{reference} is a value that denotes a range. A @deftech{label} is a
unique identifier associated with a cell (unique over all cells). Grid uses
labels to refer to single cells; it uses two labels to refer to a range of
cells: the two labels are the top-left abd bottom-right cell respectively.

A @deftech{named range} is a symbol which stands for a range. For now, Grid does
not support named ranges.






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



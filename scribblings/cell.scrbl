#lang scribble/manual

@title{The Cell Language}

@section{Motivation}

Cell is intented to be a sort of `intermediate' or `core' language for abstract
spreadsheets. Real spreadsheets are two-dimensional arrangements of expressions;
where the expressions consist solely of constants and built-in, first-order
functions. The Grid language is simply a representation of these, suitable for
direct transformation into a specific spreadsheet program. As such, Grid is
intended to be a `back-end,' so that the programmer does not have to think about
the specific output format being targetted, but it is not a convenient language.

By contrast, Cell is intended to be a language for describing `spreadsheets' of
arbitrary dimension and having higher-order functions.

What would such a language be? The obvious place to look for inspiration is the
`array languages,' most notably APL, its successor J, and various ideas which
these inspired. An array language has arrays as its primitive datatype and this
seems to align well with the idea of a spreadsheet as being `a rectangular
grid.'

Furthermore, the higher-order functions in array languages are naturally those
which express `structured recursion' over arrays. For example @tt{fold} (or
@tt{reduce}) and @tt{map} are natural ways of applying primitive functions to
arrays. Notably, these functions implement restricted forms of recursion which
are guaranteed to terminate.

Recent work that looks particularly relevant is that by Slepak @italic{et al.} on
`Remora' @cite["remora" "remora2" "remora3"] which describes a slightly more
general version of an abstract array language, as well as follow-on work by
Gibbons @cite["gibbons2017"].

However, compared to these languages, we have an additional challenge, which is
that the `values' of Cell are @italic{not} arrays. Instead, the value of a Cell
program contains first-order functions and indeed general expressions consisting
of these. Cell is more like a language for writing first-order, array-based
programs.



@bibliography[
  @bib-entry[#:key "remora"
             #:title "An Array-Oriented Language with Static Rank Polymorphism"
             #:author "Justin Slepak, Olin Shivers, and Panagiotis Manolios"
             #:location "in Programming Languages and Systems (pp. 27--46)"
             #:date "2014"
             #:url "http://www.ccs.neu.edu/home/pete/pub/esop14-full.pdf"]
  @bib-entry[#:key "remora2"
             #:title "Records with Rank Polymorphism"
             #:author "Justin Slepak, Olin Shivers, and Panagiotis Manolios"
             #:location "in ARRAY 2019"
             #:date "2019"
             #:url "https://www.ccs.neu.edu/~jrslepak/array19.pdf"]
  @bib-entry[#:key "remora3"
             #:title "An Introduction to Rank-polymorphic Programming in Remora (Draft)"
             #:author "Olin Shivers, Justin Slepak, and Panagiotis Manolios"
             #:date "2020"
             #:location "arXiv:1912.13451v2 [cs.PL]"
             #:url "https://arxiv.org/abs/1912.13451"]
   @bib-entry[#:key "gibbons2017"
              #:title "APLicative Programming with Naperian Functors"
              #:author "Jeremy Gibbons"
              #:location "Hongseok Yang, editor, European Symposium on Programming. Vol. 10201 of LNCS. Pages 568−583."
              #:date "2017"
              #:url "http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/aplicative.pdf"]
]

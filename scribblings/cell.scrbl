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
arrays. These functions implement restricted forms of recursion which are
guaranteed to terminate.

Recent work that looks particularly relevant is that by Slepak @italic{et al@._}
on `Remora' @cite["remora" "remora2" "remora3"] which describes a slightly more
general version of an abstract array language, as well as follow-on work by
Gibbons @cite["gibbons2017"].

However, compared to these languages, we have an additional challenge, which is
that the `values' of Cell are not only arrays. Instead, the value of a Cell
program contains also first-order functions and general expressions consisting
of these. Cell is more like a language for writing `programs'---specifically,
first-order, array-based programs. That means it needs to have values that can
represent expressions.

@section{Array-based Programming}

The following is a summary of the treatment of arrays in @cite["remora"].

@subsection{Arrays: rank, and shape}

Arrays are `rectangular arrangements of numbers'. Put differently, arrays are
vectors of vectors (of vectors...). But not all vectors of vectors are arrays,
only ones which are `rectangular,' in the sense that the `inner' vectors all
have the same length. This idea is captured in the follow definitions.

A @deftech{rank-0 array} is an ordinary, scalar value, called an @deftech{atom}
in the language of array-based programming. For definiteness, suppose that our
only atoms are real numbers.

A @deftech{rank-1 array} (sometimes called a vector) is a finite, ordered
sequence of @tech{atoms}. We write a @tech{rank-1 array} in square brackets; for
example: @math{[10 20 30]}.

A @deftech{rank-2 array} is a finite, ordered sequence of @tech{rank-1 arrays},
all of which have the same length. For example, @math{[[10 20 30] [100 200
300]]} is a rank-2 array, whereas @math{[[10 20] [100 200 300]]} is not an
array. The @deftech{shape} of a rank-2 array is a vector of length 2 whose
elements are the number of inner vectors and the length of an inner vector
(noting that all inner vectors have the same length). For example, the shape of
@math{[[10 20 30] [100 200 300]]} is @math{[2 3]}. (The shape of rank-1 array is
a vector of length 1 whose element is the length of the array.)

More generally a @deftech{rank-@math{n}} array is a vector of rank-@math{(n-1)}
arrays all having the same shape. The shape of a rank-@math{n} array is the
vector consisting of the length of the array followed by the elements of the
shape of the sub-arrays. The @deftech{rank} of an array is the length of its
shape. For example, a rank-3 array with shape @math{[5, 2, 3]} is `a vector of
length 5, whose elements are vectors of length 2 having elements that are
vectors of real of length 3.'


@subsection{Rank polymorphism: frames and cells}

Suppose one has a function on atoms, @math{f} : ℝ → ℝ. There is a natural way to
`lift' this function to operate on rank-1 arrays (of any length): apply @math{f}
to each element of the array. The result is a rank-1 array. (This is the
higher-order function, @racket[map], of course.) Indeed, one can naturally map
@math{f} over an array of @italic{any} rank (and any shape): apply @math{f} to
every atom, giving as a result an array of the same shape as the input.

The same trick applies to binary operators ℝ×ℝ → ℝ. Given two arrays of
identical shape, apply the operator, component-wise, to the individual atoms.

So there is a sort of built-in polymorphism to array-based programming. What if
we have a function which takes an @italic{array} as an argument? For example,
consider the function which sums the elements of a vector, @math{sum} : [*] →
ℝ. (The notation [*] means `an array of rank 1 but of any length.') Suppose we
apply @math{sum} to an array of rank 2? What meaning could be ascribed to
@math{sum A}, where @math{A} is the array [[1 2] [3 4]]?

It's reasonably intuitive that one should apply the @math{sum} to each element
of @math{A}; each element of @math{A} is a rank-1 array, so just the sort of
thing that @math{sum} wants to act on. The result is [3 7], a rank 1 array.

In order to capture this intution, Slepak @italic{et al@._} introduce a further
terminology. Suppose we have an array of shape @math{[s_1 s_2 s_3
... s_n]}. Consider a parition of the shape into two shapes: @math{[s_1
... s_i]} and @math{[s@subscript{i+1} ... s_n]}. The first shape is called (by
Slepak @italic{et al@._}) a @deftech{frame} and the second shape is called a
@deftech{cell}. (Thus, for example, the sum of the ranks of the frame and of the
cell is the rank of the original array, and there are @math{n+1} ways to
decompose an array into frame and cell.)

To apply a function requiring an argument of a particular rank, we decompose its
actual argument so that the cell of the decomposition is of the required
rank. @margin-note*{TODO: Note that the decomposition is by rank, not by
shape. That is, functions that operate on rank-1 arrays must operate on rank-1
arrays of any length, and so on.} Then we map the function over the frame,
applying it to each individual cell. The result has a frame that is the same as
the original frame, and a cell whose shape is determined by the result of the
function.

Remora's rank polymorphism is slightly more general when a function is applied
to multiple arguments.

First, it is required that the shape of every argument is a prefix of the the
argument of greatest rank (called the principle argument). Then, all the shorter
arguments are extended to the shape of the principle argument by duplicating
each atom into the remainder of the shape.








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

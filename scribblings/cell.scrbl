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

Other array-based languages include Lift, Futhark, and Accelerate. 

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
consider the function which sums the elements of a vector, @math{sum} : {1} →
ℝ. (The notation {1} means `an array of rank 1.' This is not the notation used
by Slepak @italic{et al@._}.) Suppose we apply @math{sum} to an array of rank 2?
What meaning could be ascribed to @math{sum A}, where @math{A} is the array [[1
2] [3 4]]?

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
arrays of any length, and so on.} Then we map the function over the
@italic{frame}, applying it to each individual cell. The result has a frame that
is the same as the original frame, and a cell whose shape is determined by the
result of the function. The same idea applies to functions of multiple
arguments, if the frames of the actual arguments are identical.

Remora's rank polymorphism on functions of more than one argument is actually
slightly more general than this.

First, each actual argument is decomposed as above into a frame and a cell,
according to the required shape of the formal argument. The frame with the
greatest rank is called the @italic{principal frame}. It is required that the
shape of the frame of every argument be a prefix of the shape of the principle
frame.

Each shorter frame is now extended to the shape of the principle frame by
replicating its cell as necessary. Then the polymorphism proceeds as before.

@subsection{Rank polymorphism in function position}

In Remora, the function position of an application may also be an array (an
array of functions). The process of extending the rank applies to this position
as well. In particular, this appears to be what happens in the process of
`automatic mapping'. Thus, for example, in the expression @racket[(+ [1 2] [10
20])] the function position has rank 0, so the expression becomes
@racket[([+ +] [1 2] [10 20])] which reduces to @racket[[11 22]].   

Note: I imagine that the rank of the function array is taken to be its entire
frame.

Note: I don't know what to call the rank of a function that `takes an array of
at least rank 1 and maps another function across its immediate sub-arrays.` It
seems here we are specifying the rank of the frames of its arguments.

@subsection{Higher order functions}

Remora provides several versions of reduce, scan, fold, and trace.

Reduce is like fold, but restricted to associative functions. Scan is the
`prefix partials' version of reduce; and trace is the `prefix partials' version
of fold.

@section{`Applicative, Naperian functors'}

Remora considers `vectors of length n' as its main `container type:' Everything
is vectors of vectors. In @cite["gibbons2017"] Gibbons considers what other kinds
of types might be suitable as dimensions. He concludes that a type which is
suitable for being a dimension must have a number of properties. It must be a
functor, in order to support mapping. It must support `zipping', or equivalently
`lifting functions inside the container,' and hence be an `applicative'
functor. In order to support the prefix sums operation it is necessary that the
container be `traversable.' (This probably gets you folds as well.)

The above allows things like pairs to be dimensions.

Finally, there is the question of `transposition' or `re-ranking.' Suppose one
has an array of rank 2 -- a `vector v of vectors w' (here v and w should be
thought of as shapes) -- and a function @tt{f} of rank 1. Normally the
application of @tt{f} to the rank 2 array would apply @tt{f} to each w, return a
vector v of results. Occasionally, one really wants to treat the array as a
`vector w of vectors v', and the application return a vector w fo results. This
is done by applying @tt{f} to the vector of first elements of all the ws, the
vector of second elements of the ws, and so on.

There's clearly some kind of isomorphism between a `vector of length n of
vectors of length m' and `vector of length m of vectors of length n.' The
question is then what kind of types support this isomorphism. Gibbons' answer is
a `Naperian functor,' which is, roughly, one that is equivalent to a map from
some index set.

That is, if @tt{D a} is a `container of things of type a', then @tt{D} is a
Naperian functor if there is some index type @tt{i} such that @tt{D a} is
isomorphic to @tt{i -> a}. For example, the type of `rank 1 arrays of length n'
is equivalent to the type of maps from the naturals less than n to scalars. The
naturals 0, 1, ..., n-1 index the elements of the vector.

Now we obtain transposition as the natural isomorphism between, say, @tt{i -> j
-> S} and @tt{j -> i -> S}.

All this is very encouraging since we had already started to consider vectors as
maps from such an index set.









@bibliography[
  @bib-entry[
             #:key "remora"
             #:title "An Array-Oriented Language with Static Rank Polymorphism"
             #:author "Justin Slepak, Olin Shivers, and Panagiotis Manolios"
             #:location "in Programming Languages and Systems (pp. 27--46)"
             #:date "2014"
             #:url "http://www.ccs.neu.edu/home/pete/pub/esop14-full.pdf"]
  @bib-entry[
             #:key "remora2"
             #:title "Records with Rank Polymorphism"
             #:author "Justin Slepak,Olin Shivers, and Panagiotis Manolios"
             #:location "in ARRAY 2019" #:date "2019"
             #:url "https://www.ccs.neu.edu/~jrslepak/array19.pdf"]
  @bib-entry[
             #:key "remora3"
             #:title "An Introduction to Rank-polymorphic Programming in Remora (Draft)"
             #:author "Olin Shivers, Justin Slepak, and Panagiotis Manolios"
             #:date "2020"
             #:location "arXiv:1912.13451v2 [cs.PL]"
             #:url "https://arxiv.org/abs/1912.13451"]
  @bib-entry[
             #:key "gibbons2017"
             #:title "APLicative Programming with Naperian Functors"
             #:author "Jeremy Gibbons"
             #:location "Hongseok Yang, editor, European Symposium on Programming. Vol. 10201 of LNCS. Pages 568−583."
             #:date "2017"
             #:url "http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/aplicative.pdf"]
             ]

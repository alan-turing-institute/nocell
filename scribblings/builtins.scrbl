#lang scribble/manual

@require[
  @for-label[@except-in[racket/base date? date date-month date-day date-year struct:date]]
  @for-label[nocell/grid/builtins]]

@title{Built-in Functions}

@defmodule[nocell/grid/builtins]{The @racketmodname[nocell/grid/builtins] library
defines all the primitive functions of Grid.}

The following forms are all re-exported by @racketmodname[nocell/grid/grid].

@defthing[builtins (hash/c symbol? builtin-type? #:immutable #t)]{

  A hash whose keys are the names of Grid built-in functions and whose values
  are their types, as defined above.
}

@defstruct*[builtin-type ([arity (or/c #f exact-nonnegative-integer?)]
                          [arg-types (or/c builtin-arg-type? (listof builtin-arg-type?))]
                          [ret-type  builtin-arg-type?])]{

  The `type' of a built-in function. If @racket[arity] is an integer, then
  @racket[arg-types] should be a list of @racket[builtin-arg-type?]. If
  @racket[arity] is @racket[#f] then @racket[arg-types] should be a single
  @racket[builtin-arg-type?] and the function takes any number of arguments
  of that type (but at least one).
}

@defproc[(builtin-arg-type? [v any/c]) boolean?]{

  Recognises one of the following symbols, which represent the types of
  arguments to, and return values from, Grid built-in functions:
  @racket['number?], @racket['string?], @racket['boolean?], @racket['date?],
  @racket['error?], @racket['matrix?], @racket['reference?], @racket['value?].
}

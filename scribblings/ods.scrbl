#lang scribble/manual

@title[#:tag "ods"]{Grid-to-Ods Backend}

@(require (@for-label "../grid/grid.rkt" "../ods/ods.rkt" sxml racket/system racket/file))

@defmodule["../ods/ods.rkt"]{The @racketmodname[ods/ods.rkt] library
provides utilities to create spreadsheet files from a @exec{grid} @racket[program].
Currently the library supports @exec{fods} (only opens in LibreOffice) and @exec{ods}
format (opens in both LibreOffice and Excel).
Conversion of sexp to xml is implemented by @racket[SXML].}

@defproc[(sxml->ods [sxml-program sxml-program?] ...
                    [#:type type (or/c "f" "fods" "flat" "e" "extended" "ods")])
                  bytes?]


Produces a bytes string (see also @racket[bytes?]) that corresponds to a single xml file if
 @racket[type] is one of "f", "fods" or "flat", and a zipped folder if
 @racket[type] is one of "e", "extended", or "ods".

 Serialization from sexp to xml is achieved using @racket[srl:sxml->xml].

 @exec{WARNING:} @racket[system] zip is used because @racketmodname[file/zip] does not support
 leaving the first file in the archive uncompressed.

@defproc[(grid-program->sxml [program program?])
                  sxml-program?]

Translates a grid @racket[program] into @racket[SXML].

@defproc[(bytes->file [bstr bytes?] [fn string?])
                  exact-nonnegative-integer?]

Like @racket[write-bytes], but also takes a filename.
 The provided filename should have an extension that matches the spreadsheet format of
 @racket[bstr] (".xml" for fods, ".ods" for ods). Currently there are no checks are made to ensure
 this is the case.
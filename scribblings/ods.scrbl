#lang scribble/manual

@title[#:tag "ods"]{Grid-to-Ods Backend}

@(require (@for-label nocell/sheet/ods/ods nocell/sheet/sheet sxml racket/system racket/file
            @except-in[racket/base date? date date-month date-day date-year struct:date]))

@defmodule[nocell/sheet/ods/ods]{The @racketmodname[nocell/sheet/ods/ods] library
provides utilities to create spreadsheet files from a @exec{sheet} @racket[spreadsheet].
Currently the library supports @bold{fods} (opens in LibreOffice) and @bold{ods}
format (opens in both LibreOffice and Excel).
Conversion of sexp to xml is implemented by @link["https://docs.racket-lang.org/sxml/index.html?q=sxml"]{sxml}.}

@defproc[(sxml->ods [sxml-spreadsheet sxml-spreadsheet?]
                    [#:type type (or/c 'fods 'ods)])
                  bytes?]

If @italic{type} is @racket['fods], produces a bytes string (see also
@racket[bytes?]) corresponding to the spreadsheet in the "flat" ODS
file format.  If @italic{type} is @racket['ods], produces an
"extended" ODS file (a zipped folder).


@margin-note{An ods file is a zipped folder with a .ods extension, containing with four files. 
  First, there is an uncompressed "mimetype" file. Then there are three compressed files: 
 "content.xml", "styles.xml", and "META-INF/manifest.xml". A fods file essentially contains the
 mimetype, content, and styles information wrapped into a single xml file.}

 Serialization from sexp to xml is achieved using @racket[srl:sxml->xml].

 @exec{WARNING:} @racket[system] zip is used because @racketmodname[file/zip] does not support
 leaving the first file in the archive uncompressed.

@defproc[(sheet-spreadsheet->sxml [spreadsheet spreadsheet?])
                  sxml-spreadsheet?]

Translates a @racketmodname[nocell/sheet/sheet] spreadsheet into @link["https://docs.racket-lang.org/sxml/SXML.html?q=sxml"]{SXML}.

@defproc[(bytes->file [bstr bytes?] [fn string?])
                  exact-nonnegative-integer?]

Like @racket[write-bytes], but also takes a filename.
 The provided filename should have an extension that matches the spreadsheet format of
 @italic{bstr} (".xml" for fods, ".ods" for ods). 

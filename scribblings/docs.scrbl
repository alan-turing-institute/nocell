#lang scribble/manual

@title{Nocell Documentation}
@author["James Geddes" "Oliver Strickson" "Callum Mole"]

Nocell is a language for building probabilistic models that output to spreadsheets.

You can see the source code at 
@(let ([url "https://github.com/alan-turing-institute/nocell"])
   (link url url))

@table-of-contents[]

@; ------------------------------------------------------------------------
@include-section["grid.scrbl"]
@include-section["ods.scrbl"]

@index-section[]
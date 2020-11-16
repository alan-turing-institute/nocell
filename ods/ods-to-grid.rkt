#lang racket
(require sxml)

(define NS_abbrev '((office . "urn:oasis:names:tc:opendocument:xmlns:office:1.0")
                    (table . "urn:oasis:names:tc:opendocument:xmlns:table:1.0")))

(define fn "flat_example.xml")
(call-with-input-file fn
    (lambda (in) (ssax:xml->sxml in NS_abbrev)))
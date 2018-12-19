#lang racket
(require "Racket/parser.rkt")

(define the-listener (tcp-listen 9876))
(define-values (in out) (tcp-accept the-listener))
(define inputThing (read in))
(displayln inputThing)
(parse-stmt inputThing)
(write (string-append "Hello" (~v inputThing)) out)
(flush-output out)
(tcp-close the-listener)
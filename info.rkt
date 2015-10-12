#lang info
(define collection "zmq")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/zmq.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(eric))

(define pre-install-collection "install.rkt")

#lang racket/base

(require make/setup-extension)

(provide pre-installer)

(define (pre-installer collections-top-path racket-zeromq-path)
  (pre-install racket-zeromq-path
               racket-zeromq-path
               "ext.c"
               "."
               null
               '("zmq")
               null
               null
               null
               null
               (λ (thunk) (thunk))
               #t))

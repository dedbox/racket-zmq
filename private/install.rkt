#lang racket/base

(require make/setup-extension)

(provide pre-installer)

(define (pre-installer collections-top-path racket-zeromq-path)
  (pre-install racket-zeromq-path
               (build-path racket-zeromq-path "private")
               "ext.c"
               "."
               null
               null
               null
               null
               null
               null
               (λ (thunk) (thunk))
               #t))

#lang racket/base

(provide zmq-version)

(require racket/format
         zmq/unsafe/version)

(define (zmq-version)
  (let ([major (box -1)]
        [minor (box -1)]
        [patch (box -1)])
    (zmq_version major minor patch)
    (~a (unbox major) "." (unbox minor) "." (unbox patch))))

(module+ test
  (require rackunit)
  (check-regexp-match "[0-9]+\\.[0-9]+\\.[0-9]+$" (zmq-version)))

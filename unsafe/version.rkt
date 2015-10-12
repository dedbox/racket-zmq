#lang racket/base

(provide zmq_version)

(require ffi/unsafe
         zmq/unsafe/define)

(define-zmq zmq_version
  (_fun (_box _fixint) (_box _fixint) (_box _fixint) -> _void))

(module+ test
  (require rackunit)
  (let ([major (box -1)]
        [minor (box -1)]
        [patch (box -1)])
    (check-equal? (zmq_version major minor patch) (void))
    (check-true (> (unbox major) 0))
    (check-true (> (unbox minor) 0))
    (check-true (> (unbox patch) 0))
    ))

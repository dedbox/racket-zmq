#lang racket/base

(provide define-zmq define-zmq-alias define-zmq-check _efun croak)

(require ffi/unsafe
         ffi/unsafe/define
         zmq/private/ext)

;; ---------------------------------------------------------------------------
;; library loader

(define zmq-lib (ffi-lib "libzmq"))

(define-ffi-definer define-zmq zmq-lib)

(define-syntax-rule (define-zmq-alias (alias actual) def)
  (define alias (get-ffi-obj 'actual zmq-lib def)))

(define-syntax-rule (define-zmq-check name args ...)
  (define-zmq name
    (_efun args ...
           -> (rc : _fixint)
           -> (if (= rc -1) (croak 'name) rc))))

;; ---------------------------------------------------------------------------
;; error handling

(define-syntax-rule (_efun args ...)
  (_fun #:save-errno 'posix args ...))

(define-zmq zmq_strerror (_fun _fixint -> _string))

(define (croak caller)
  (if (= (saved-errno) EAGAIN)
      (raise 'AGAIN)
      (error caller (zmq_strerror (saved-errno)))))

;; ---------------------------------------------------------------------------
;; common data types

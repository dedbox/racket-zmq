#lang racket/base

(provide zmq_setsockopt)

(require ffi/unsafe
         zmq/unsafe/ctypes
         zmq/unsafe/context
         zmq/unsafe/define
         zmq/unsafe/socket
         [for-syntax racket/base
                     racket/syntax])

(define-syntax (define-setsockopt->ctype stx)
  (syntax-case stx ()
    [(_ type)
     (with-syntax ([alias (format-id #'type "setsockopt->~a" #'type)]
                   [_type (format-id #'type "_~a" #'type)])
       #'(define-zmq-alias [alias zmq_setsockopt]
           (_efun _socket _set_socket_option
                  (val : (_ptr i _type))
                  (_size = (ctype-sizeof _type))
                  -> (rc : _fixint)
                  -> (if (= rc -1) (croak 'zmq_setsockopt) rc))))]))

(define-setsockopt->ctype uint64)
(define-setsockopt->ctype fixint)

(define-zmq-alias [setsockopt->char* zmq_setsockopt]
  (_efun _socket _set_socket_option
         (val : _bytes)
         (_size = (bytes-length val))
         -> (rc : _fixint)
         -> (if (= rc -1) (croak 'zmq_setsockopt) rc)))

(define (zmq_setsockopt obj name val)
  (case name
    [(AFFINITY   ) (setsockopt->uint64 obj name val)]
    [(SUBSCRIBE  ) (setsockopt->char* obj name (string->bytes/utf-8 val))]
    [(UNSUBSCRIBE) (setsockopt->char* obj name (string->bytes/utf-8 val))]
    [(LINGER     ) (setsockopt->fixint obj name val)]
    [(BACKLOG    ) (setsockopt->fixint obj name val)]
    [(RCVHWM     ) (setsockopt->fixint obj name val)]
    ))

(module+ test
  (require rackunit
           zmq/unsafe/getsockopt)

  (define-syntax-rule (check-setsockopt obj name binop val)
    (begin
      (check = (zmq_setsockopt obj name val) 0)
      (check binop (zmq_getsockopt obj name) val)))

  (let* ([C (zmq_ctx_new)]
         [S (zmq_socket C 'REP)])
    (check-setsockopt S 'AFFINITY = 3)
    (check-setsockopt S 'LINGER   = 30)
    (check-setsockopt S 'RCVHWM   = 500)
    ))

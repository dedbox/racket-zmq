#lang racket/base

(provide zmq_ctx_new zmq_ctx_get zmq_ctx_set zmq_ctx_shutdown zmq_ctx_term)

(require ffi/unsafe
         zmq/unsafe/ctypes
         zmq/unsafe/define)

(define-zmq zmq_ctx_new
  (_efun -> (obj : _context)
         -> (or obj (croak 'zmq_ctx_new))))

(define-zmq-check zmq_ctx_get _context _ctx_get_option)
(define-zmq-check zmq_ctx_set _context _ctx_set_option _fixint)
(define-zmq-check zmq_ctx_shutdown _context)
(define-zmq-check zmq_ctx_term _context)

(module+ test
  (require rackunit)

  (let ([C (zmq_ctx_new)])
    (check = (zmq_ctx_get C 'IO_THREADS) 1)
    (check = (zmq_ctx_get C 'MAX_SOCKETS) 1023)
    (check = (zmq_ctx_get C 'SOCKET_LIMIT) 65535)
    (check = (zmq_ctx_get C 'IPV6) 0)
    (check = (zmq_ctx_set C 'IO_THREADS 3) 0)
    (check = (zmq_ctx_set C 'MAX_SOCKETS 511) 0)
    (check = (zmq_ctx_set C 'THREAD_PRIORITY 4095) 0)
    (check = (zmq_ctx_set C 'THREAD_SCHED_POLICY 98) 0)
    (check = (zmq_ctx_set C 'IPV6 1) 0)
    (check = (zmq_ctx_get C 'IO_THREADS) 3)
    (check = (zmq_ctx_get C 'MAX_SOCKETS) 511)
    (check = (zmq_ctx_get C 'SOCKET_LIMIT) 65535)
    (check = (zmq_ctx_get C 'IPV6) 1)
    (check = (zmq_ctx_shutdown C) 0)
    (check = (zmq_ctx_term C) 0)))

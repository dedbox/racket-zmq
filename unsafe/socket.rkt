#lang racket/base

(provide zmq_socket zmq_close zmq_bind zmq_connect
         zmq_send zmq_recv zmq_send_const)

(require ffi/unsafe
         zmq/unsafe/ctypes
         zmq/unsafe/define)

(define-zmq zmq_socket
  (_efun _context _socket_type
         -> (obj : _socket)
         -> (or obj (croak 'zmq_socket))))

(define-zmq-check zmq_close _socket)
(define-zmq-check zmq_bind _socket _bytes)
(define-zmq-check zmq_connect _socket _bytes)
(define-zmq-check zmq_send _socket _bytes _size _send_flags)
(define-zmq-check zmq_recv _socket _bytes _size _recv_flags)
(define-zmq-check zmq_send_const _socket _bytes _size _send_flags)

(module+ test
  (require rackunit
           zmq/unsafe/context)

  (let* ([C (zmq_ctx_new)]
         [P (zmq_socket C 'REP)]
         [Q (zmq_socket C 'REQ)]
         [buf (make-bytes 10)])
    (check = (zmq_bind P #"inproc://test1") 0)
    (check = (zmq_connect Q #"inproc://test1") 0)
    (check = (zmq_send Q #"abc123" 6 null) 6)
    (check = (zmq_recv P buf 10 null) 6)
    (check-equal? buf #"abc123\0\0\0\0")
    (check = (zmq_send_const P #"ok" 2 null) 2)
    (check = (zmq_recv Q buf 10 null) 2)
    (check-equal? buf #"okc123\0\0\0\0")
    (check = (zmq_close Q) 0)
    (check = (zmq_close P) 0)
    (check = (zmq_ctx_shutdown C) 0)
    (check = (zmq_ctx_term C) 0)
    ))

#lang racket/base

(provide pollitems pollitems-revents zmq_poll)

(require ffi/unsafe
         ffi/unsafe/cvector
         zmq/ext
         zmq/unsafe/ctypes
         zmq/unsafe/define)

(define-syntax-rule (pollitems [sock flags] ...)
  (cvector _pollitem (make-pollitem sock 0 flags null) ...))

(define (pollitems-revents items k)
  (pollitem-revents (cvector-ref items k)))

(define (zmq_poll items nitems timeout)
  (zmq_poll* (cvector-ptr items) nitems timeout))

(module+ test
  (require rackunit
           zmq/unsafe/context
           zmq/unsafe/socket)

  (define (poll-server C)
    (let ([P (zmq_socket C 'REP)])
      (thread (λ ()
                (check = (zmq_bind P #"inproc://poll-test") 0)
                (let ([items (pollitems [P 'POLLIN])]
                      [buf (make-bytes 10)])
                  (bytes-fill! buf 0)
                  (check = (zmq_poll items 1 -1) 1)
                  (check = (zmq_recv P buf 10 null) 5)
                  (check-equal? buf #"PING!\0\0\0\0\0"))))))

  (define (poll-client C)
    (let ([Q (zmq_socket C 'REQ)])
      (thread (λ ()
                (check = (zmq_connect Q #"inproc://poll-test") 0)
                (sleep 0.1)
                (check = (zmq_send_const Q #"PING!" 5 null) 5)))))

  (let ([C (zmq_ctx_new)])
    (let ([server (poll-server C)]
          [client (poll-client C)])
      (thread-wait server)
      (thread-wait client)))

  (let ([C (zmq_ctx_new)])
    (let ([server (poll-server C)]
          [client (poll-client C)])
      (thread-wait client)
      (thread-wait server)))

  (let ([C (zmq_ctx_new)])
    (let ([client (poll-client C)]
          [server (poll-server C)])
      (thread-wait client)
      (thread-wait server)))

  (let ([C (zmq_ctx_new)])
    (let ([client (poll-client C)]
          [server (poll-server C)])
      (thread-wait server)
      (thread-wait client))))

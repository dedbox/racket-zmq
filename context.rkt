#lang racket/base

(provide make-context context-get context-set! context-shutdown context-term
         current-context with-context with-new-context)

(require ffi/unsafe
         zmq/unsafe/context)

(struct context (ptr) #:property prop:cpointer 0)

(define (make-context)
  (context (zmq_ctx_new)))

(define (context-get name [ctx (current-context)])
  (zmq_ctx_get ctx name))

(define (context-set! name value [ctx (current-context)])
  (void (zmq_ctx_set ctx name value)))

(define (context-shutdown [ctx (current-context)])
  (void (zmq_ctx_shutdown ctx)))

(define (context-term [ctx (current-context)])
  (void (zmq_ctx_term ctx)))

;; syntax

(define current-context (make-parameter (make-context)))

(define-syntax-rule (with-context ctx body ...)
  (parameterize ([current-context ctx]) body ...))

(define-syntax-rule (with-new-context body ...)
  (with-context (make-context)
    (begin0 (begin body ...) (context-shutdown) (context-term))))

(module+ test
  (require rackunit)
  (with-new-context
    (check = (context-get 'IO_THREADS) 1)
    (check = (context-get 'MAX_SOCKETS) 1023)
    (check = (context-get 'SOCKET_LIMIT) 65535)
    (check = (context-get 'IPV6) 0)
    (check-equal? (context-set! 'IO_THREADS 3) (void))
    (check-equal? (context-set! 'MAX_SOCKETS 511) (void))
    (check-equal? (context-set! 'THREAD_PRIORITY 4095) (void))
    (check-equal? (context-set! 'THREAD_SCHED_POLICY 98) (void))
    (check-equal? (context-set! 'IPV6 1) (void))
    (check = (context-get 'IO_THREADS) 3)
    (check = (context-get 'MAX_SOCKETS) 511)
    (check = (context-get 'SOCKET_LIMIT) 65535)
    (check = (context-get 'IPV6) 1)))

#lang racket/base

(require racket/format
         zmq/unsafe)

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------
;; version

(define (zmq-version)
  (define-values (major minor patch) (zmq_version))
  (~a major "." minor "." patch))

(provide zmq-version)

(module+ test
  (check-regexp-match "[0-9]+\\.[0-9]+\\.[0-9]+$" (zmq-version)))

;; ---------------------------------------------------------------------------
;; context

(define zmq-ctx-new zmq_ctx_new)
(define zmq-ctx-get zmq_ctx_get)
(define zmq-ctx-set zmq_ctx_set)
(define zmq-ctx-shutdown zmq_ctx_shutdown)
(define zmq-ctx-term zmq_ctx_term)

(provide zmq-ctx-new zmq-ctx-get zmq-ctx-set zmq-ctx-shutdown zmq-ctx-term)

(module+ test
  (let ([C (zmq-ctx-new)])
    (check = (zmq-ctx-get C 'IO_THREADS) 1)
    (check = (zmq-ctx-get C 'MAX_SOCKETS) 1023)
    (check = (zmq-ctx-get C 'SOCKET_LIMIT) 65535)

    (check-equal? (zmq-ctx-set C 'IO_THREADS 3) (void))
    (check-equal? (zmq-ctx-set C 'MAX_SOCKETS 511) (void))
    (check-equal? (zmq-ctx-set C 'THREAD_PRIORITY 4095) (void))
    (check-equal? (zmq-ctx-set C 'THREAD_SCHED_POLICY 98) (void))
    (check-equal? (zmq-ctx-set C 'IPV6 1) (void))

    (check = (zmq-ctx-get C 'IO_THREADS) 3)
    (check = (zmq-ctx-get C 'MAX_SOCKETS) 511)
    (check = (zmq-ctx-get C 'SOCKET_LIMIT) 65535)
    (check = (zmq-ctx-get C 'IPV6) 1)

    (check-equal? (zmq-ctx-shutdown C) (void))
    (check-equal? (zmq-ctx-term C) (void))))
